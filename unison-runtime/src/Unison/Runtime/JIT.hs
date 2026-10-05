-- | The JIT's entry points for the rest of the runtime. See docs/jit/design.md.
module Unison.Runtime.JIT
  ( startJIT,
    jitCompileGroup,
    jitRequestGroup,
    jitRequestCell,
    registerDataTypes,
    printJITStats,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TBQueue, atomically, isFullTBQueue, newTBQueueIO, readTBQueue, readTVarIO, writeTBQueue)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Data.IORef
import Data.Set qualified as Set
import Foreign.Ptr (Ptr, nullPtr)
import System.Timeout (timeout)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Map.Strict qualified as Map
import Data.List (sortOn)
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Reference (Reference)
import Unison.Runtime.JIT.Codegen (CtxOffsets (..), RtsFacts (..))
import Unison.Runtime.JIT.Compile
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Estimate (Estimate (..), judge)
import Unison.Runtime.JIT.Exits
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (probeArrays, probeBytes, probeLayouts, probeLists, probeMurmur, probeNames, probeTexts)
import Unison.Runtime.JIT.Native (configureNative, ctxLayout, rtsFacts)
import Unison.Runtime.MCode (CombIx (..), GComb (..), GCombInfo (..), NativeCell, claimNativeCell, combDeps, nativeCellRequested, noNativeCell, prettyIns, prettySection, readNativeCode, readNativeCount, releaseNativeCell, takeNativeCell, writeNativeCount)
import Unison.Runtime.Machine.Types (CCache (combRefs, combs, sandboxed), MCombs)
import Unison.Util.EnumContainers qualified as EC

-- | The constructor arities of every data type loaded so far, which the
-- code generator needs to take apart constructors with three or more
-- fields (their field count isn't in the closure's pointer tag).
dataTypes :: IORef (Map.Map Reference [Int])
dataTypes = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE dataTypes #-}

-- | Records the constructor arities of some data types. Called by the
-- interface whenever it learns about types, before their code is loaded.
registerDataTypes :: Map.Map Reference [Int] -> IO ()
registerDataTypes m = case mode config of
  Off -> pure ()
  _ -> atomicModifyIORef' dataTypes (\old -> (Map.union m old, ()))

-- | Set once by 'startJIT' when everything checks out. Nothing means off.
jitState :: IORef (Maybe JITState)
jitState = unsafePerformIO (newIORef Nothing)
{-# NOINLINE jitState #-}

-- | Gets the JIT going if it is enabled. Called once when the runtime
-- starts. In eager mode this starts LLVM; in @on@ mode it only starts the
-- compile thread, which starts LLVM when the first request arrives, so
-- that a program that never gets hot pays nothing.
startJIT :: IO ()
startJIT = do
  when (statsEach config) (writeIORef outputHook printExitsSinceOutput)
  startCompiler

startCompiler :: IO ()
startCompiler = case mode config of
  Off -> pure ()
  Eager ->
    readIORef jitState >>= \case
      Just _ -> pure () -- a second runtime in the same process
      Nothing -> initJIT
  On -> do
    first <- atomicModifyIORef' compileThreadStarted (\started -> (True, not started))
    when first (void (forkIO compileThread))

-- | Starts LLVM and probes the runtime. On success 'jitState' is set; if
-- anything is wrong the JIT stays off, with a message.
initJIT :: IO ()
initJIT = do
  let timed what io = do
        t0 <- getMonotonicTimeNSec
        x <- io
        t1 <- getMonotonicTimeNSec
        x <$ jitLog (what ++ ": " ++ show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms")
  r <- initLLVM
  case r of
    Left e -> jitLog (e ++ "; the JIT is off")
    Right () -> do
      layouts <- probeLayouts >>= \case
        Left e -> pure (Left e)
        -- the list helpers are part of the generated code's contract too
        Right ls ->
          timed "list helper checks" (probeLists ls (stressLists config)) >>= \case
            Left e -> pure (Left e)
            Right () ->
              -- bytes before texts: the text checks build bytes too (toUtf8)
              timed "bytes helper checks" (probeBytes ls (stressBytes config)) >>= \case
                Left e -> pure (Left e)
                Right () ->
                  timed "text helper checks" (probeTexts ls (stressTexts config)) >>= \case
                    Left e -> pure (Left e)
                    Right () ->
                      probeArrays ls >>= \case
                        Left e -> pure (Left e)
                        Right () ->
                          probeMurmur ls >>= \case
                            Left e -> pure (Left e)
                            Right () -> fmap (const ls) <$> probeNames ls
      (offs, _) <- ctxLayout
      case (layouts, offs) of
        (Left e, _) -> jitLog (e ++ "; the JIT is off")
        (Right ls, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]) -> do
          configureNative (stressPoll config) (stressCallee config) (stressCStack config) (stressAlloc config) (trace config)
          facts <- rtsFacts
          case facts of
            -- the allocator's address isn't needed: generated code calls
            -- unison_jit_alloc_words by name, and the JIT resolves it from the process
            [arrWordsInfo, arrPtrsInfo, bytesHdr, bytesCount, ptrsHdr, ptrsCount, ptrsSize, cardBits, _allocator, mutVarVar, _barrier, arrPtrsDirtyInfo] -> do
              let rts = RtsFacts arrWordsInfo arrPtrsInfo bytesHdr bytesCount ptrsHdr ptrsCount ptrsSize cardBits mutVarVar arrPtrsDirtyInfo
              writeIORef jitState (Just (JITState ls (CtxOffsets a b c d e f g h i j k l m n o p q r s t u) rts))
              triple <- targetTriple
              jitLog ("mode " ++ show (mode config) ++ ", LLVM ready, target " ++ triple)
            _ -> jitLog "unexpected runtime facts; the JIT is off"
        _ -> jitLog "unexpected Ctx layout; the JIT is off"

-- | Compiles a freshly loaded top-level definition, in eager mode.
jitCompileGroup :: Bool -> Reference -> Word64 -> MCombs -> IO ()
jitCompileGroup sandbox ref grp cmbs = case mode config of
  Eager ->
    readIORef jitState >>= \case
      Nothing -> pure ()
      Just st -> do
        types <- readIORef dataTypes
        (now, _) <- groupUnits sandbox False ref grp cmbs
        compileUnits st types ("unison_" ++ show grp) False now
  _ -> pure ()

-- ---------------------------------------------------------------------------
-- The @on@ mode: compiling what gets hot, on a thread of its own.
-- See docs/jit/m5.md.

-- | What the interpreter asks the compile thread for.
data Request
  = -- | compile this definition: its group number, and how to read the
    -- code cache (every group's combinators, and their references)
    ReqGroup !Word64 !Bool (IO (EC.EnumMap Word64 MCombs, EC.EnumMap Word64 Reference))
  | -- | generate a re-entry function that was left for later
    ReqUnit Unit

-- | Bounded, so a burst of requests costs nothing but the requests: one
-- that doesn't fit is dropped, and made again if the code stays hot.
requests :: TBQueue Request
requests = unsafePerformIO (newTBQueueIO 256)
{-# NOINLINE requests #-}

compileThreadStarted :: IORef Bool
compileThreadStarted = unsafePerformIO (newIORef False)
{-# NOINLINE compileThreadStarted #-}

-- | Queues a request if there is room; says whether there was.
offer :: Request -> IO Bool
offer r = atomically $ do
  full <- isFullTBQueue requests
  unless full (writeTBQueue requests r)
  pure (not full)

-- | The cell of a definition's entry combinator. Its state stands for the
-- whole definition: requested from the moment a request for the
-- definition is queued, taken once the compile thread has it in a batch.
-- It never goes back, so a definition is queued at most once.
entryCell :: MCombs -> Maybe (Ptr NativeCell)
entryCell cmbs = case EC.lookup 0 cmbs of
  Just (Comb (LamI _ _ _ cell)) | cell /= noNativeCell -> Just cell
  _ -> Nothing

-- | Asks for the definition this combinator belongs to to be compiled.
-- Called by the interpreter when the combinator's call count says it is
-- hot (see 'bumpNativeCount'). If the queue is full the flag is cleared
-- again and the count set to ask again after another 1024 calls.
jitRequestGroup :: CCache p -> CombIx -> Ptr NativeCell -> IO ()
jitRequestGroup cc (CIx _ grp _) cell
  | cell == noNativeCell = pure () -- a builtin, or code not loaded through the cache
  | otherwise = do
      cache <- readTVarIO (combs cc)
      forM_ (EC.lookup grp cache >>= entryCell) $ \entry -> do
        mine <- claimNativeCell entry
        when mine $ do
          queued <- offer (ReqGroup grp (sandboxed cc) ((,) <$> readTVarIO (combs cc) <*> readTVarIO (combRefs cc)))
          unless queued $ do
            releaseNativeCell entry
            writeNativeCount cell (-1024)
{-# NOINLINE jitRequestGroup #-}

-- | Asks for the re-entry function that belongs in this cell to be
-- generated, if there is one waiting (see 'Compile.pending'). Called by
-- the interpreter when it has found the cell empty often enough. The
-- cell's request state keeps it from being queued twice.
jitRequestCell :: Ptr NativeCell -> IO ()
jitRequestCell cell =
  lookupPending cell >>= \case
    Nothing -> pure ()
    Just u -> do
      mine <- claimNativeCell cell
      when mine $ do
        queued <- offer (ReqUnit u)
        -- addPending starts the count again
        unless queued (releaseNativeCell cell >> addPending u)
{-# NOINLINE jitRequestCell #-}

-- | Which definitions call which: for each group, the groups whose code
-- refers to it. The code cache only records the other direction, so the
-- compile thread keeps this, adding the groups loaded since it last looked.
data Callers = Callers !(Set.Set Word64) !(Map.Map Word64 (Set.Set Word64))

indexCallers :: EC.EnumMap Word64 MCombs -> Callers -> Callers
indexCallers cache (Callers seen callers) =
  Callers
    (Set.union seen (Set.fromList (map fst new)))
    (Map.unionWith Set.union callers (Map.fromListWith Set.union [(d, Set.singleton g) | (g, cmbs) <- new, d <- groupDeps g cmbs]))
  where
    new = [gc | gc@(g, _) <- EC.mapToList cache, not (Set.member g seen)]

-- | The other groups a group's code refers to.
groupDeps :: Word64 -> MCombs -> [Word64]
groupDeps g cmbs = Set.toList (Set.delete g (Set.fromList [d | (_, c) <- EC.mapToList cmbs, d <- combDeps c]))

-- | Serves requests, forever. LLVM is only ever used from this thread (in
-- @on@ mode), and is started by the first request.
--
-- A definition that got hot is compiled as soon as its request comes up.
-- Requests for re-entry functions are held instead, and compiled together
-- as one module once there are a batch of them or no request has come for
-- 'reentryWait' milliseconds: each would otherwise be a module of its own,
-- paying LLVM's per-module cost for a few dozen instructions, and nothing
-- is lost by the wait, since a re-entry point that doesn't exist yet is
-- resumed by the interpreter as it was before the counter said to generate
-- it. A definition's request is served ahead of the held re-entry
-- functions, so the wait never delays a hot loop.
compileThread :: IO ()
compileThread = do
  initialized <- newIORef False
  callersRef <- newIORef (Callers Set.empty Map.empty)
  let loop held = do
        next <-
          if null held
            then Just <$> atomically (readTBQueue requests)
            else timeout (reentryWait config * 1000) (atomically (readTBQueue requests))
        case next of
          Nothing -> flush held >> loop []
          Just (ReqUnit u) | reentryWait config > 0 -> do
            let held' = u : held
            if length held' >= batch config then flush held' >> loop [] else loop held'
          Just req -> do
            r <- try (serve initialized callersRef req)
            case r of
              Left (e :: SomeException) -> jitLog ("compile thread: " ++ show e)
              Right () -> pure ()
            loop held
      flush [] = pure ()
      flush held = do
        r <- try (withState initialized $ \st types -> do
          let name = "unison_" ++ unitName (last held) ++ "_x" ++ show (length held)
          guarded name (compileUnits st types name True (reverse held))
          forM_ held dropPending)
        case r of
          Left (e :: SomeException) -> jitLog ("compile thread: " ++ show e)
          Right () -> pure ()
  loop []
  where
    withState initialized k = do
      ready <- readIORef initialized
      unless ready (initJIT >> writeIORef initialized True)
      readIORef jitState >>= \case
        Nothing -> pure ()
        Just st -> do
          types <- readIORef dataTypes
          k st types
    serve initialized callersRef req =
      withState initialized $ \st types -> do
          case req of
            ReqGroup grp sandbox find -> do
              (cache, refs) <- find
              -- a request whose definition went into an earlier batch is dropped
              mine <- maybe (pure False) takeNativeCell (EC.lookup grp cache >>= entryCell)
              when mine $ do
                modifyIORef' callersRef (indexCallers cache)
                Callers _ callers <- readIORef callersRef
                members <- formBatch cache callers grp
                (now, later) <-
                  unzip
                    <$> sequence
                      [ groupUnits sandbox True ref g cmbs
                        | g <- members,
                          Just cmbs <- [EC.lookup g cache],
                          Just ref <- [EC.lookup g refs]
                      ]
                -- pending before the groups' code can exit to them
                forM_ (concat later) addPending
                copies <- copyCandidates sandbox cache refs members
                guarded ("unison_" ++ show grp) (compileUnits st types ("unison_" ++ show grp) True (concat now ++ copies))
            ReqUnit u -> do
              guarded (unitName u) (compileUnits st types ("unison_" ++ unitName u) True [u])
              dropPending u
    -- The definitions to compile together with one that got hot, so that
    -- calls between them are direct and LLVM can inline across them:
    -- breadth first over what it calls and what calls it, up to the batch
    -- size. A callee usually gets hot before its callers do (it is called
    -- at least as often), so the callers are what a batch mostly gains. A
    -- definition is taken if it isn't in a batch already and it is in use:
    -- its own request is waiting in the queue (it got hot while this one
    -- waited; the request is dropped when it comes up), or, for a callee,
    -- it has been called at least half the threshold, for a caller, once.
    formBatch cache callers grp = go [grp] (Set.singleton grp) [grp]
      where
        full taken = length taken >= batch config
        go taken _ [] = pure (reverse taken)
        go taken seen (g : queue)
          | full taken = pure (reverse taken)
          | otherwise = do
              let callees = maybe [] (groupDeps g) (EC.lookup g cache)
                  around = map ((,) (max 1 (threshold config `div` 2))) callees ++ map ((,) 1) (Set.toList (Map.findWithDefault Set.empty g callers))
              (taken', seen', new) <- foldM consider (taken, seen, []) around
              go taken' seen' (queue ++ reverse new)
        consider acc@(taken, seen, new) (enough, d)
          | Set.member d seen || full taken = pure acc
          | otherwise = do
              claimed <- case EC.lookup d cache >>= entryCell of
                Nothing -> pure False
                Just cell -> do
                  -- counts start at minus the threshold
                  n <- readNativeCount cell
                  hot <- nativeCellRequested cell
                  if hot || n + threshold config >= enough then takeNativeCell cell else pure False
              pure (if claimed then (d : taken, Set.insert d seen, d : new) else (taken, Set.insert d seen, new))
    -- The compiled callees of the batch that are worth a private copy in
    -- its module (see 'copyUnits'): those where a call is a real share of
    -- the work per call, by the estimate's path saving, and that don't
    -- loop or recurse (a copy of those would stay as a second body and
    -- gain only the outer call).
    copyCandidates sandbox cache refs members
      | copyBound config <= 0 || "copy" `elem` disabled config = pure []
      | otherwise = do
          let inBatch = Set.fromList members
              callees = Set.toList (Set.fromList [d | g <- members, Just cmbs <- [EC.lookup g cache], d <- groupDeps g cmbs, not (Set.member d inBatch)])
          picked <- forM callees $ \d -> case (EC.lookup d cache, EC.lookup d refs) of
            (Just cmbs, Just ref)
              | Just cell <- entryCell cmbs,
                Just (Comb (LamI _ _ entry _)) <- EC.lookup 0 cmbs -> do
                  code <- readNativeCode cell
                  if code == nullPtr
                    then pure []
                    else do
                      (_, est) <- judge cell entry
                      case est of
                        Just (Estimate saving _ False) | saving <= copyBound config -> copyUnits sandbox ref d cmbs
                        _ -> pure []
            _ -> pure []
          let units = take (batch config) (concat picked)
          unless (null units) (jitLog ("private copies: " ++ unwords (map unitName units)))
          pure units
    guarded what act =
      try (act >>= evaluate) >>= \case
        Left (e :: SomeException) -> jitLog (what ++ ": " ++ show e)
        Right () -> pure ()

-- | Prints how often each exit was taken, when UNISON_JIT_STATS is set.
-- Called after each evaluation.
printJITStats :: IO ()
printJITStats = when (stats config) $ do
  t <- compileTotals
  jitDump
    ( "[jit] compiled " ++ show (ctModules t) ++ " modules: " ++ show (ctFunctions t) ++ " functions, "
        ++ show (ctAuxiliary t) ++ " auxiliary functions with them, " ++ show (ctOnDemand t)
        ++ " re-entry functions on demand (" ++ show (ctPending t) ++ " never asked for); "
        ++ show (ctNotWorthIt t) ++ " left interpreted by the exit rule; "
        ++ show (ctIRBytes t `div` 1024) ++ " KB of IR, " ++ show (fromIntegral (ctNanoseconds t) / 1e6 :: Double) ++ " ms"
    )
  printExits maxBound
  when (statsEach config) resetExitCounts

-- | With @UNISON_JIT_STATS=each@: the exits taken since the program last
-- wrote output (the busiest sites only), after which counting starts again.
printExitsSinceOutput :: IO ()
printExitsSinceOutput = do
  printExits 12
  resetExitCounts

printExits :: Int -> IO ()
printExits most = do
  entered <- entryCounts
  let enteredTotal = sum (map snd entered)
  when (enteredTotal > 0) $ do
    jitDump ("[jit] " ++ show enteredTotal ++ " entries into native code from the trampoline (" ++ show (length entered) ++ " functions):")
    forM_ (take most (sortOn (negate . snd) entered)) $ \(name, n) ->
      jitDump ("  " ++ show n ++ "\t" ++ name)
  counts <- exitCounts
  let total = sum [n | (_, _, n) <- counts]
  when (total > 0) $ do
    jitDump ("[jit] " ++ show total ++ " exits taken (" ++ show (length counts) ++ " sites):")
    forM_ (take most (sortOn (\(_, _, n) -> negate n) counts)) $ \(i, e, n) ->
      jitDump ("  " ++ show n ++ "\t#" ++ show i ++ "\t" ++ describe e)
  where
    describe = \case
      Resume cix sect -> "resume " ++ show cix ++ " at " ++ takeWhile (/= '\n') (dropWhile (== ' ') (prettySection 0 sect ""))
      GrowStack n _ -> "grow stack by " ++ show n
      Reenter _ -> "reenter (poll)"
      CallOut cix instr _ _ _ -> "call out " ++ show cix ++ " for " ++ takeWhile (/= '\n') (prettyIns instr "")
      Named name e -> name ++ ": " ++ describe e
