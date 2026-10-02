{-# LANGUAGE LambdaCase #-}

-- | Compiling a group of combinators to a native module and installing
-- the result in their cells. See docs/jit-m1.md.
module Unison.Runtime.JIT.Compile
  ( JITState (..),
    Unit,
    unitName,
    groupUnits,
    compileUnits,
    addPending,
    lookupPending,
    dropPending,
    CompileTotals (..),
    compileTotals,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef
import Data.Primitive.PrimArray (sizeofPrimArray)
import Data.Maybe (isJust)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, castFunPtrToPtr)
import Data.Map qualified as Map
import GHC.Clock (getMonotonicTimeNSec)
import System.FilePath ((</>))
import Unison.Runtime.JIT.Codegen qualified as CG
import Unison.Runtime.JIT.Codegen (AuxMemo, CtxOffsets, Deferred (..), Function (..), RtsFacts, genDeferred, genFunction, modulePrelude)
import Unison.Runtime.JIT.Config
import Unison.Runtime.JIT.Estimate (Estimate (..), judge)
import Unison.Runtime.JIT.Exits (nameEntry, registerExits, replaceExits)
import Unison.Runtime.JIT.Frames (registerFrames, replaceFrames)
import Unison.Runtime.JIT.LLVM
import Unison.Runtime.JIT.Layout (Layouts)
import Unison.Runtime.JIT.Pool (PoolKey (..), poolIndices)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Reference (Reference)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.TypeTags qualified as TT
import Unison.Builtin.Decls qualified as Ty (optionalRef, seqViewRef, unitRef)
import Unison.Runtime.MCode
import Unison.Runtime.Machine.Types (MCombs, MSection)
import Unison.Runtime.Stack (Val (..))
import Unison.Util.EnumContainers qualified as EC
import Data.Bits ((.&.))
import Data.Set qualified as Set


-- | Everything in a section tree, innermost sections included, in order.
sectionsOf :: MSection -> [MSection]
sectionsOf s = s : rest
  where
    rest = case s of
      Let b _ _ bd _ -> sectionsOf b ++ sectionsOf bd
      Ins _ nx -> sectionsOf nx
      Match _ bs -> goB bs
      DMatch _ _ bs -> goB bs
      NMatch _ _ bs -> goB bs
      RMatch _ p bs -> sectionsOf p ++ concatMap goB (map snd (EC.mapToList bs))
      _ -> []
    goB = \case
      Test1 _ a d -> sectionsOf a ++ sectionsOf d
      Test2 _ a _ b d -> sectionsOf a ++ sectionsOf b ++ sectionsOf d
      TestW d m -> sectionsOf d ++ concatMap sectionsOf (map snd (EC.mapToList m))
      TestT d m -> sectionsOf d ++ concatMap sectionsOf (Map.elems m)
      TestY d m -> sectionsOf d ++ concatMap sectionsOf (Map.elems m)

-- | The cells carried by every Let in a section tree.
letCellsOf :: MSection -> [Ptr NativeCell]
letCellsOf s = [c | Let _ _ _ _ c <- sectionsOf s]

-- | The constants a section tree needs from the pool.
poolKeysOf :: MSection -> [PoolKey]
poolKeysOf s =
  concat [keys i | Ins i _ <- sectionsOf s]
    ++ concat [combKey r | App _ r ZArgs <- sectionsOf s]
    ++ concat [cachedKey cix comb | Call _ cix comb ZArgs <- sectionsOf s]
    -- a function applied to fewer arguments than it takes: the partial
    -- application is built from the function's closure
    ++ [ KeyComb cix info
         | App _ (Env cix comb) args <- sectionsOf s,
           Comb info@(LamI arity _ _ _) <- [unRComb comb],
           Just n <- [argCount args],
           n > 0,
           n < arity
       ]
  where
    -- a known combinator used as a value, or a top-level value
    combKey = \case
      Env cix comb
        | Comb info <- unRComb comb -> [KeyComb cix info]
        | otherwise -> cachedKey cix comb
      _ -> []
    cachedKey cix comb = case unRComb comb of
      CachedVal _ v -> [KeyCached cix (getBoxedVal v)]
      _ -> []
    argCount = \case
      ZArgs -> Just 0
      VArg1 _ -> Just 1
      VArg2 _ _ -> Just 2
      VArgR _ l -> Just l
      VArgN v -> Just (sizeofPrimArray v)
      VArgV _ -> Nothing
    keys = \case
      Pack r t ZArgs -> [KeyEnum r t]
      Pack r _ _ -> [KeyEnum r (PackedTag 0)]
      Lit l@(MT _) -> [KeyLit l]
      Lit l@(MM _) -> [KeyLit l]
      Lit l@(MY _) -> [KeyLit l]
      Prim2 REFW _ _ -> [KeyEnum Ty.unitRef TT.unitTag]
      -- the list helpers are handed the result type's field-less constructor
      Prim1 VWLS _ -> [KeyEnum Ty.seqViewRef TT.seqViewEmptyTag]
      Prim1 VWRS _ -> [KeyEnum Ty.seqViewRef TT.seqViewEmptyTag]
      Prim2 IDXS _ _ -> [KeyEnum Ty.optionalRef TT.noneTag]
      -- a partial application of a known function starts from its closure
      Name (Env cix comb) _ | Comb info <- unRComb comb -> [KeyComb cix info]
      ForeignCall _ MutableArray_write _ -> [KeyEnum Ty.unitRef TT.unitTag]
      _ -> []

-- | What compilation needs, found once at startup.
data JITState = JITState
  { jsLayouts :: Layouts,
    jsCtx :: CtxOffsets,
    jsRts :: RtsFacts
  }

-- | One LLVM function to generate, with what it brings along: a
-- combinator, or a re-entry function that was left for later.
data Unit = Unit
  { -- | the LLVM function's name
    uName :: String,
    -- | the combinator-level function this belongs to (itself, for a
    -- combinator): re-entry functions generated at different times for
    -- the same root share one table of cells
    uRoot :: String,
    uCell :: Ptr NativeCell,
    -- | the group the code is from, for the arity of Let body combinators
    uCombs :: MCombs,
    uSection :: MSection,
    -- | for the IR dump: what this is, and its MCode
    uDescribe :: String,
    -- | a re-entry function that waits to be asked for
    uOnDemand :: Bool,
    -- | the arity, for a combinator that can have a worker (an entry
    -- point or local function whose returns all yield one value)
    uWorker :: Maybe Int,
    uGen :: CG.Env -> Either String Function
  }

unitName :: Unit -> String
unitName = uName

-- | Re-entry functions that have a cell but no code yet, by cell. The
-- interpreter counts the times it finds such a cell empty; when the count
-- says the re-entry point is used, the unit is found here and queued for
-- compilation. The cell's request state keeps it from being queued
-- twice.
pending :: IORef (Map.Map (Ptr NativeCell) Unit)
pending = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE pending #-}

addPending :: Unit -> IO ()
addPending u = do
  writeNativeCount (uCell u) (negate (threshold config))
  atomicModifyIORef' pending (\m -> (Map.insert (uCell u) u m, ()))

lookupPending :: Ptr NativeCell -> IO (Maybe Unit)
lookupPending cell = Map.lookup cell <$> readIORef pending

-- | Forgets a unit that has been dealt with.
dropPending :: Unit -> IO ()
dropPending u = atomicModifyIORef' pending (\m -> (Map.delete (uCell u) m, ()))

-- | What has been compiled so far, for @UNISON_JIT_STATS@.
data CompileTotals = CompileTotals
  { -- | modules handed to LLVM
    ctModules :: !Int,
    -- | functions for combinators, auxiliary functions generated with
    -- them, and re-entry functions generated later, on demand
    ctFunctions, ctAuxiliary, ctOnDemand :: !Int,
    -- | re-entry functions still waiting to be asked for
    ctPending :: !Int,
    -- | functions left to the interpreter because native code for them
    -- would mostly exit (see JIT.Estimate)
    ctNotWorthIt :: !Int,
    ctIRBytes :: !Int,
    ctNanoseconds :: !Word64
  }

totals :: IORef CompileTotals
totals = unsafePerformIO (newIORef (CompileTotals 0 0 0 0 0 0 0 0))
{-# NOINLINE totals #-}

compileTotals :: IO CompileTotals
compileTotals = do
  t <- readIORef totals
  n <- Map.size <$> readIORef pending
  pure t {ctPending = n}

-- | The auxiliary functions known for each root function (see 'uRoot').
rootMemos :: IORef (Map.Map String AuxMemo)
rootMemos = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE rootMemos #-}

-- | For each group number, the first cell of every group seen with that
-- number (see 'groupUnits').
groupCells :: IORef (Map.Map Word64 [Ptr NativeCell])
groupCells = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE groupCells #-}

-- | The units of one top-level definition: those to compile now, and those
-- to compile when they turn out to be used. Entry points and local
-- functions (zero in the low bits of their number) are compiled now. A Let
-- body combinator is a re-entry point, entered only through the cell its
-- Let carries when the Let's binding left native code: when @lazy@, it
-- waits until that has happened often enough. Let body combinators that no
-- Let refers to (the ones inside bindings) are never entered.
--
-- Function names must be unique in the process, and group numbers aren't
-- when a second runtime (with a code cache of its own) is started: the
-- names of a group whose number has been seen with different cells get a
-- suffix.
groupUnits :: Bool -> Reference -> Word64 -> MCombs -> IO ([Unit], [Unit])
groupUnits lazy ref grp combs = do
  suffix <- case [cell | (_, _, _, _, cell) <- every] of
    [] -> pure ""
    first : _ -> atomicModifyIORef' groupCells $ \m ->
      let seen = Map.findWithDefault [] grp m
       in case lookup first (zip seen [1 :: Int ..]) of
            Just 1 -> (m, "")
            Just n -> (m, "_v" ++ show n)
            Nothing -> (Map.insert grp (seen ++ [first]) m, if null seen then "" else "_v" ++ show (length seen + 1))
  pure (map (unit suffix False) now, map (unit suffix True) later)
  where
    letCells = Set.fromList [c | Comb (LamI _ _ s _) <- map snd (EC.mapToList combs), c <- letCellsOf s, c /= noNativeCell]
    candidates = [c | c@(i, _, _, _, cell) <- every, isEntry i || Set.member cell letCells]
    every = [(i, a, f, entry, cell) | (i, Comb (LamI a f entry cell)) <- EC.mapToList combs, cell /= noNativeCell]
    isEntry i = i .&. 0xFFFF == 0
    now = [c | c@(i, _, _, _, _) <- candidates, isEntry i || not lazy]
    later = [c | c@(i, _, _, _, _) <- candidates, not (isEntry i), lazy]
    unit suffix onDemand (i, a, f, entry, cell) =
      let name = "u" ++ show grp ++ "_" ++ show i ++ suffix
          cix = CIx ref grp i
          describe =
            "; " ++ name ++ " = " ++ show cix ++ "\n;   arity " ++ show a ++ ", frame size " ++ show f ++ "\n"
              ++ unlines (map ("; " ++) (lines (prettySection 4 entry "")))
          worker = if isEntry i && CG.workerShape entry then Just a else Nothing
       in Unit name name cell combs entry describe onDemand worker (\env -> genFunction env name cix a f entry cell)

-- | The unit for a re-entry function its parent left for later.
deferredUnit :: Unit -> Deferred -> Unit
deferredUnit parent d =
  Unit
    (dName d)
    (uRoot parent)
    (dCell d)
    (uCombs parent)
    (dBody d)
    ( "; " ++ dName d ++ " = re-entry into " ++ show (dCix d) ++ " at depth " ++ show (dLoaded d) ++ ", frame base " ++ show (dBase d) ++ "\n"
        ++ unlines (map ("; " ++) (lines (prettySection 4 (dBody d) "")))
    )
    True
    Nothing
    (\env -> genDeferred env d)

-- | Compiles some units as one module and installs the code in their
-- cells. Units that aren't worth compiling (see JIT.Estimate) or that the
-- code generator can't handle are left interpreted, and are not asked for
-- again: a cell's count reaches zero once. When @lazy@, the re-entry
-- functions that are only needed when something exits are not generated:
-- they become pending units (see 'pending').
compileUnits :: JITState -> Map.Map Reference [Int] -> String -> Bool -> [Unit] -> IO ()
compileUnits st types modName lazy candidates = do
  t0 <- getMonotonicTimeNSec
  judged <- forM candidates $ \u -> (,) u <$> judge (uCell u) (uSection u)
  let units = [u | (u, (True, _)) <- judged]
      notWorthIt =
        [ (u, "left interpreted: an average path takes " ++ showEstimate e)
          | (u, (False, Just e)) <- judged
        ]
      showEstimate (Estimate saving exits _) =
        show2 exits ++ " exits and saves " ++ show2 saving ++ " instructions' overhead"
      show2 x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double)
  forM_ notWorthIt $ \(u, why) -> jitLog (uName u ++ ": " ++ why)
  atomicModifyIORef' totals (\t -> (t {ctNotWorthIt = ctNotWorthIt t + length notWorthIt}, ()))
  jitLog (modName ++ ": compiling " ++ unwords (map uName units))
  poolIxs <- poolIndices (concatMap (poolKeysOf . uSection) units)
  memos <- readIORef rootMemos
  let env local workers u (base, fbase, cells) =
        CG.Env (jsLayouts st) (jsCtx st) base fbase (stressPoll config > 0) (stressCallee config > 0) (uCombs u) poolIxs (jsRts st) types cells (disabled config) lazy (Map.findWithDefault Map.empty (uRoot u) memos) local workers
      -- The functions that get a worker: calls to them from this module
      -- pass arguments and results in registers (docs/jit-m6.md, step 9).
      workersOf us
        | any (`elem` disabled config) ["direct", "worker"] = Map.empty
        | otherwise = Map.fromList [(uCell u, (uName u ++ "_w", a)) | u <- us, Just a <- [uWorker u]]
      -- first pass: find out which functions compile, and how many exits,
      -- frames and cells for auxiliary functions each needs
      -- A function that fails as a worker (it turns out to return other
      -- than one value) is generated in the uniform form instead.
      candidates1 = workersOf units
      firstTry u = uGen u (env Map.empty candidates1 u (0, 0, repeat noNativeCell))
      firstPass =
        [ case firstTry u of
            Left _ | Map.member (uCell u) candidates1 -> (u {uWorker = Nothing}, uGen u (env Map.empty (Map.delete (uCell u) candidates1) u (0, 0, repeat noNativeCell)))
            r -> (u, r)
          | u <- units
        ]
      failed = [(u, "not compiled: " ++ why) | (u, Left why) <- firstPass]
      skipped = notWorthIt ++ failed
      ok = [(u, f) | (u, Right f) <- firstPass]
  forM_ failed $ \(u, why) -> jitLog (uName u ++ ": " ++ why)
  unless (null ok) $ do
    let counts = map (length . fnExits . snd) ok
        fcounts = map (length . fnFrames . snd) ok
        acounts = map (\(_, f) -> length (fnAux f) + length (fnDeferred f)) ok
    base <- registerExits (concatMap (fnExits . snd) ok)
    fbase <- registerFrames (concatMap (fnFrames . snd) ok)
    cellBlock <- newNativeCells (sum acounts)
    -- second pass, with each function's real exit and frame bases and cells
    let cells = [map (nativeCellAt cellBlock) [a .. a + n - 1] | (a, n) <- zip (scanl (+) 0 acounts) acounts]
        bases = zip3 (scanl (+) base counts) (scanl (+) fbase fcounts) cells
        -- calls between the functions of this module are direct (a call
        -- site has the same exits either way, so the counts above hold)
        local
          | "direct" `elem` disabled config = Map.empty
          | otherwise = Map.fromList [(uCell u, uName u) | (u, _) <- ok]
        compiled = [(u, f) | (b, (u, _)) <- zip bases ok, Right f <- [uGen u (env local (workersOf (map fst ok)) u b)]]
        fns = map snd compiled
        ir = modulePrelude ++ unlines (map fnIR fns)
    -- the second pass's exits and frames name the auxiliary functions' cells
    replaceExits base (concatMap fnExits fns)
    replaceFrames fbase (concatMap fnFrames fns)
    -- The dump has each function's MCode as a comment above its IR, and
    -- says which units were not compiled and why.
    forM_ (dumpIR config) $ \dir -> do
      let annotated =
            unlines $
              [ "; module " ++ modName,
                "; " ++ show (length compiled) ++ " of " ++ show (length candidates) ++ " functions compiled"
              ]
                ++ ["; " ++ uName u ++ " " ++ why | (u, why) <- skipped]
                ++ [modulePrelude]
                ++ concat [[uDescribe u, fnIR f] | (u, f) <- compiled]
      if dir == "-"
        then jitDump annotated
        else writeFile (dir </> modName ++ ".ll") annotated
    r <- addModule (isJust (dumpIR config)) "default<O2>" ir
    case r of
      -- a module that doesn't compile or link is a bug in the generator: say so even without the log
      Left e -> hPutStrLn stderr ("[jit] " ++ modName ++ ": " ++ e)
      Right optimized -> do
        -- the module after O2: what actually runs
        forM_ ((,) <$> dumpIR config <*> optimized) $ \(dir, txt) ->
          if dir == "-"
            then jitDump ("; module " ++ modName ++ " after O2\n" ++ txt)
            else writeFile (dir </> modName ++ ".opt.ll") txt
        -- The re-entry functions left for later become pending before any
        -- code that can exit to them is installed, so that no request for
        -- one is ever made before it is known here.
        forM_ compiled $ \(u, f) -> do
          atomicModifyIORef' rootMemos (\m -> (Map.insertWith Map.union (uRoot u) (fnMemo f) m, ()))
          forM_ (fnDeferred f) (addPending . deferredUnit u)
        let described = [(uName u, takeWhile (/= '\n') (drop 2 (dropWhile (/= '=') (uDescribe u)))) | (u, _) <- compiled]
        forM_ compiled $ \(_, f) -> do
          forM_ (fnNotes f) $ \note -> jitLog (fnName f ++ ": partly interpreted: " ++ note)
          forM_ ((fnName f, fnCell f) : fnAux f) $ \(sym, cell) ->
            lookupSymbol sym >>= \case
              Left e -> hPutStrLn stderr ("[jit] " ++ sym ++ " could not be linked: " ++ e)
              Right fp -> do
                -- stress mode install=N: a module's functions appear one at a time
                when (stressInstall config > 0) (threadDelay (1000 * stressInstall config))
                when (stats config) (nameEntry (castFunPtrToPtr fp) (sym ++ maybe "" (" = " ++) (lookup sym described)))
                writeNativeCode cell (castFunPtrToPtr fp)
        t1 <- getMonotonicTimeNSec
        let onDemand = length [() | (u, _) <- compiled, uOnDemand u]
        atomicModifyIORef' totals $ \t ->
          ( t
              { ctModules = ctModules t + 1,
                ctFunctions = ctFunctions t + length compiled - onDemand,
                ctAuxiliary = ctAuxiliary t + sum (map (length . fnAux) fns),
                ctOnDemand = ctOnDemand t + onDemand,
                ctIRBytes = ctIRBytes t + length ir,
                ctNanoseconds = ctNanoseconds t + (t1 - t0)
              },
            ()
          )
        jitLog
          ( modName ++ ": compiled " ++ show (length compiled) ++ " of " ++ show (length candidates)
              ++ " functions, " ++ show (sum (map (length . fnAux) fns)) ++ " auxiliary, "
              ++ show (sum (map (length . fnDeferred) fns)) ++ " left for later, "
              ++ show (sum counts) ++ " exits, " ++ show (length ir `div` 1024) ++ " KB of IR, in "
              ++ show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms"
          )
