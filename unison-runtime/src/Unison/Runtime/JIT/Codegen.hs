{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | MCode to LLVM IR, for the subset the JIT supports. See docs/jit-m1.md
-- for what that subset is and docs/jit-design.md for the conventions.
--
-- Within a function, every Unison stack slot is an LLVM alloca (decision
-- D11): @%u<k>@ holds the unboxed word and @%b<k>@ the boxed pointer of the
-- slot at frame offset @k@, which is stack index @fp + k@. The frame depth
-- @d@ (the interpreter's @sp - fp@) is known statically at every point, so
-- MCode's "slot @i@ from the top" is frame offset @d - i@. The real stack
-- is touched only at entry, before a tail call, and on exit paths.
module Unison.Runtime.JIT.Codegen
  ( Env (..),
    CtxOffsets (..),
    RtsFacts (..),
    Function (..),
    Deferred (..),
    AuxKey,
    AuxMemo,
    genFunction,
    genDeferred,
    modulePrelude,
    pushCount,
  )
where

import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.State.Strict
import Data.Char (ord)
import Data.Bits (shiftL, shiftR)
import Data.Int (Int64)
import Data.Primitive.PrimArray (primArrayToList)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, WordPtr (..), ptrToWordPtr)
import GHC.Float (castDoubleToWord64)
import Unison.Runtime.JIT.Exits (Exit (..))
import Unison.Runtime.JIT.Frames (Frame (..))
import Unison.Runtime.JIT.Layout
import Unison.Runtime.JIT.Pool
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.TypeTags qualified as TT
import Unison.Builtin.Decls qualified as Ty (unitRef)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IM
import Unison.Runtime.MCode hiding (Env)
import Unison.Runtime.MCode qualified as MCode (GRef (Env))
import Unison.Runtime.Machine.Types (MCombs, MRef, MSection)
import Unison.Runtime.Stack (Val)
import Unison.Util.EnumContainers qualified as EC

-- | Byte offsets of the fields of the C @Ctx@, from @unison_jit_ctx_layout@.
data CtxOffsets = CtxOffsets
  { oUstk, oBstk, oPool, oStackSize, oHplim, oAp, oFp, oSp, oMaxSp, oStressPoll, oStressPollLeft, oStressCallee, oStressCalleeLeft, oFrames, oNFrames, oMaxFrames, oCStackLimit, oCap, oAllocLeft :: !Int
  }

-- | From unison_jit_rts_facts: the info pointers and layouts of the two
-- arrays a @Seg@ is made of.
data RtsFacts = RtsFacts
  { rArrWordsInfo, rArrPtrsInfo :: !Int,
    -- | ByteArray#: header words including the byte count, and the word index of the count
    rBytesHeader, rBytesCount :: !Int,
    -- | Array#: header words, word index of the element count and of the payload size
    rPtrsHeader, rPtrsCount, rPtrsSize :: !Int,
    -- | log2 of elements per card-table byte
    rCardBits :: !Int,
    -- | word index of a MutVar#'s content
    rMutVarVar :: !Int,
    -- | info pointer a mutable Array# gets when written
    rArrPtrsDirtyInfo :: !Int
  }

data Env = Env
  { envLayouts :: Layouts,
    envCtx :: CtxOffsets,
    -- | index of this module's first exit in the global table
    envExitBase :: Int,
    -- | index of this module's first frame in the global frame table
    envFrameBase :: Int,
    -- | emit the stress-mode poll countdown
    envStressPoll :: Bool,
    -- | emit the stress-mode "callee not compiled" countdown
    envStressCallee :: Bool,
    -- | the group being compiled, for the arity of Let body combinators
    envCombs :: MCombs,
    -- | pool index of every constant this group uses
    envPool :: Map.Map PoolKey Int,
    envRts :: RtsFacts,
    -- | constructor arities of the data types loaded so far
    envTypes :: Map.Map Reference [Int],
    -- | cells for the auxiliary functions (re-entry points), taken in order
    envCells :: [Ptr NativeCell],
    -- | features turned off for debugging (see Config)
    envDisabled :: [String],
    -- | leave the re-entry points that are only used when something exits
    -- (bodies of Lets inside bindings, slow paths of instructions with a
    -- native fast path) to be generated when they turn out to be used:
    -- each gets a cell and a 'Deferred', but no code
    envLazy :: Bool,
    -- | the auxiliary functions already known for the function this one
    -- belongs to, generated or deferred: a re-entry function generated
    -- later reuses the cells its parent handed out
    envKnown :: AuxMemo,
    -- | the functions defined in the module being generated, by cell: a
    -- call to one of them is a direct call to its symbol, which LLVM can
    -- inline, instead of a call through the cell
    envLocal :: Map.Map (Ptr NativeCell) String
  }

-- | What an auxiliary function is generated from: the section (without its
-- combinator references, which have no Ord; their CombIx stays), the depth
-- it starts at, and the frame base.
type AuxKey = (GSection (), Int, Int)

type AuxMemo = Map.Map AuxKey (String, Ptr NativeCell)

-- | A re-entry function that was given a cell but not generated: all that
-- 'genDeferred' needs to generate it later.
data Deferred = Deferred
  { dName :: String,
    dCix :: CombIx,
    -- | slots on the stack at entry
    dLoaded :: Int,
    dFrameSize :: Int,
    -- | the frame base (see 'feBase')
    dBase :: Int,
    dBody :: MSection,
    dCell :: Ptr NativeCell
  }

data Function = Function
  { fnName :: String,
    fnIR :: String,
    -- | in index order, starting at the module's base plus the count before this function
    fnExits :: [Exit],
    -- | likewise for the frame table
    fnFrames :: [Frame],
    fnCell :: Ptr NativeCell,
    -- | the auxiliary functions defined alongside (re-entry points after
    -- call-outs, bodies of Lets inside bindings) and their cells
    fnAux :: [(String, Ptr NativeCell)],
    -- | the re-entry functions given a cell but left for later
    fnDeferred :: [Deferred],
    -- | every auxiliary function known after this one was generated
    fnMemo :: AuxMemo,
    -- | why parts of the function fell back to the interpreter
    fnNotes :: [String]
  }

-- | Declarations every module needs.
modulePrelude :: String
modulePrelude = "declare ptr @llvm.stacksave.p0()\ndeclare ptr @unison_jit_alloc_words(ptr, i64)\ndeclare void @unison_jit_write_mutvar(ptr, ptr, ptr)\n"

-- ---------------------------------------------------------------------------
-- The generator

data GS = GS
  { gsFresh :: !Int,
    -- | finished blocks, reversed: (label, instructions in order)
    gsBlocks :: [(String, [String])],
    -- | the block being written: label and reversed instructions
    gsCur :: (String, [String]),
    -- | exits, reversed
    gsExits :: [Exit],
    gsNExits :: !Int,
    -- | frames, reversed
    gsFrames :: [Frame],
    gsNFrames :: !Int,
    -- | highest frame offset used
    gsMaxK :: !Int,
    -- | slots whose value is a boolean held as an i1 register, with no
    -- closure built yet (docs/jit-m3.md, step 3). Absent means the slot's
    -- allocas hold the value. Saved and restored around branch arms.
    gsKinds :: IM.IntMap String,
    -- | set when the function can't be compiled after all
    gsFailed :: Maybe String,
    -- | auxiliary functions generated so far: their IR, their names and
    -- cells, how many there are, and the cells left to hand out
    gsAuxText :: [String],
    gsAuxCells :: [(String, Ptr NativeCell)],
    gsNAux :: !Int,
    gsCells :: [Ptr NativeCell],
    -- | why parts of the function fell back to the interpreter, for the log
    gsNotes :: [String],
    -- | the captured segment of the closure being called: its arrays and
    -- element count, from 'closureCallee' for 'copyCaptured'
    gsCaptured :: Maybe (String, String, String),
    -- | auxiliary functions already generated, by what they were generated
    -- from (section, depth, frame base). The same Let body or call-out
    -- continuation is met again wherever its enclosing code is generated
    -- more than once (inline after a binding and in an auxiliary function
    -- for the binding's body); without this the output grows exponentially
    -- with the nesting of bindings.
    -- The combinator references are dropped from the key (their CombIx
    -- stays), since they have no Ord.
    gsAuxMemo :: AuxMemo,
    -- | highest pool index the function being generated uses
    gsMaxPool :: !Int,
    -- | re-entry functions left for later, reversed
    gsDeferred :: [Deferred]
  }

type Gen = State GS

fresh :: String -> Gen String
fresh base = do
  n <- gets gsFresh
  modify' (\s -> s {gsFresh = n + 1})
  pure ("%" ++ base ++ "." ++ show n)

freshLabel :: String -> Gen String
freshLabel base = do
  n <- gets gsFresh
  modify' (\s -> s {gsFresh = n + 1})
  pure (base ++ "." ++ show n)

emit :: String -> Gen ()
emit i = modify' (\s -> let (l, is) = gsCur s in s {gsCur = (l, i : is)})

-- | Ends the current block (which must have been terminated) and starts another.
startBlock :: String -> Gen ()
startBlock label = modify' $ \s ->
  let (l, is) = gsCur s
   in s {gsBlocks = (l, reverse is) : gsBlocks s, gsCur = (label, [])}

-- | Generates a block off to the side, then returns to the current one.
sideBlock :: String -> Gen () -> Gen String
sideBlock base body = do
  label <- if base `elem` ["grow", "stale"] then pure base else freshLabel base
  sideBlockNamed label body
  pure label

sideBlockNamed :: String -> Gen () -> Gen ()
sideBlockNamed label body = do
  cur <- gets gsCur
  modify' (\s -> s {gsCur = (label, [])})
  body
  modify' $ \s ->
    let (l, is) = gsCur s
     in s {gsBlocks = (l, reverse is) : gsBlocks s, gsCur = cur}

useK :: Int -> Gen ()
useK k
  | k < 0 = modify' (\s -> s {gsFailed = Just ("refers to a slot below the frame (offset " ++ show k ++ ")")})
  | otherwise = modify' (\s -> s {gsMaxK = max (gsMaxK s) k})

uSlot, bSlot :: Int -> String
uSlot k = "%u" ++ show k
bSlot k = "%b" ++ show k

-- | Slot offsets start at 1; offset 0 and below belong to the caller.
useSlot :: Int -> Gen ()
useSlot k
  | k < 1 = modify' (\s -> s {gsFailed = Just ("refers to a slot below the frame (offset " ++ show k ++ ")")})
  | otherwise = useK k

slotKind :: Int -> Gen (Maybe String)
slotKind k = gets (IM.lookup k . gsKinds)

-- | Marks slot @k@ as holding the boolean @c@ (an i1) with no closure.
setBoolKind :: Int -> String -> Gen ()
setBoolKind k c = useSlot k >> modify' (\s -> s {gsKinds = IM.insert k c (gsKinds s)})

clearKind :: Int -> Gen ()
clearKind k = modify' (\s -> s {gsKinds = IM.delete k (gsKinds s)})

-- | Runs a generator and puts the slot kinds back afterwards: for code
-- (a branch arm, an inline binding) after which control rejoins other paths.
withKinds :: Gen a -> Gen a
withKinds g = do
  ks <- gets gsKinds
  r <- g
  modify' (\s -> s {gsKinds = ks})
  pure r

loadU :: Int -> Gen String
loadU k = do
  useSlot k
  slotKind k >>= \case
    Just _ -> pure "-1" -- a boxed value's word
    Nothing -> do
      v <- fresh "u"
      emit (v ++ " = load i64, ptr " ++ uSlot k)
      pure v

-- | The closure in slot @k@; a boolean held as an i1 is materialized here.
loadB :: Int -> Gen String
loadB k = do
  useSlot k
  slotKind k >>= \case
    Just c -> do
      p <- fresh "bool"
      emit (p ++ " = select i1 " ++ c ++ ", ptr %val.true, ptr %val.false")
      pure p
    Nothing -> do
      v <- fresh "b"
      emit (v ++ " = load ptr, ptr " ++ bSlot k)
      pure v

storeU :: Int -> String -> Gen ()
storeU k v = useSlot k >> clearKind k >> emit ("store i64 " ++ v ++ ", ptr " ++ uSlot k)

storeB :: Int -> String -> Gen ()
storeB k v = useSlot k >> clearKind k >> emit ("store ptr " ++ v ++ ", ptr " ++ bSlot k)

-- | Address of stack index @fp + k@ in the unboxed or boxed stack.
stackAddrU, stackAddrB :: Int -> Gen String
stackAddrU k = do
  f <- fpPlus k
  a <- fresh "ua"
  emit (a ++ " = getelementptr i64, ptr %ustk, i64 " ++ f)
  pure a
stackAddrB k = do
  f <- fpPlus k
  a <- fresh "ba"
  emit (a ++ " = getelementptr ptr, ptr %bstk, i64 " ++ f)
  pure a

-- | The register holding @fp + k@. The entry block defines one for
-- every k up to the highest used, so using one records it.
fpPlus :: Int -> Gen String
fpPlus k = useK k >> pure ("%fpk" ++ show k)

ctxField :: Env -> (CtxOffsets -> Int) -> Gen String
ctxField env f = do
  a <- fresh "ctx"
  emit (a ++ " = getelementptr i8, ptr %ctx, i64 " ++ show (f (envCtx env)))
  pure a

-- ---------------------------------------------------------------------------
-- Exits

data FnEnv = FnEnv
  { feEnv :: Env,
    -- | the LLVM function's name; auxiliary functions are named after it
    feName :: String,
    feCix :: CombIx,
    -- | slots loaded at entry: the combinator's arity, or for an auxiliary
    -- function the depth it starts at
    feArity :: Int,
    feFrameSize :: Int,
    feCell :: Ptr NativeCell,
    -- | the loop head, for self tail calls; an auxiliary function has none
    feHead :: Maybe String,
    -- | the frame base: the frame offset the interpreter's @fp@ points at
    -- when it enters this function. Zero for a combinator. An auxiliary
    -- function generated inside an inline binding has the binding's base,
    -- and the interpreter holds a @Push@ frame for the binding's body. Slot
    -- offsets stay relative to the combinator's frame throughout.
    feBase :: Int,
    -- | registers holding the type-tag and boolean closures, loaded at entry
    feTagChar, feTagFloat, feTagInt, feTagNat :: String,
    -- | the inline @Let@ bindings the code being generated is inside of,
    -- innermost first
    feEnclosing :: [Enclosing]
  }

-- | An inline @Let@ binding being generated. Inside it, the interpreter's
-- view is a fresh frame starting at @enBase@ (its @ap = fp = sp0@), so
-- exits write a frame record for it, and a @Yield@ delivers the results
-- to the body instead of returning.
data Enclosing = Enclosing
  { -- | frame table index
    enIndex :: Int,
    -- | frame offset of the binding's frame base
    enBase :: Int,
    -- | label of the body block
    enBody :: String,
    -- | number of results the body expects
    enResults :: Int
  }

-- | The frame base the interpreter would see: @fp@ for the function's own
-- frame, or the innermost inline binding's base.
frameBase :: FnEnv -> Gen String
frameBase fe = fpPlus (currentBase fe)

-- | The frame offset of the interpreter's current frame base.
currentBase :: FnEnv -> Int
currentBase fe = case feEnclosing fe of
  [] -> feBase fe
  e : _ -> enBase e

-- | Writes the frame records for every enclosing inline binding,
-- innermost first (the order a chain of native callers would write them).
unwindEnclosing :: FnEnv -> Gen ()
unwindEnclosing fe = go (feEnclosing fe)
  where
    go [] = pure ()
    go (e : outer) = do
      let (fsz, asz) = case outer of
            [] -> (enBase e - feBase fe, Nothing)
            o : _ -> (enBase e - enBase o, Just "0")
      writeRecord (feEnv fe) (enIndex e) fsz asz
      go outer

-- | Writes one frame record. The pending-argument count is @fp - ap@
-- unless given.
writeRecord :: Env -> Int -> Int -> Maybe String -> Gen ()
writeRecord env ix fsz masz = do
  fr <- ctxField env oFrames
  frp <- fresh "frames"
  emit (frp ++ " = load ptr, ptr " ++ fr)
  nfa <- ctxField env oNFrames
  nf <- fresh "nf"
  emit (nf ++ " = load i64, ptr " ++ nfa)
  off <- fresh "off"
  emit (off ++ " = mul i64 " ++ nf ++ ", 3")
  rec0 <- fresh "rec"
  emit (rec0 ++ " = getelementptr i64, ptr " ++ frp ++ ", i64 " ++ off)
  emit ("store i64 " ++ show ix ++ ", ptr " ++ rec0)
  rec1 <- fresh "rec"
  emit (rec1 ++ " = getelementptr i64, ptr " ++ rec0 ++ ", i64 1")
  emit ("store i64 " ++ show fsz ++ ", ptr " ++ rec1)
  rec2 <- fresh "rec"
  emit (rec2 ++ " = getelementptr i64, ptr " ++ rec0 ++ ", i64 2")
  asz <- case masz of
    Just a -> pure a
    Nothing -> do
      a <- fresh "asz"
      emit (a ++ " = sub i64 %fpb, %ap")
      pure a
  emit ("store i64 " ++ asz ++ ", ptr " ++ rec2)
  nf' <- fresh "nf"
  emit (nf' ++ " = add i64 " ++ nf ++ ", 1")
  emit ("store i64 " ++ nf' ++ ", ptr " ++ nfa)

-- | Adds an exit and returns its global index.
addExit :: Env -> Exit -> Gen Int
addExit env e = do
  n <- gets gsNExits
  modify' (\s -> s {gsExits = e : gsExits s, gsNExits = n + 1})
  pure (envExitBase env + n)

-- | Adds a frame table entry and returns its global index.
addFrame :: Env -> Frame -> Gen Int
addFrame env f = do
  n <- gets gsNFrames
  modify' (\s -> s {gsFrames = f : gsFrames s, gsNFrames = n + 1})
  pure (envFrameBase env + n)

-- | Writes slots 1..d back to the Unison stack.
writeFrame :: Int -> Gen ()
writeFrame d =
  forM_ [1 .. d] $ \k -> do
    u <- loadU k
    ua <- stackAddrU k
    emit ("store i64 " ++ u ++ ", ptr " ++ ua)
    b <- loadB k
    ba <- stackAddrB k
    emit ("store ptr " ++ b ++ ", ptr " ++ ba)

-- | A stress-mode countdown on a pair of Ctx fields; gives an i1 that is
-- true every Nth time.
stressFire :: Env -> (CtxOffsets -> Int) -> (CtxOffsets -> Int) -> Gen String
stressFire env oLeft oEvery = do
  left <- ctxField env oLeft
  n <- fresh "left"
  emit (n ++ " = load i64, ptr " ++ left)
  n' <- fresh "left"
  emit (n' ++ " = sub i64 " ++ n ++ ", 1")
  fire <- fresh "fire"
  emit (fire ++ " = icmp sle i64 " ++ n' ++ ", 0")
  every <- ctxField env oEvery
  ev <- fresh "every"
  emit (ev ++ " = load i64, ptr " ++ every)
  reset <- fresh "reset"
  emit (reset ++ " = select i1 " ++ fire ++ ", i64 " ++ ev ++ ", i64 " ++ n')
  emit ("store i64 " ++ reset ++ ", ptr " ++ left)
  pure fire

-- | Loads the callee's code pointer from its cell. Gives the pointer and
-- an i1 saying whether the callee must be treated as not compiled. A
-- callee defined in this module is named directly, and is always there
-- (except under the callee stress mode, which keeps the cell path tested).
loadCallee :: Env -> Ptr NativeCell -> Gen (String, String)
loadCallee env cell
  | not (envStressCallee env), Just name <- Map.lookup cell (envLocal env) = pure ("@" ++ name, "false")
loadCallee env cell = do
  let WordPtr addr = ptrToWordPtr cell
  fnp <- fresh "fn"
  emit (fnp ++ " = load ptr, ptr inttoptr (i64 " ++ show addr ++ " to ptr)")
  isNull <- fresh "isnull"
  emit (isNull ++ " = icmp eq ptr " ++ fnp ++ ", null")
  if envStressCallee env
    then do
      fire <- stressFire env oStressCalleeLeft oStressCallee
      skip <- fresh "skip"
      emit (skip ++ " = or i1 " ++ isNull ++ ", " ++ fire)
      pure (fnp, skip)
    else pure (fnp, isNull)

-- | A block that writes the frame back to the Unison stack, records the
-- stack pointers in @Ctx@, and returns the exit's index. @d@ is the frame
-- depth at the exit point; every slot 1..d is written back.
exitBlock :: FnEnv -> Int -> Exit -> Gen String
exitBlock = exitBlockNamed "exit"

exitBlockNamed :: String -> FnEnv -> Int -> Exit -> Gen String
exitBlockNamed base fe d e = do
  let env = feEnv fe
  ix <- addExit env e
  sideBlock base $ do
    writeFrame d
    b <- frameBase fe
    ap <- ctxField env oAp
    emit ("store i64 " ++ (if null (feEnclosing fe) then "%ap" else b) ++ ", ptr " ++ ap)
    fp <- ctxField env oFp
    emit ("store i64 " ++ b ++ ", ptr " ++ fp)
    sp <- ctxField env oSp
    f <- fpPlus d
    emit ("store i64 " ++ f ++ ", ptr " ++ sp)
    unwindEnclosing fe
    emit ("ret i64 " ++ show ix)

-- | Terminates the current block with a resume exit at this section.
exitResume :: FnEnv -> Int -> MSection -> Gen ()
exitResume fe d sect = do
  l <- exitBlock fe d (Resume (feCix fe) sect)
  emit ("br label %" ++ l)

-- ---------------------------------------------------------------------------
-- Functions

-- | Compiles one combinator, or says why it can't be.
genFunction :: Env -> String -> CombIx -> Int -> Int -> MSection -> Ptr NativeCell -> Either String Function
genFunction env name cix arity frameSize body cell
  | not (startsSupported body) = Left "body starts with something the JIT doesn't compile"
  | otherwise = runFunction env name cix arity frameSize cell (Just "head") 0 body

-- | Generates a re-entry function that was left for later.
genDeferred :: Env -> Deferred -> Either String Function
genDeferred env d = runFunction env (dName d) (dCix d) (dLoaded d) (dFrameSize d) (dCell d) Nothing (dBase d) (dBody d)

runFunction :: Env -> String -> CombIx -> Int -> Int -> Ptr NativeCell -> Maybe String -> Int -> MSection -> Either String Function
runFunction env name cix arity frameSize cell headL base body =
  let fe = FnEnv env name cix arity frameSize cell headL base "%tag.char" "%tag.float" "%tag.int" "%tag.nat" []
      gs0 = GS 0 [] ("head", []) [] 0 [] 0 arity IM.empty Nothing [] [] 0 (envCells env) [] Nothing (envKnown env) (-1) []
      (text, gs) = runState (genFunctionText fe body) gs0
   in case gsFailed gs of
        Just why -> Left why
        Nothing ->
          Right
            ( Function
                name
                (unlines (text : reverse (gsAuxText gs)))
                (reverse (gsExits gs))
                (reverse (gsFrames gs))
                cell
                (reverse (gsAuxCells gs))
                (reverse (gsDeferred gs))
                (gsAuxMemo gs)
                (reverse (gsNotes gs))
            )

-- | The text of one LLVM function for @body@, generated in the current
-- state (which must hold no blocks yet). Its exits and frames join the
-- state's lists.
genFunctionText :: FnEnv -> MSection -> Gen String
genFunctionText fe body = do
  let env = feEnv fe
      arity = feArity fe
  growIx <- gets gsNExits
  genHead fe body
  startBlock "unreachable"
  gs <- get
  let blocks = [b | b@(l, _) <- reverse (gsBlocks gs), l /= "unreachable"]
      maxK = max (gsMaxK gs) (arity + feFrameSize fe)
      entry = entryBlock env arity (feBase fe) maxK (gsMaxPool gs)
      -- the grow exit, the first this function added, asks for what the
      -- entry check demanded
      fixGrow i e
        | i == growIx, GrowStack _ c <- e = GrowStack (maxK - arity) c
        | otherwise = e
  modify' (\s -> s {gsExits = zipWith fixGrow [gsNExits s - 1, gsNExits s - 2 ..] (gsExits s)})
  pure . unlines $
    ["define i64 @" ++ feName fe ++ "(ptr %ctx, i64 %ap, i64 %fp.in, i64 %sp) {"]
      ++ entry
      ++ concat [(l ++ ":") : map ("  " ++) is | (l, is) <- blocks]
      ++ ["}"]

-- | Generates an auxiliary function in this module: the code for @body@
-- starting at depth @loaded@ (that many slots are on the stack), entered
-- by the interpreter with its frame pointer at frame offset @base@. Gives
-- its name and cell, or Nothing if it can't be compiled or there are no
-- cells left. Its exits and frames join this module's tables.
--
-- When @later@ is set and the module is generated lazily, the function
-- only gets its name and cell, and a 'Deferred' to generate it from when
-- the interpreter has found the cell empty often enough.
genAuxFunction :: Bool -> FnEnv -> Int -> Int -> MSection -> Gen (Maybe (String, Ptr NativeCell))
genAuxFunction later fe loaded base body = do
  s <- get
  let key = (void body, loaded, base)
  case (Map.lookup key (gsAuxMemo s), gsCells s) of
    (Just known, _) -> pure (Just known)
    (_, []) -> pure Nothing
    (_, cell : cells)
      | later && envLazy (feEnv fe) -> do
          let name = feName fe ++ "_r" ++ show (gsNAux s)
          put
            s
              { gsCells = cells,
                gsNAux = gsNAux s + 1,
                gsDeferred = Deferred name (feCix fe) loaded (feFrameSize fe) base body cell : gsDeferred s,
                gsAuxMemo = Map.insert key (name, cell) (gsAuxMemo s)
              }
          pure (Just (name, cell))
    (_, cell : cells) -> do
      let name = feName fe ++ "_r" ++ show (gsNAux s)
          fe' = fe {feName = name, feArity = loaded, feCell = cell, feHead = Nothing, feBase = base, feEnclosing = []}
          gs0 = s {gsFresh = 0, gsBlocks = [], gsCur = ("head", []), gsMaxK = loaded, gsKinds = IM.empty, gsFailed = Nothing, gsCells = cells, gsNAux = gsNAux s + 1, gsCaptured = Nothing, gsMaxPool = -1}
          (text, gs) = runState (genFunctionText fe' body) gs0
      case gsFailed gs of
        Just why -> put s {gsNotes = (name ++ ": " ++ why) : gsNotes gs} >> pure Nothing
        Nothing -> do
          put
            s
              { gsNotes = gsNotes gs,
                gsExits = gsExits gs,
                gsNExits = gsNExits gs,
                gsFrames = gsFrames gs,
                gsNFrames = gsNFrames gs,
                gsAuxText = text : gsAuxText gs,
                gsAuxCells = (name, cell) : gsAuxCells gs,
                gsNAux = gsNAux gs,
                gsCells = gsCells gs,
                gsDeferred = gsDeferred gs,
                gsAuxMemo = Map.insert key (name, cell) (gsAuxMemo gs)
              }
          pure (Just (name, cell))

-- The first instruction decides whether compiling is worth anything: a
-- section that only calls out and yields the result is better interpreted.
startsSupported :: MSection -> Bool
startsSupported = \case
  Ins (Lit _) _ -> True
  Ins (Pack {}) _ -> True
  Ins (Prim1 op _) _ | prim1Supported op -> True
  Ins (Prim2 op _ _) _ | prim2Supported op -> True
  Ins i rest -> callOutWorthwhile i rest
  Match {} -> True
  DMatch {} -> True
  Call {} -> True
  App _ (MCode.Env _ _) _ -> True
  App _ (Stk _) _ -> True
  Yield {} -> True
  Let b _ _ _ _ -> startsSupported b
  _ -> False

-- | Whether an instruction the generator can't compile can be a call-out
-- with native code after it.
callOutWorthwhile :: GInstr (RComb Val) -> MSection -> Bool
callOutWorthwhile i rest = case (pushCount i, rest) of
  (Just _, Yield (VArgV _)) -> False
  (Just _, _) -> True
  _ -> False

-- | The entry block: allocas, addresses from @Ctx@, argument loads, the
-- stack check, then a branch to the loop head. @maxK@ is the highest
-- frame offset the function touches, which is at least the frame size and
-- covers the arguments of every call it makes.
entryBlock :: Env -> Int -> Int -> Int -> Int -> [String]
entryBlock env arity base maxK maxPool =
  map ("  " ++) $
    -- %fp is the combinator's frame pointer, %fpb the interpreter's (the
    -- one passed in); they differ by the frame base
    [ "%fp = sub i64 %fp.in, " ++ show base,
      "%fpb = add i64 %fp, " ++ show base
    ]
      ++ ["%u" ++ show k ++ " = alloca i64" | k <- [1 .. maxK]]
      ++ ["%b" ++ show k ++ " = alloca ptr" | k <- [1 .. maxK]]
      ++ ["%fpk" ++ show k ++ " = add i64 %fp, " ++ show k | k <- [0 .. maxK]]
      ++ [ ctxLoad "ptr" "%ustk" oUstk,
           ctxLoad "ptr" "%bstk" oBstk,
           ctxLoad "ptr" "%pool" oPool,
           ctxLoad "ptr" "%hplim.p" oHplim,
           ctxLoad "i64" "%stack.size" oStackSize
         ]
      ++ concat [poolLoad "%tag.char" poolIndexCharTag, poolLoad "%tag.float" poolIndexFloatTag, poolLoad "%tag.int" poolIndexIntTag, poolLoad "%tag.nat" poolIndexNatTag, poolLoad "%val.true" poolIndexTrue, poolLoad "%val.false" poolIndexFalse]
      ++ concat
        [ [ "%arg.u" ++ show k ++ ".a = getelementptr i64, ptr %ustk, i64 %fpk" ++ show k,
            "%arg.u" ++ show k ++ " = load i64, ptr %arg.u" ++ show k ++ ".a",
            "store i64 %arg.u" ++ show k ++ ", ptr %u" ++ show k,
            "%arg.b" ++ show k ++ ".a = getelementptr ptr, ptr %bstk, i64 %fpk" ++ show k,
            "%arg.b" ++ show k ++ " = load ptr, ptr %arg.b" ++ show k ++ ".a",
            "store ptr %arg.b" ++ show k ++ ", ptr %b" ++ show k
          ]
          | k <- [1 .. arity]
        ]
      -- the high-water mark of slots this function may write, for marking bstk on return
      ++ [ "%maxsp.a = getelementptr i8, ptr %ctx, i64 " ++ show (oMaxSp (envCtx env)),
           "%maxsp.old = load i64, ptr %maxsp.a",
           "%maxsp.gt = icmp sgt i64 %fpk" ++ show maxK ++ ", %maxsp.old",
           "%maxsp.new = select i1 %maxsp.gt, i64 %fpk" ++ show maxK ++ ", i64 %maxsp.old",
           "store i64 %maxsp.new, ptr %maxsp.a"
         ]
      -- A constant past the pool's first array may not be in the array this
      -- run was entered with, if this function was installed after the run
      -- began (see Pool). Then exit, to be entered again with the current one.
      ++ ( if maxPool < poolStableSize
             then []
             else
               [ "%pool.n.a = getelementptr i64, ptr %pool, i64 " ++ show (rPtrsCount (envRts env) - rPtrsHeader (envRts env)),
                 "%pool.n = load i64, ptr %pool.n.a",
                 "%pool.ok = icmp ugt i64 %pool.n, " ++ show maxPool,
                 "br i1 %pool.ok, label %entry.room, label %stale",
                 "entry.room:"
               ]
         )
      -- the interpreter's check: sp + size + 1 < stack size, else grow
      ++ [ "%need = add i64 %sp, " ++ show (maxK - arity + 1),
           "%room = icmp slt i64 %need, %stack.size",
           "br i1 %room, label %head, label %grow"
         ]
  where
    ctxLoad ty name f =
      name ++ ".a = getelementptr i8, ptr %ctx, i64 " ++ show (f (envCtx env)) ++ "\n  " ++ name ++ " = load " ++ ty ++ ", ptr " ++ name ++ ".a"
    poolLoad name ix =
      [ name ++ ".a = getelementptr ptr, ptr %pool, i64 " ++ show ix,
        name ++ " = load ptr, ptr " ++ name ++ ".a"
      ]

-- | The loop head: the high-water mark, the poll, then the body.
genHead :: FnEnv -> MSection -> Gen ()
genHead fe body = do
  let env = feEnv fe
      d = feArity fe
  -- the entry block's stack check branches to %grow when the frame doesn't
  -- fit; the size asked for is fixed up in genFunction once it is known
  _ <- exitBlockNamed "grow" fe d (GrowStack (feFrameSize fe) (feCell fe))
  -- likewise for the entry block's check of the constant pool
  _ <- exitBlockNamed "stale" fe d (Named "stale constant pool" (Reenter (feCell fe)))
  -- poll
  -- The load is volatile: another thread sets HpLim, and without volatile
  -- LLVM would hoist the load out of the loop and the poll would never fire.
  hp <- fresh "hplim"
  emit (hp ++ " = load volatile ptr, ptr %hplim.p")
  stopHp <- fresh "stop"
  emit (stopHp ++ " = icmp eq ptr " ++ hp ++ ", null")
  -- the allocation budget: exhausted once it goes negative
  leftA <- ctxField env oAllocLeft
  left <- fresh "alloc.left"
  emit (left ++ " = load i64, ptr " ++ leftA)
  over <- fresh "over"
  emit (over ++ " = icmp slt i64 " ++ left ++ ", 0")
  stop <- fresh "stop"
  emit (stop ++ " = or i1 " ++ stopHp ++ ", " ++ over)
  reenter <- exitBlock fe d (Reenter (feCell fe))
  bodyLabel <- freshLabel "body"
  when (envStressPoll env) $ do
    left <- ctxField env oStressPollLeft
    n <- fresh "left"
    emit (n ++ " = load i64, ptr " ++ left)
    n' <- fresh "left"
    emit (n' ++ " = sub i64 " ++ n ++ ", 1")
    fire <- fresh "fire"
    emit (fire ++ " = icmp sle i64 " ++ n' ++ ", 0")
    every <- ctxField env oStressPoll
    ev <- fresh "every"
    emit (ev ++ " = load i64, ptr " ++ every)
    reset <- fresh "reset"
    emit (reset ++ " = select i1 " ++ fire ++ ", i64 " ++ ev ++ ", i64 " ++ n')
    emit ("store i64 " ++ reset ++ ", ptr " ++ left)
    stop' <- fresh "stop"
    emit (stop' ++ " = or i1 " ++ stop ++ ", " ++ fire)
    emit ("br i1 " ++ stop' ++ ", label %" ++ reenter ++ ", label %" ++ bodyLabel)
  unless (envStressPoll env) $
    emit ("br i1 " ++ stop ++ ", label %" ++ reenter ++ ", label %" ++ bodyLabel)
  startBlock bodyLabel
  genSection fe d body

-- | Generates a section at frame depth @d@. Every path ends in a terminator.
genSection :: FnEnv -> Int -> MSection -> Gen ()
genSection fe d sect = case sect of
  Ins i rest -> genInstr fe d i sect (\d' -> genSection fe d' rest)
  Yield args -> genYield fe d args sect
  Call _ cix comb args -> genCall fe d cix comb args sect
  App _ r args | on "app" -> genApp fe d r args sect
  Match i br -> do
    x <- loadU (d - i)
    genBranch fe d x br sect
  DMatch mr i br -> genDMatch fe d i mr br sect
  NMatch _ i br -> do
    x <- loadU (d - i)
    genBranch fe d x br sect
  Let binding bcix f body cell -> genLet fe d binding bcix f body cell sect
  _ -> exitResume fe d sect
  where
    on = enabled fe

-- | Whether a feature is on (see Config's @disabled@).
enabled :: FnEnv -> String -> Bool
enabled fe name = name `notElem` envDisabled (feEnv fe)

-- | A @Let@. A binding that is a call to a known function becomes a native
-- call. Other bindings are generated inline: their exits write a frame
-- record for this @Let@, and their @Yield@ delivers the results to the
-- body. If neither works, the interpreter takes the @Let@, and pushes the
-- frame itself.
genLet :: FnEnv -> Int -> MSection -> CombIx -> Int -> MSection -> Ptr NativeCell -> MSection -> Gen ()
genLet fe d binding bcix@(CIx _ _ w) f body cell sect = case EC.lookup w (envCombs env) of
  Just (Comb (LamI bodyArity _ _ _))
    | m <- bodyArity - d,
      m >= 0 -> case binding of
        Call _ _ comb args
          | Comb (LamI arity _ _ ccell) <- unRComb comb,
            let srcs = argSources fe d args,
            length srcs == arity -> do
              bcell <- bodyCell
              ix <- addFrame env (Frame bcix f body bcell)
              genNonTailCall fe d d ccell srcs sect (Just (ix, d)) $ do
                loadResults d m
                genSection fe (d + m) body
        App _ r@(MCode.Env _ comb) args
          | enabled fe "app",
            Comb (LamI arity _ _ ccell) <- unRComb comb,
            let srcs = argSources fe d args,
            length srcs == arity -> do
              bcell <- bodyCell
              ix <- addFrame env (Frame bcix f body bcell)
              genNonTailCall fe d d ccell srcs sect (Just (ix, d)) $ do
                loadResults d m
                genSection fe (d + m) body
          | enabled fe "app",
            ZArgs <- args,
            m == 1,
            Just ix <- combConstant fe r -> do
              poolConstant fe d ix
              genSection fe (d + 1) body
          | otherwise -> exitResume fe d sect
        App _ (Stk i) args | enabled fe "app" -> do
          bcell <- bodyCell
          ix <- addFrame env (Frame bcix f body bcell)
          genClosureCall fe d d (d - i) (argSources fe d args) sect (Just (ix, d)) $ do
            loadResults d m
            genSection fe (d + m) body
        _ | startsSupported binding -> do
              bcell <- bodyCell
              ix <- addFrame env (Frame bcix f body bcell)
              bodyL <- freshLabel "body"
              let fe' = fe {feEnclosing = Enclosing ix d bodyL m : feEnclosing fe}
              ok <- attempt (withKinds (genSection fe' d binding))
              if ok
                then do
                  startBlock bodyL
                  genSection fe (d + m) body
                else exitResume fe d sect
        _ -> exitResume fe d sect
  _ -> exitResume fe d sect
  where
    env = feEnv fe
    -- A Let inside a binding carries no cell (see attachNativeCells): its
    -- body combinator's arity counts the enclosing function's slots, so
    -- the interpreter can't enter it. An auxiliary function generated
    -- here, with the current frame base, can be.
    bodyCell
      | cell /= noNativeCell = pure cell
      | otherwise = case EC.lookup w (envCombs env) of
          Just (Comb (LamI bodyArity _ _ _)) ->
            maybe noNativeCell snd <$> genAuxFunction True fe bodyArity (currentBase fe) body
          _ -> pure noNativeCell

-- | Loads @m@ results left on the stack above offset @base@ into their slots.
loadResults :: Int -> Int -> Gen ()
loadResults base m =
  forM_ [1 .. m] $ \j -> do
    ua <- stackAddrU (base + j)
    u <- fresh "u"
    emit (u ++ " = load i64, ptr " ++ ua)
    storeU (base + j) u
    ba <- stackAddrB (base + j)
    b <- fresh "b"
    emit (b ++ " = load ptr, ptr " ++ ba)
    storeB (base + j) b

-- | A native call that returns here. The callee's frame starts at offset
-- @base@ (the current depth for a @Let@ binding, or an enclosing binding's
-- base for a tail call inside one); its arguments come from the slots
-- @srcs@ at the current depth @d@. On @OK@ the continuation runs with the
-- results on the stack above @base@. Otherwise the slots up to @base@ are
-- written back, the frame record for this @Let@ (if given: index and frame
-- size) and those of the enclosing bindings are written, and the status is
-- passed on. If the callee can't be called, the interpreter resumes at
-- @sect@.
genNonTailCall :: FnEnv -> Int -> Int -> Ptr NativeCell -> [Int] -> MSection -> Maybe (Int, Int) -> Gen () -> Gen ()
genNonTailCall fe d base ccell srcs sect ownFrame continue =
  genNonTailCallWith fe d base (loadCallee (feEnv fe) ccell) (fpPlus (base + length srcs)) srcs sect ownFrame continue

-- | 'genNonTailCall' with the callee given by a generator (the code
-- pointer and an i1 saying the call can't be made), and the callee's
-- stack pointer given by another, run after the arguments are in place
-- (a closure call copies its captured arguments there).
genNonTailCallWith :: FnEnv -> Int -> Int -> Gen (String, String) -> Gen String -> [Int] -> MSection -> Maybe (Int, Int) -> Gen () -> Gen ()
genNonTailCallWith fe d base getCallee getTop srcs sect ownFrame continue = do
  let env = feEnv fe
      n = length srcs
  (fnp, skip) <- getCallee
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  guardL <- freshLabel "guard"
  emit ("br i1 " ++ skip ++ ", label %" ++ slow ++ ", label %" ++ guardL)
  startBlock guardL
  -- the C stack guard: exit instead of calling when the budget is used up
  csp <- fresh "csp"
  emit (csp ++ " = call ptr @llvm.stacksave.p0()")
  cspi <- fresh "csp"
  emit (cspi ++ " = ptrtoint ptr " ++ csp ++ " to i64")
  lim <- ctxField env oCStackLimit
  limv <- fresh "lim"
  emit (limv ++ " = load i64, ptr " ++ lim)
  deep <- fresh "deep"
  emit (deep ++ " = icmp ult i64 " ++ cspi ++ ", " ++ limv)
  callL <- freshLabel "call"
  emit ("br i1 " ++ deep ++ ", label %" ++ slow ++ ", label %" ++ callL)
  startBlock callL
  -- arguments go above the callee's base, as moveArgs would put them
  vals <- loadSources srcs
  forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
    ua <- stackAddrU (base + n - j)
    emit ("store i64 " ++ u ++ ", ptr " ++ ua)
    ba <- stackAddrB (base + n - j)
    emit ("store ptr " ++ b ++ ", ptr " ++ ba)
  bp <- fpPlus base
  top <- getTop
  r <- fresh "r"
  emit (r ++ " = call i64 " ++ fnp ++ "(ptr %ctx, i64 " ++ bp ++ ", i64 " ++ bp ++ ", i64 " ++ top ++ ")")
  ok <- fresh "ok"
  emit (ok ++ " = icmp eq i64 " ++ r ++ ", 0")
  -- the callee is exiting: record the frames the interpreter would have
  -- pushed, and pass the status along
  unwind <- sideBlock "unwind" $ do
    writeFrame base
    forM_ ownFrame $ \(ix, fdepth) -> case feEnclosing fe of
      [] -> writeRecord env ix (fdepth - feBase fe) Nothing
      e : _ -> writeRecord env ix (fdepth - enBase e) (Just "0")
    unwindEnclosing fe
    emit ("ret i64 " ++ r)
  contL <- freshLabel "cont"
  emit ("br i1 " ++ ok ++ ", label %" ++ contL ++ ", label %" ++ unwind)
  startBlock contL
  continue

-- | Runs a generator that may fail. On failure the state is rolled back
-- (except the name counter) and False is returned; nothing was emitted.
attempt :: Gen () -> Gen Bool
attempt g = do
  before <- get
  g
  after <- get
  case gsFailed after of
    Nothing -> pure True
    Just why -> do
      put before {gsFresh = gsFresh after, gsNotes = why : gsNotes after}
      pure False

-- | Branch on an i64 value. @arm@ generates one arm at depth @d@; if it
-- can't (a data match arm that needs fields the native path doesn't
-- push), that arm exits at the whole section instead.
genBranch :: FnEnv -> Int -> String -> GBranch (RComb Val) -> MSection -> Gen ()
genBranch fe d x br sect = genBranchWith fe d x br sect (\_ -> genSection fe d)

-- | 'genBranch' with the code for an arm supplied: it gets the case's
-- constructor tag (or word) and the arm's section.
genBranchWith :: FnEnv -> Int -> String -> GBranch (RComb Val) -> MSection -> (Word64 -> MSection -> Gen ()) -> Gen ()
genBranchWith fe d x br sect armCode = case br of
  Test1 u y n -> do
    c <- fresh "is"
    emit (c ++ " = icmp eq i64 " ++ x ++ ", " ++ signed u)
    ly <- arm "yes" (Just u) y
    ln <- arm "no" Nothing n
    emit ("br i1 " ++ c ++ ", label %" ++ ly ++ ", label %" ++ ln)
  Test2 u cu v cv e -> do
    lu <- arm "case" (Just u) cu
    lv <- arm "case" (Just v) cv
    le <- arm "default" Nothing e
    emit ("switch i64 " ++ x ++ ", label %" ++ le ++ " [ i64 " ++ signed u ++ ", label %" ++ lu ++ "  i64 " ++ signed v ++ ", label %" ++ lv ++ " ]")
  TestW df cs -> do
    ldf <- arm "default" Nothing df
    arms <- forM (EC.mapToList cs) $ \(w, s) -> do
      l <- arm "case" (Just w) s
      pure ("i64 " ++ signed w ++ ", label %" ++ l)
    emit ("switch i64 " ++ x ++ ", label %" ++ ldf ++ " [ " ++ unwords arms ++ " ]")
  _ -> exitResume fe d sect
  where
    -- the default arm has no known constructor, so it runs at depth d
    -- with nothing pushed (the interpreter's dataBranch does the same)
    arm base mu s = do
      label <- freshLabel base
      let code = case mu of
            Just u -> armCode u s
            Nothing -> genSection fe d s
      ok <- attempt (withKinds (sideBlockNamed label code))
      unless ok $ sideBlockNamed label (exitResume fe d sect)
      pure label

signed :: Word64 -> String
signed w = show (fromIntegral w :: Int64)

-- | Branch on the constructor of a data value. Only enumerations (no
-- fields) are handled natively so far; anything else exits.
genDMatch :: FnEnv -> Int -> Int -> Maybe Reference -> GBranch (RComb Val) -> MSection -> Gen ()
genDMatch fe d i mr br sect =
  slotKind (d - i) >>= \case
    Just c -> genBoolBranch fe d c br sect
    Nothing -> genDMatchClosure fe d i mr br sect

-- | A match on a boolean that is still an i1: one branch. False is
-- constructor 0, true is 1.
genBoolBranch :: FnEnv -> Int -> String -> GBranch (RComb Val) -> MSection -> Gen ()
genBoolBranch fe d c br sect = case br of
  Test1 u y n -> two (if u == 1 then (y, n) else (n, y))
  Test2 u cu v cv e -> two (pick 1 [(u, cu), (v, cv)] e, pick 0 [(u, cu), (v, cv)] e)
  TestW df cs -> two (maybe df id (EC.lookup 1 cs), maybe df id (EC.lookup 0 cs))
  _ -> exitResume fe d sect
  where
    pick t alts e = maybe e id (lookup t alts)
    two (t, f) = do
      lt <- arm "true" t
      lf <- arm "false" f
      emit ("br i1 " ++ c ++ ", label %" ++ lt ++ ", label %" ++ lf)
    arm base s = do
      label <- freshLabel base
      ok <- attempt (withKinds (sideBlockNamed label (genSection fe d s)))
      unless ok $ sideBlockNamed label (exitResume fe d sect)
      pure label

-- | A match on a data closure. The pointer tag says which of the four
-- constructor closures it is; each keeps the constructor tag at a
-- different offset, so four small blocks load it and meet at a switch.
-- Each arm then pushes the fields as @dataBranch@ does: the first field
-- on top. How many fields a constructor has comes from the data type's
-- declaration ('envTypes'); one with three or more is a @DataG@ holding
-- two arrays.
genDMatchClosure :: FnEnv -> Int -> Int -> Maybe Reference -> GBranch (RComb Val) -> MSection -> Gen ()
genDMatchClosure fe d i mr br sect = do
  let env = feEnv fe
      ls = envLayouts env
      arities = mr >>= \r -> Map.lookup r (envTypes env)
      -- a type whose arities aren't known (Boolean and the other builtin
      -- references) can still be matched when the value is an enumeration,
      -- which the pointer tag says; the other kinds exit
      kinds = case arities of
        Just _ -> [lEnum ls, lData1 ls, lData2 ls, lDataG ls]
        Nothing -> [lEnum ls]
  p <- loadB (d - i)
  raw <- fresh "raw"
  emit (raw ++ " = ptrtoint ptr " ++ p ++ " to i64")
  tagBits <- fresh "ptrtag"
  emit (tagBits ++ " = and i64 " ++ raw ++ ", 7")
  base <- fresh "base"
  emit (base ++ " = sub i64 " ++ raw ++ ", " ++ tagBits)
  other <- exitBlock fe d (Resume (feCix fe) sect)
  dispatch <- freshLabel "dispatch"
  -- one block per closure kind, loading the packed tag from its offset
  loads <- forM kinds $ \layout -> do
    l <- freshLabel "kind"
    packed <- fresh "packed"
    sideBlockNamed l $ do
      addr <- fresh "tag.a"
      emit (addr ++ " = add i64 " ++ base ++ ", " ++ show (lFieldOffset layout (lPtrs layout)))
      ptr <- fresh "tag.p"
      emit (ptr ++ " = inttoptr i64 " ++ addr ++ " to ptr")
      emit (packed ++ " = load i64, ptr " ++ ptr)
      emit ("br label %" ++ dispatch)
    pure (layout, l, packed)
  emit ("switch i64 " ++ tagBits ++ ", label %" ++ other ++ " [ " ++ unwords [" i64 " ++ show (lPtrTag layout) ++ ", label %" ++ l | (layout, l, _) <- loads] ++ " ]")
  startBlock dispatch
  packed <- fresh "packed"
  emit (packed ++ " = phi i64 " ++ commas ["[ " ++ v ++ ", %" ++ l ++ " ]" | (_, l, v) <- loads])
  tag <- fresh "tag"
  emit (tag ++ " = and i64 " ++ packed ++ ", 65535") -- maskTags
  -- an arm for constructor u knows its field count, so it knows the kind
  let arm u body = case arities of
        Nothing -> genSection fe d body
        Just as -> case lookup (fromIntegral u) (zip [0 :: Int ..] as) of
          Nothing -> exitResume fe d sect
          Just 0 -> genSection fe d body
          Just n -> do
            pushFields fe d base n other
            genSection fe (d + n) body
  genBranchWith fe d tag br sect arm
  where
    commas = foldr1 (\a b -> a ++ ", " ++ b)

-- | Pushes the @n@ fields of the constructor closure at untagged address
-- @base@ onto slots d+1..d+n, first field on top. A @DataG@ whose
-- segment boxes aren't evaluated branches to @slow@.
pushFields :: FnEnv -> Int -> String -> Int -> String -> Gen ()
pushFields fe d base n slow = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
      loadFrom from what ty off = do
        a <- fresh (what ++ ".a")
        emit (a ++ " = add i64 " ++ from ++ ", " ++ show off)
        pp <- fresh (what ++ ".p")
        emit (pp ++ " = inttoptr i64 " ++ a ++ " to ptr")
        v <- fresh what
        emit (v ++ " = load " ++ ty ++ ", ptr " ++ pp)
        pure v
      loadAt = loadFrom base
      -- field j of Data1/Data2: pointer j+1 and non-pointer j+1
      field layout j = do
        b <- loadAt "fb" "ptr" (lFieldOffset layout (j + 1))
        u <- loadAt "fu" "i64" (lFieldOffset layout (lPtrs layout + j + 1))
        pure (u, b)
  case n of
    1 -> do
      (u, b) <- field (lData1 ls) 0
      storeU (d + 1) u >> storeB (d + 1) b
    2 -> do
      (u0, b0) <- field (lData2 ls) 0
      (u1, b1) <- field (lData2 ls) 1
      storeU (d + 2) u0 >> storeB (d + 2) b0
      storeU (d + 1) u1 >> storeB (d + 1) b1
    _ -> do
      -- DataG: the Seg's two fields are lifted boxes (ByteArray, Array)
      -- around the arrays, since a tuple's fields can't be unpacked. Each
      -- box is an evaluated single-constructor object: untag it and read
      -- its one field, the array. The arrays hold the fields in reverse,
      -- so element j goes to slot d+1+j.
      let layout = lDataG ls
          unbox what off = do
            box <- loadAt what "ptr" (lFieldOffset layout off)
            boxI <- fresh (what ++ ".box")
            emit (boxI ++ " = ptrtoint ptr " ++ box ++ " to i64")
            boxTag <- fresh (what ++ ".tag")
            emit (boxTag ++ " = and i64 " ++ boxI ++ ", 7")
            boxBad <- fresh (what ++ ".lazy")
            emit (boxBad ++ " = icmp ne i64 " ++ boxTag ++ ", 1")
            branchIf what boxBad slow
            untagged <- fresh (what ++ ".un")
            emit (untagged ++ " = and i64 " ++ boxI ++ ", -8")
            loadFrom untagged what "i64" (lHeaderBytes ls)
      usegI <- unbox "useg" 1
      bsegI <- unbox "bseg" 2
      forM_ [0 .. n - 1] $ \j -> do
        u <- loadFrom usegI "fu" "i64" (8 * (rBytesHeader rts + j))
        b <- loadFrom bsegI "fb" "ptr" (8 * (rPtrsHeader rts + j))
        storeU (d + 1 + j) u >> storeB (d + 1 + j) b
-- | The slots that an argument list selects, top first, as frame offsets.
-- @VArgV@ means "everything in the frame above index i", and the frame
-- the interpreter sees is the innermost inline binding's, if any.
argSources :: FnEnv -> Int -> Args -> [Int]
argSources fe d = \case
  ZArgs -> []
  VArg1 i -> [d - i]
  VArg2 i j -> [d - i, d - j]
  VArgR i l -> [d - i - k | k <- [0 .. l - 1]]
  VArgN v -> [d - i | i <- primArrayToList v]
  VArgV i -> [d - k | k <- [0 .. (d - base) - i - 1]]
  where
    base = currentBase fe

-- | Loads the selected values into registers (a parallel move must read everything first).
loadSources :: [Int] -> Gen [(String, String)]
loadSources ks = forM ks $ \k -> (,) <$> loadU k <*> loadB k

-- | Return: move the results into place as @moveArgs@ then @frameArgs@ would,
-- and hand them to the continuation.
genYield :: FnEnv -> Int -> Args -> MSection -> Gen ()
genYield fe d args _sect
  | e : _ <- feEnclosing fe = do
      -- inside an inline binding: the results go to the body
      let srcs = argSources fe d args
          n = length srcs
      if n /= enResults e
        then modify' (\s -> s {gsFailed = Just "binding yields the wrong number of values"})
        else do
          vals <- loadSources srcs
          forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
            storeU (enBase e + n - j) u
            storeB (enBase e + n - j) b
          emit ("br label %" ++ enBody e)
genYield fe d args sect = do
  let env = feEnv fe
      base = feBase fe
  -- pending arguments (fp /= ap) mean over-application; leave that to the interpreter
  pending <- fresh "pending"
  emit (pending ++ " = icmp ne i64 %ap, %fpb")
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  fast <- freshLabel "yield"
  emit ("br i1 " ++ pending ++ ", label %" ++ slow ++ ", label %" ++ fast)
  startBlock fast
  let srcs = argSources fe d args
      n = length srcs
  vals <- loadSources srcs
  forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
    ua <- stackAddrU (base + n - j)
    emit ("store i64 " ++ u ++ ", ptr " ++ ua)
    ba <- stackAddrB (base + n - j)
    emit ("store ptr " ++ b ++ ", ptr " ++ ba)
  ap <- ctxField env oAp
  emit ("store i64 %ap, ptr " ++ ap)
  fp <- ctxField env oFp
  emit ("store i64 %ap, ptr " ++ fp) -- frameArgs: fp = ap
  sp <- ctxField env oSp
  f <- fpPlus (base + n)
  emit ("store i64 " ++ f ++ ", ptr " ++ sp)
  emit "ret i64 0"

-- | A tail call: to this function (a loop) or to another one through its cell.
genCall :: FnEnv -> Int -> CombIx -> RComb Val -> Args -> MSection -> Gen ()
genCall fe d cix comb args sect
  | e : _ <- feEnclosing fe = case unRComb comb of
      -- a tail call inside an inline binding is a call that returns to the body
      Comb (LamI arity _ _ ccell)
        | let srcs = argSources fe d args,
          length srcs == arity ->
            genNonTailCall fe d (enBase e) ccell srcs sect Nothing $ do
              loadResults (enBase e) (enResults e)
              emit ("br label %" ++ enBody e)
      _ -> exitResume fe d sect
  | cix == feCix fe,
    Just headL <- feHead fe = do
      let srcs = argSources fe d args
          n = length srcs
      if n /= feArity fe
        then exitResume fe d sect
        else do
          vals <- loadSources srcs
          forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
            storeU (n - j) u
            storeB (n - j) b
          emit ("br label %" ++ headL)
  | otherwise = case unRComb comb of
      Comb (LamI arity _ _ cell) -> do
        let srcs = argSources fe d args
            n = length srcs
            base = feBase fe
        if n /= arity
          then exitResume fe d sect
          else do
            (fnp, isNull) <- loadCallee env cell
            slow <- exitBlock fe d (Resume (feCix fe) sect)
            go <- freshLabel "tail"
            emit ("br i1 " ++ isNull ++ ", label %" ++ slow ++ ", label %" ++ go)
            startBlock go
            vals <- loadSources srcs
            forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
              ua <- stackAddrU (base + n - j)
              emit ("store i64 " ++ u ++ ", ptr " ++ ua)
              ba <- stackAddrB (base + n - j)
              emit ("store ptr " ++ b ++ ", ptr " ++ ba)
            f <- fpPlus (base + n)
            r <- fresh "r"
            emit (r ++ " = musttail call i64 " ++ fnp ++ "(ptr %ctx, i64 %ap, i64 %fpb, i64 " ++ f ++ ")")
            emit ("ret i64 " ++ r)
      _ -> exitResume fe d sect
  where
    env = feEnv fe

-- | A call to a function value (@App@). A known combinator used as a value
-- (@Env@) with the right number of arguments is a plain call. A value on
-- the stack (@Stk@) is called through its closure, see 'genClosureCall'.
-- Anything else (a dynamic-scope reference, a mismatched arity) is left
-- to the interpreter.
genApp :: FnEnv -> Int -> MRef -> Args -> MSection -> Gen ()
genApp fe d r args sect = case r of
  MCode.Env cix comb
    | Comb (LamI arity _ _ _) <- unRComb comb,
      length (argSources fe d args) == arity ->
        genCall fe d cix comb args sect
    -- the combinator as a value: a constant closure, returned
    | ZArgs <- args,
      Just ix <- combConstant fe r -> do
        poolConstant fe d ix
        genYield fe (d + 1) (VArg1 0) sect
  Stk i
    | e : _ <- feEnclosing fe ->
        genClosureCall fe d (enBase e) (d - i) (argSources fe d args) sect Nothing $ do
          loadResults (enBase e) (enResults e)
          emit ("br label %" ++ enBody e)
    | otherwise -> do
        let srcs = argSources fe d args
            base = feBase fe
            n = length srcs
        -- pending arguments would be applied to the result; the interpreter's job
        pending <- fresh "pending"
        emit (pending ++ " = icmp ne i64 %ap, %fpb")
        (fnp, skip0) <- closureCallee fe d (d - i) n
        skip <- fresh "skip"
        emit (skip ++ " = or i1 " ++ skip0 ++ ", " ++ pending)
        slow <- exitBlock fe d (Resume (feCix fe) sect)
        go <- freshLabel "tail"
        emit ("br i1 " ++ skip ++ ", label %" ++ slow ++ ", label %" ++ go)
        startBlock go
        vals <- loadSources srcs
        forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
          ua <- stackAddrU (base + n - j)
          emit ("store i64 " ++ u ++ ", ptr " ++ ua)
          ba <- stackAddrB (base + n - j)
          emit ("store ptr " ++ b ++ ", ptr " ++ ba)
        top <- copyCaptured fe (base + n)
        r' <- fresh "r"
        emit (r' ++ " = musttail call i64 " ++ fnp ++ "(ptr %ctx, i64 %ap, i64 %fpb, i64 " ++ top ++ ")")
        emit ("ret i64 " ++ r')
  _ -> exitResume fe d sect

-- | The pool index of a known combinator's closure (an @App@ with no
-- arguments of a combinator with some arity), if it has one.
combConstant :: FnEnv -> MRef -> Maybe Int
combConstant fe = \case
  MCode.Env cix comb
    | Comb info@(LamI arity _ _ _) <- unRComb comb,
      arity > 0 ->
        Map.lookup (KeyComb cix info) (envPool (feEnv fe))
  _ -> Nothing

-- | A non-tail call to the closure in slot @k@ with the given argument
-- slots; the callee's frame starts at @base@. See 'genNonTailCall'.
genClosureCall :: FnEnv -> Int -> Int -> Int -> [Int] -> MSection -> Maybe (Int, Int) -> Gen () -> Gen ()
genClosureCall fe d base k srcs sect ownFrame continue =
  genNonTailCallWith fe d base (closureCallee fe d k (length srcs)) (copyCaptured fe (base + length srcs)) srcs sect ownFrame continue

-- | Examines the closure in slot @k@ for a call with @n@ supplied
-- arguments: it must be a @PAp@ whose arity is @n@ plus its captured
-- arguments, with compiled code. Gives the code pointer and an i1 that is
-- true when the call can't be made natively. Leaves the captured count in
-- @%cap.n@ and the segment arrays in @%cap.u@ and @%cap.b@ for
-- 'copyCaptured' (registers named per call site through the suffix).
closureCallee :: FnEnv -> Int -> Int -> Int -> Gen (String, String)
closureCallee fe _d k n = do
  let env = feEnv fe
      ls = envLayouts env
      layout = lPAp ls
      rts = envRts env
      loadFrom from what ty off = do
        a <- fresh (what ++ ".a")
        emit (a ++ " = add i64 " ++ from ++ ", " ++ show off)
        pp <- fresh (what ++ ".p")
        emit (pp ++ " = inttoptr i64 " ++ a ++ " to ptr")
        v <- fresh what
        emit (v ++ " = load " ++ ty ++ ", ptr " ++ pp)
        pure v
  p <- loadB k
  raw <- fresh "raw"
  emit (raw ++ " = ptrtoint ptr " ++ p ++ " to i64")
  tagBits <- fresh "ptrtag"
  emit (tagBits ++ " = and i64 " ++ raw ++ ", 7")
  notPAp <- fresh "notpap"
  emit (notPAp ++ " = icmp ne i64 " ++ tagBits ++ ", " ++ show (lPtrTag layout))
  base <- fresh "pap"
  emit (base ++ " = and i64 " ++ raw ++ ", -8")
  -- The loads below are only valid for a PAp, but any closure has at
  -- least the header, and the payload words read are within the
  -- allocation of a PAp; for another kind they may read past its
  -- payload, so they are guarded by a branch.
  okL <- freshLabel "pap"
  joinL <- freshLabel "pap.join"
  cur <- gets (fst . gsCur)
  emit ("br i1 " ++ notPAp ++ ", label %" ++ joinL ++ ", label %" ++ okL)
  startBlock okL
  arity <- loadFrom base "arity" "i64" (lFieldOffset layout (lPtrs layout + 2))
  cellA <- loadFrom base "cell" "i64" (lFieldOffset layout (lPtrs layout + 4))
  cellP <- fresh "cell.p"
  emit (cellP ++ " = inttoptr i64 " ++ cellA ++ " to ptr")
  fnp0 <- fresh "fn"
  emit (fnp0 ++ " = load ptr, ptr " ++ cellP)
  isNull <- fresh "isnull"
  emit (isNull ++ " = icmp eq ptr " ++ fnp0 ++ ", null")
  -- the segment's boxes must be evaluated (pointer tag 1) to be read
  -- through; a lazily built one is left to the interpreter
  let unbox what off = do
        box <- loadFrom base what "ptr" (lFieldOffset layout off)
        boxI <- fresh (what ++ ".box")
        emit (boxI ++ " = ptrtoint ptr " ++ box ++ " to i64")
        boxTag <- fresh (what ++ ".tag")
        emit (boxTag ++ " = and i64 " ++ boxI ++ ", 7")
        boxBad <- fresh (what ++ ".lazy")
        emit (boxBad ++ " = icmp ne i64 " ++ boxTag ++ ", 1")
        untagged <- fresh (what ++ ".un")
        emit (untagged ++ " = and i64 " ++ boxI ++ ", -8")
        arr <- loadFrom untagged what "i64" (lHeaderBytes ls)
        pure (arr, boxBad)
  (useg, usegBad) <- unbox "useg" 2
  (bseg, bsegBad) <- unbox "bseg" 3
  count <- loadFrom bseg "count" "i64" (8 * rPtrsCount rts)
  need <- fresh "need"
  emit (need ++ " = add i64 " ++ count ++ ", " ++ show n)
  wrong <- fresh "wrong"
  emit (wrong ++ " = icmp ne i64 " ++ arity ++ ", " ++ need)
  -- the captured arguments must fit on the stack (the callee checks its own frame)
  top <- fresh "top"
  emit (top ++ " = add i64 %fpb, " ++ need)
  limit <- fresh "limit"
  emit (limit ++ " = add i64 " ++ top ++ ", 1")
  noRoom <- fresh "noroom"
  emit (noRoom ++ " = icmp sge i64 " ++ limit ++ ", %stack.size")
  bad0 <- fresh "bad"
  emit (bad0 ++ " = or i1 " ++ isNull ++ ", " ++ wrong)
  bad1 <- fresh "bad"
  emit (bad1 ++ " = or i1 " ++ bad0 ++ ", " ++ noRoom)
  bad2 <- fresh "bad"
  emit (bad2 ++ " = or i1 " ++ bad1 ++ ", " ++ usegBad)
  bad <- fresh "bad"
  emit (bad ++ " = or i1 " ++ bad2 ++ ", " ++ bsegBad)
  emit ("br label %" ++ joinL)
  startBlock joinL
  skip0 <- fresh "skip"
  emit (skip0 ++ " = phi i1 [ true, %" ++ cur ++ " ], [ " ++ bad ++ ", %" ++ okL ++ " ]")
  fnp <- fresh "fn"
  emit (fnp ++ " = phi ptr [ null, %" ++ cur ++ " ], [ " ++ fnp0 ++ ", %" ++ okL ++ " ]")
  usegR <- fresh "cap.u"
  emit (usegR ++ " = phi i64 [ 0, %" ++ cur ++ " ], [ " ++ useg ++ ", %" ++ okL ++ " ]")
  bsegR <- fresh "cap.b"
  emit (bsegR ++ " = phi i64 [ 0, %" ++ cur ++ " ], [ " ++ bseg ++ ", %" ++ okL ++ " ]")
  countR <- fresh "cap.n"
  emit (countR ++ " = phi i64 [ 0, %" ++ cur ++ " ], [ " ++ count ++ ", %" ++ okL ++ " ]")
  modify' (\st -> st {gsCaptured = Just (usegR, bsegR, countR)})
  skip <-
    if envStressCallee env
      then do
        fire <- stressFire env oStressCalleeLeft oStressCallee
        sk <- fresh "skip"
        emit (sk ++ " = or i1 " ++ skip0 ++ ", " ++ fire)
        pure sk
      else pure skip0
  pure (fnp, skip)

-- | Copies the captured arguments left by 'closureCallee' to the slots
-- above frame offset @from@, as @dumpSeg@ does, and gives the register
-- holding the resulting stack pointer. Marks the high-water mark, since
-- the count isn't static.
copyCaptured :: FnEnv -> Int -> Gen String
copyCaptured fe from = do
  let env = feEnv fe
      rts = envRts env
  (useg, bseg, count) <- gets gsCaptured >>= \case
    Just c -> pure c
    Nothing -> error "copyCaptured: no closure examined"
  modify' (\st -> st {gsCaptured = Nothing})
  cur <- gets (fst . gsCur)
  headL <- freshLabel "cp.head"
  bodyL <- freshLabel "cp.body"
  endL <- freshLabel "cp.end"
  fromR <- fpPlus from
  emit ("br label %" ++ headL)
  startBlock headL
  k <- fresh "k"
  k1 <- fresh "k"
  emit (k ++ " = phi i64 [ 0, %" ++ cur ++ " ], [ " ++ k1 ++ ", %" ++ bodyL ++ " ]")
  done <- fresh "done"
  emit (done ++ " = icmp eq i64 " ++ k ++ ", " ++ count)
  emit ("br i1 " ++ done ++ ", label %" ++ endL ++ ", label %" ++ bodyL)
  startBlock bodyL
  -- element k of each array goes to slot from + 1 + k
  ui <- fresh "ui"
  emit (ui ++ " = add i64 " ++ k ++ ", " ++ show (rBytesHeader rts))
  ua <- fresh "ua"
  emit (ua ++ " = inttoptr i64 " ++ useg ++ " to ptr")
  up <- fresh "up"
  emit (up ++ " = getelementptr i64, ptr " ++ ua ++ ", i64 " ++ ui)
  u <- fresh "u"
  emit (u ++ " = load i64, ptr " ++ up)
  bi <- fresh "bi"
  emit (bi ++ " = add i64 " ++ k ++ ", " ++ show (rPtrsHeader rts))
  ba <- fresh "ba"
  emit (ba ++ " = inttoptr i64 " ++ bseg ++ " to ptr")
  bp <- fresh "bp"
  emit (bp ++ " = getelementptr ptr, ptr " ++ ba ++ ", i64 " ++ bi)
  b <- fresh "b"
  emit (b ++ " = load ptr, ptr " ++ bp)
  slot <- fresh "slot"
  emit (slot ++ " = add i64 " ++ fromR ++ ", " ++ k)
  slot1 <- fresh "slot"
  emit (slot1 ++ " = add i64 " ++ slot ++ ", 1")
  dstU <- fresh "ua"
  emit (dstU ++ " = getelementptr i64, ptr %ustk, i64 " ++ slot1)
  emit ("store i64 " ++ u ++ ", ptr " ++ dstU)
  dstB <- fresh "ba"
  emit (dstB ++ " = getelementptr ptr, ptr %bstk, i64 " ++ slot1)
  emit ("store ptr " ++ b ++ ", ptr " ++ dstB)
  emit (k1 ++ " = add i64 " ++ k ++ ", 1")
  emit ("br label %" ++ headL)
  startBlock endL
  top <- fresh "top"
  emit (top ++ " = add i64 " ++ fromR ++ ", " ++ count)
  -- the high-water mark for card marking on return
  maxA <- ctxField env oMaxSp
  old <- fresh "maxsp"
  emit (old ++ " = load i64, ptr " ++ maxA)
  gt <- fresh "maxsp.gt"
  emit (gt ++ " = icmp sgt i64 " ++ top ++ ", " ++ old)
  new <- fresh "maxsp"
  emit (new ++ " = select i1 " ++ gt ++ ", i64 " ++ top ++ ", i64 " ++ old)
  emit ("store i64 " ++ new ++ ", ptr " ++ maxA)
  pure top

-- ---------------------------------------------------------------------------
-- Instructions

litSupported :: MLit -> Bool
litSupported = \case
  MI _ -> True
  MN _ -> True
  MC _ -> True
  MD _ -> True
  _ -> False

prim1Supported :: Prim1 -> Bool
prim1Supported op = op `elem` [DECI, DECN, INCI, INCN, NEGI, COMN, COMI, TRNC, SGNI]

prim2Supported :: Prim2 -> Bool
prim2Supported op =
  op
    `elem` [ ADDI, SUBI, MULI, DIVI, MODI, EQLI, NEQI, LEQI, LESI, ANDI, IORI, XORI, SHLI, SHRI,
             ADDN, SUBN, MULN, DIVN, MODN, EQLN, NEQN, LEQN, LESN, ANDN, IORN, XORN, SHLN, SHRN, DRPN
           ]

-- | An instruction at depth @d@; continues with the depth after it.
genInstr :: FnEnv -> Int -> GInstr (RComb Val) -> MSection -> (Int -> Gen ()) -> Gen ()
genInstr fe d instr sect k = case instr of
  Lit l | litSupported l -> do
    let (u, tag) = case l of
          MI i -> (show i, feTagInt fe)
          MN n -> (show (fromIntegral n :: Int64), feTagNat fe)
          MC c -> (show (ord c), feTagChar fe)
          MD x -> (show (fromIntegral (castDoubleToWord64 x) :: Int64), feTagFloat fe)
          _ -> error "unreachable"
    storeU (d + 1) u
    storeB (d + 1) tag
    k (d + 1)
  -- boxed literals and constructors without fields are pool constants
  Lit l | Just ix <- Map.lookup (KeyLit l) (envPool (feEnv fe)) -> do
    poolConstant fe d ix
    k (d + 1)
  Pack r t ZArgs | Just ix <- Map.lookup (KeyEnum r t) (envPool (feEnv fe)) -> do
    poolConstant fe d ix
    k (d + 1)
  Pack r t args
    | Just refIx <- Map.lookup (KeyEnum r (PackedTag 0)) (envPool (feEnv fe)),
      Just fields <- packFields fe d args -> do
        genPack fe d refIx t fields
        k (d + 1)
  Prim1 op i | prim1Supported op -> do
    x <- loadU (d - i)
    genPrim1 fe d op x sect
    k (d + 1)
  Prim1 REFR i | enabled fe "ref" -> do
    genRefRead fe d (d - i) instr sect
    k (d + 1)
  Prim2 REFW i j
    | enabled fe "ref",
      Just unitIx <- Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe)) -> do
        genRefWrite fe d (d - i) (d - j) unitIx instr sect
        k (d + 1)
  Prim2 op i j | enabled fe "cmp", op `elem` [EQLU, LEQU, LESU, CMPU] -> do
    genUniversal fe d op (d - i) (d - j) instr sect
    k (d + 1)
  ForeignCall _ MutableArray_size args
    | enabled fe "array", [ka] <- argSources fe d args -> do
        genArrayOp fe d ka Nothing Nothing instr sect
        k (d + 1)
  ForeignCall _ MutableArray_read args
    | enabled fe "array", [ka, ki] <- argSources fe d args -> do
        genArrayOp fe d ka (Just ki) Nothing instr sect
        k (d + 1)
  ForeignCall _ MutableArray_write args
    | enabled fe "array",
      [ka, ki, kv] <- argSources fe d args,
      Just unitIx <- Map.lookup (KeyEnum Ty.unitRef TT.unitTag) (envPool (feEnv fe)) -> do
        genArrayOp fe d ka (Just ki) (Just (kv, unitIx)) instr sect
        k (d + 1)
  Prim2 op i j | prim2Supported op -> do
    x <- loadU (d - i)
    y <- loadU (d - j)
    genPrim2 fe d op x y sect
    k (d + 1)
  _ -> genCallOut fe d instr sect

-- | An instruction the generator has no code for: the interpreter runs
-- it, and native code continues after it in an auxiliary function (a
-- re-entry point). Not worth it when the instruction's results are
-- yielded straight away, or when the instruction reshapes the stack; the
-- interpreter then takes the whole section.
genCallOut :: FnEnv -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genCallOut fe d instr sect = do
  l <- callOutExit False fe d instr sect
  emit ("br label %" ++ l)

-- | The exit block for leaving an instruction to the interpreter: a
-- call-out when one is possible, else a resume at the section. Also the
-- slow path of instructions with a native fast path (@slowPath@): the
-- code after a slow path is only needed when the fast path misses, so its
-- re-entry function can be left for later.
callOutExit :: Bool -> FnEnv -> Int -> GInstr (RComb Val) -> MSection -> Gen String
callOutExit slowPath fe d instr sect = case (pushCount instr, sect) of
  (Just n, Ins _ rest)
    | enabled fe "callout", callOutWorthwhile instr rest ->
        genAuxFunction slowPath fe (d + n) (currentBase fe) rest >>= \case
          Nothing -> resume
          Just (_, cell) -> exitBlock fe d (CallOut (feCix fe) instr rest n cell)
  _ -> resume
  where
    resume = exitBlock fe d (Resume (feCix fe) sect)

-- | How many values an instruction leaves on the stack, or Nothing for
-- one that changes the stack in other ways (captures and discards cut it
-- back to a mark). MCode doesn't record this, so the trampoline checks
-- the stack pointer against it before re-entering native code; a wrong
-- entry here costs a resume, not correctness. Every constructor is
-- listed so that a new instruction fails to compile until it is added.
pushCount :: GInstr comb -> Maybe Int
pushCount = \case
  Prim1 LOAD _ -> Just 2 -- a tag and the value
  Prim1 _ _ -> Just 1
  Prim2 TRCE _ _ -> Just 0
  Prim2 THRO _ _ -> Just 0 -- never returns
  Prim2 _ _ _ -> Just 1
  RefCAS {} -> Just 1
  ForeignCall {} -> Just 1
  DLLCall -> Just 1
  SetAff {} -> Just 0
  Capture _ -> Nothing
  Discard _ -> Nothing
  Name _ _ -> Just 1
  Info _ -> Just 0
  Pack {} -> Just 1
  Lit _ -> Just 1
  Print _ -> Just 0
  Reset {} -> Just 0
  InLocal _ -> Just 0
  Fork _ -> Just 1
  Atomically _ -> Just 1
  Seq _ -> Just 1
  TryForce _ -> Just 1
  SandboxingFailure _ -> Nothing
  KeepAlive _ -> Just 0
  NewForeignPtr _ _ -> Just 1
  AddFinalizer _ _ -> Just 0

-- | The slots a @Pack@'s arguments come from, in field order, when the
-- code generator can build the constructor (one or two fields so far).
packFields :: FnEnv -> Int -> Args -> Maybe [Int]
packFields fe d args = case args of
  ZArgs -> Nothing
  _ -> Just (argSources fe d args)

-- | Allocates a constructor with the given fields and pushes it. The
-- reference comes from the pool entry for the type's enumeration
-- constructor 0 (its first field). See docs/jit-m3.md for why each Pack
-- allocates separately.
genPack :: FnEnv -> Int -> Int -> PackedTag -> [Int] -> Gen ()
genPack fe d refIx (PackedTag t) fields = do
  let env = feEnv fe
      ls = envLayouts env
      rts = envRts env
      n = length fields
      (layout, ptrTag) = case n of
        1 -> (lData1 ls, 3)
        2 -> (lData2 ls, 4)
        _ -> (lDataG ls, 5)
      conWords = 1 + lPtrs layout + lNptrs layout
      -- a DataG also gets its two arrays and their boxes, all in one chunk
      cardWords = (n + (1 `shiftL` rCardBits rts) - 1) `shiftR` rCardBits rts
      cardWords' = (cardWords + 7) `div` 8
      bytesWords = rBytesHeader rts + n
      ptrsWords = rPtrsHeader rts + n + cardWords'
      boxWords = 2
      words
        | n <= 2 = conWords
        | otherwise = conWords + 2 * boxWords + bytesWords + ptrsWords
  -- the fields, before the allocation call so nothing is held across it
  vals <- forM fields $ \k -> (,) <$> loadU k <*> loadB k
  -- the Reference: first field of the pool's Enum for this type
  usePool refIx
  ea <- fresh "enum.a"
  emit (ea ++ " = getelementptr ptr, ptr %pool, i64 " ++ show refIx)
  ep <- fresh "enum"
  emit (ep ++ " = load ptr, ptr " ++ ea)
  ei <- fresh "enum"
  emit (ei ++ " = ptrtoint ptr " ++ ep ++ " to i64")
  eb <- fresh "enum.base"
  emit (eb ++ " = and i64 " ++ ei ++ ", -8")
  ra <- fresh "ref.a"
  emit (ra ++ " = add i64 " ++ eb ++ ", " ++ show (lFieldOffset (lEnum ls) 0))
  rp <- fresh "ref.p"
  emit (rp ++ " = inttoptr i64 " ++ ra ++ " to ptr")
  ref <- fresh "ref"
  emit (ref ++ " = load ptr, ptr " ++ rp)
  -- charge the budget and allocate
  leftA <- ctxField env oAllocLeft
  left <- fresh "alloc.left"
  emit (left ++ " = load i64, ptr " ++ leftA)
  left' <- fresh "alloc.left"
  emit (left' ++ " = sub i64 " ++ left ++ ", " ++ show words)
  emit ("store i64 " ++ left' ++ ", ptr " ++ leftA)
  obj <- fresh "obj"
  emit (obj ++ " = call ptr @unison_jit_alloc_words(ptr %ctx, i64 " ++ show words ++ ")")
  let word i = do
        a <- fresh "w"
        emit (a ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 " ++ show i)
        pure a
      payload i = word (1 + i) -- after the header
  hdr <- word 0
  emit ("store i64 " ++ show (lInfo layout) ++ ", ptr " ++ hdr)
  refA <- payload 0
  emit ("store ptr " ++ ref ++ ", ptr " ++ refA)
  tagA <- payload (lPtrs layout)
  emit ("store i64 " ++ show t ++ ", ptr " ++ tagA)
  if n <= 2
    then forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
      ba <- payload (1 + j)
      emit ("store ptr " ++ b ++ ", ptr " ++ ba)
      ua <- payload (lPtrs layout + 1 + j)
      emit ("store i64 " ++ u ++ ", ptr " ++ ua)
    else do
      -- layout of the chunk after the constructor: ByteArray box, Array
      -- box, ByteArray#, Array#. The arrays hold the fields in reverse:
      -- element n-1-j is field j.
      let ubox = conWords
          bbox = ubox + boxWords
          ubytes = bbox + boxWords
          bptrs = ubytes + bytesWords
          storeAt i what = do
            a <- word i
            emit ("store " ++ what ++ ", ptr " ++ a)
          addrOf i = do
            a <- word i
            v <- fresh "addr"
            emit (v ++ " = ptrtoint ptr " ++ a ++ " to i64")
            pure v
          tagged i tg = do
            v <- addrOf i
            tv <- fresh "tagged"
            emit (tv ++ " = or i64 " ++ v ++ ", " ++ show (tg :: Int))
            tp <- fresh "tagged"
            emit (tp ++ " = inttoptr i64 " ++ tv ++ " to ptr")
            pure tp
      -- the ByteArray#
      storeAt ubytes ("i64 " ++ show (rArrWordsInfo rts))
      storeAt (ubytes + rBytesCount rts) ("i64 " ++ show (8 * n))
      -- the Array#: element count, then payload size including the card table
      storeAt bptrs ("i64 " ++ show (rArrPtrsInfo rts))
      storeAt (bptrs + rPtrsCount rts) ("i64 " ++ show n)
      storeAt (bptrs + rPtrsSize rts) ("i64 " ++ show (n + cardWords'))
      forM_ [0 .. cardWords' - 1] $ \c -> storeAt (bptrs + rPtrsHeader rts + n + c) "i64 0"
      forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
        storeAt (ubytes + rBytesHeader rts + (n - 1 - j)) ("i64 " ++ u)
        storeAt (bptrs + rPtrsHeader rts + (n - 1 - j)) ("ptr " ++ b)
      -- the boxes, each pointing at its array
      ua <- word ubytes
      storeAt ubox ("i64 " ++ show (lByteArrayBoxInfo ls))
      storeAt (ubox + 1) ("ptr " ++ ua)
      ba <- word bptrs
      storeAt bbox ("i64 " ++ show (lArrayBoxInfo ls))
      storeAt (bbox + 1) ("ptr " ++ ba)
      -- and the constructor's two Seg fields point at the boxes (tag 1)
      ubp <- tagged ubox 1
      bbp <- tagged bbox 1
      storeAt (1 + 1) ("ptr " ++ ubp)
      storeAt (1 + 2) ("ptr " ++ bbp)
  -- the result is the tagged pointer
  oi <- fresh "obj"
  emit (oi ++ " = ptrtoint ptr " ++ obj ++ " to i64")
  ti <- fresh "tagged"
  emit (ti ++ " = or i64 " ++ oi ++ ", " ++ show (ptrTag :: Int))
  tp <- fresh "tagged"
  emit (tp ++ " = inttoptr i64 " ++ ti ++ " to ptr")
  storeU (d + 1) "-1"
  storeB (d + 1) tp

-- | Loads through a Ref: the closure in slot @k@ must be @Foreign
-- (WrapIORef ref)@, checked by info pointer, and the MutVar#'s content an
-- evaluated @Val@ (pointer tag 1). Anything else branches to @slow@.
-- Gives the MutVar# address and the Val's fields.
refFields :: FnEnv -> Int -> String -> Gen (String, String, String)
refFields fe k slow = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
      check what c = do
        l <- freshLabel what
        emit ("br i1 " ++ c ++ ", label %" ++ slow ++ ", label %" ++ l)
        startBlock l
      loadFrom from what ty off = do
        a <- fresh (what ++ ".a")
        emit (a ++ " = add i64 " ++ from ++ ", " ++ show off)
        pp <- fresh (what ++ ".p")
        emit (pp ++ " = inttoptr i64 " ++ a ++ " to ptr")
        v <- fresh what
        emit (v ++ " = load " ++ ty ++ ", ptr " ++ pp)
        pure v
      -- an object with pointer tag 7 and the given info pointer; gives its untagged address
      object what raw info = do
        tagBits <- fresh "ptrtag"
        emit (tagBits ++ " = and i64 " ++ raw ++ ", 7")
        notTag <- fresh "nottag"
        emit (notTag ++ " = icmp ne i64 " ++ tagBits ++ ", 7")
        check what notTag
        base <- fresh what
        emit (base ++ " = and i64 " ++ raw ++ ", -8")
        i <- loadFrom base (what ++ ".info") "i64" (0 :: Int)
        notInfo <- fresh "notinfo"
        emit (notInfo ++ " = icmp ne i64 " ++ i ++ ", " ++ show info)
        check what notInfo
        pure base
  p <- loadB k
  raw <- fresh "raw"
  emit (raw ++ " = ptrtoint ptr " ++ p ++ " to i64")
  fo <- object "foreign" raw (lForeignInfo ls)
  wrapped <- loadFrom fo "wrap" "i64" (lHeaderBytes ls)
  w <- object "ioref" wrapped (lIORefInfo ls)
  mv <- loadFrom w "mutvar" "i64" (lHeaderBytes ls)
  valp <- loadFrom mv "val" "i64" (8 * rMutVarVar rts)
  vtag <- fresh "ptrtag"
  emit (vtag ++ " = and i64 " ++ valp ++ ", 7")
  notVal <- fresh "notval"
  emit (notVal ++ " = icmp ne i64 " ++ vtag ++ ", " ++ show (lPtrTag (lVal ls)))
  check "val" notVal
  vbase <- fresh "val"
  emit (vbase ++ " = and i64 " ++ valp ++ ", -8")
  b <- loadFrom vbase "vb" "ptr" (lFieldOffset (lVal ls) 0)
  u <- loadFrom vbase "vu" "i64" (lFieldOffset (lVal ls) 1)
  pure (mv, u, b)

-- | @Ref.read@: the Val's fields go to the result slot.
genRefRead :: FnEnv -> Int -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genRefRead fe d k instr sect = do
  slow <- callOutExit True fe d instr sect
  (_, u, b) <- refFields fe k slow
  storeU (d + 1) u
  storeB (d + 1) b

-- | @Ref.write@: a fresh @Val@ holding the value, stored through the
-- runtime's barrier; the result is unit.
genRefWrite :: FnEnv -> Int -> Int -> Int -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genRefWrite fe d k kv unitIx instr sect = do
  let env = feEnv fe
      ls = envLayouts env
      layout = lVal ls
      words = 1 + lPtrs layout + lNptrs layout
  slow <- callOutExit True fe d instr sect
  -- the value, before anything else: the allocation call is a GC-safe
  -- point only for what is on the Unison stack, not for registers
  u <- loadU kv
  b <- loadB kv
  (mv, _, _) <- refFields fe k slow
  leftA <- ctxField env oAllocLeft
  left <- fresh "alloc.left"
  emit (left ++ " = load i64, ptr " ++ leftA)
  left' <- fresh "alloc.left"
  emit (left' ++ " = sub i64 " ++ left ++ ", " ++ show words)
  emit ("store i64 " ++ left' ++ ", ptr " ++ leftA)
  obj <- fresh "obj"
  emit (obj ++ " = call ptr @unison_jit_alloc_words(ptr %ctx, i64 " ++ show words ++ ")")
  hdr <- fresh "w"
  emit (hdr ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 0")
  emit ("store i64 " ++ show (lInfo layout) ++ ", ptr " ++ hdr)
  ba <- fresh "w"
  emit (ba ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 1")
  emit ("store ptr " ++ b ++ ", ptr " ++ ba)
  ua <- fresh "w"
  emit (ua ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 2")
  emit ("store i64 " ++ u ++ ", ptr " ++ ua)
  oi <- fresh "obj"
  emit (oi ++ " = ptrtoint ptr " ++ obj ++ " to i64")
  ti <- fresh "tagged"
  emit (ti ++ " = or i64 " ++ oi ++ ", " ++ show (lPtrTag layout))
  tp <- fresh "tagged"
  emit (tp ++ " = inttoptr i64 " ++ ti ++ " to ptr")
  mvp <- fresh "mutvar.p"
  emit (mvp ++ " = inttoptr i64 " ++ mv ++ " to ptr")
  emit ("call void @unison_jit_write_mutvar(ptr %ctx, ptr " ++ mvp ++ ", ptr " ++ tp ++ ")")
  poolConstant fe d unitIx

-- | Universal comparison (@==@, @<=@, @<@, @compare@ on any type). The
-- fast path: both values unboxed with the same type-tag closure, which is
-- how @Val@'s instances compare them (a word comparison, signed for Int,
-- unsigned for Nat, bitwise for Float and Char). Anything else, including
-- an Int against a Nat, is left to the interpreter through the call-out.
genUniversal :: FnEnv -> Int -> Prim2 -> Int -> Int -> GInstr (RComb Val) -> MSection -> Gen ()
genUniversal fe d op ki kj instr sect = do
  slow <- callOutExit True fe d instr sect
  bi <- loadB ki
  bj <- loadB kj
  ui <- loadU ki
  uj <- loadU kj
  same <- fresh "same"
  emit (same ++ " = icmp eq ptr " ++ bi ++ ", " ++ bj)
  let isTag name = do
        c <- fresh "istag"
        emit (c ++ " = icmp eq ptr " ++ bi ++ ", " ++ name)
        pure c
  isNat <- isTag (feTagNat fe)
  isInt <- isTag (feTagInt fe)
  isOther <- case op of
    EQLU -> do
      c1 <- isTag (feTagChar fe)
      c2 <- isTag (feTagFloat fe)
      o <- fresh "istag"
      emit (o ++ " = or i1 " ++ c1 ++ ", " ++ c2)
      pure o
    _ -> pure "false"
  num <- fresh "isnum"
  emit (num ++ " = or i1 " ++ isNat ++ ", " ++ isInt)
  known <- fresh "known"
  emit (known ++ " = or i1 " ++ num ++ ", " ++ isOther)
  fast <- fresh "fast"
  emit (fast ++ " = and i1 " ++ same ++ ", " ++ known)
  go <- freshLabel "univ"
  emit ("br i1 " ++ fast ++ ", label %" ++ go ++ ", label %" ++ slow)
  startBlock go
  let signedUnsigned s u = do
        cs <- cmp s ui uj
        cu <- cmp u ui uj
        r <- fresh "c"
        emit (r ++ " = select i1 " ++ isInt ++ ", i1 " ++ cs ++ ", i1 " ++ cu)
        pure r
  case op of
    EQLU -> cmp "eq" ui uj >>= resultBool fe d
    LEQU -> signedUnsigned "sle" "ule" >>= resultBool fe d
    LESU -> signedUnsigned "slt" "ult" >>= resultBool fe d
    _ -> do
      -- compare: -1, 0 or 1 as an Int
      lt <- signedUnsigned "slt" "ult"
      eq <- cmp "eq" ui uj
      a <- fresh "r"
      emit (a ++ " = select i1 " ++ eq ++ ", i64 0, i64 1")
      r <- fresh "r"
      emit (r ++ " = select i1 " ++ lt ++ ", i64 -1, i64 " ++ a)
      result fe d (feTagInt fe) r

-- | An object with pointer tag 7 and the given info pointer, else branch
-- to @slow@; gives the untagged address.
taggedObject :: String -> String -> Int -> String -> Gen String
taggedObject what raw info slow = do
  tagBits <- fresh "ptrtag"
  emit (tagBits ++ " = and i64 " ++ raw ++ ", 7")
  notTag <- fresh "nottag"
  emit (notTag ++ " = icmp ne i64 " ++ tagBits ++ ", 7")
  branchIf what notTag slow
  base <- fresh what
  emit (base ++ " = and i64 " ++ raw ++ ", -8")
  ip <- fresh (what ++ ".info.p")
  emit (ip ++ " = inttoptr i64 " ++ base ++ " to ptr")
  i <- fresh (what ++ ".info")
  emit (i ++ " = load i64, ptr " ++ ip)
  notInfo <- fresh "notinfo"
  emit (notInfo ++ " = icmp ne i64 " ++ i ++ ", " ++ show info)
  branchIf what notInfo slow
  pure base

-- | Branches to @slow@ if the condition holds, else continues in a new block.
branchIf :: String -> String -> String -> Gen ()
branchIf what c slow = do
  l <- freshLabel what
  emit ("br i1 " ++ c ++ ", label %" ++ slow ++ ", label %" ++ l)
  startBlock l

-- | A load of the given type at a byte offset from an address held in an i64.
loadAt :: String -> String -> String -> Int -> Gen String
loadAt from what ty off = do
  a <- fresh (what ++ ".a")
  emit (a ++ " = add i64 " ++ from ++ ", " ++ show off)
  pp <- fresh (what ++ ".p")
  emit (pp ++ " = inttoptr i64 " ++ a ++ " to ptr")
  v <- fresh what
  emit (v ++ " = load " ++ ty ++ ", ptr " ++ pp)
  pure v

-- | The fields of the evaluated @Val@ at the tagged address (else @slow@).
valFields :: FnEnv -> String -> String -> Gen (String, String)
valFields fe valp slow = do
  let ls = envLayouts (feEnv fe)
  vtag <- fresh "ptrtag"
  emit (vtag ++ " = and i64 " ++ valp ++ ", 7")
  notVal <- fresh "notval"
  emit (notVal ++ " = icmp ne i64 " ++ vtag ++ ", " ++ show (lPtrTag (lVal ls)))
  branchIf "val" notVal slow
  vbase <- fresh "val"
  emit (vbase ++ " = and i64 " ++ valp ++ ", -8")
  b <- loadAt vbase "vb" "ptr" (lFieldOffset (lVal ls) 0)
  u <- loadAt vbase "vu" "i64" (lFieldOffset (lVal ls) 1)
  pure (u, b)

-- | Allocates a @Val@ holding the value in slot @kv@; gives its tagged
-- address. The value is read before the allocation call.
allocVal :: FnEnv -> Int -> Gen String
allocVal fe kv = do
  let env = feEnv fe
      layout = lVal (envLayouts env)
      words = 1 + lPtrs layout + lNptrs layout
  u <- loadU kv
  b <- loadB kv
  leftA <- ctxField env oAllocLeft
  left <- fresh "alloc.left"
  emit (left ++ " = load i64, ptr " ++ leftA)
  left' <- fresh "alloc.left"
  emit (left' ++ " = sub i64 " ++ left ++ ", " ++ show words)
  emit ("store i64 " ++ left' ++ ", ptr " ++ leftA)
  obj <- fresh "obj"
  emit (obj ++ " = call ptr @unison_jit_alloc_words(ptr %ctx, i64 " ++ show words ++ ")")
  hdr <- fresh "w"
  emit (hdr ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 0")
  emit ("store i64 " ++ show (lInfo layout) ++ ", ptr " ++ hdr)
  ba <- fresh "w"
  emit (ba ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 1")
  emit ("store ptr " ++ b ++ ", ptr " ++ ba)
  ua <- fresh "w"
  emit (ua ++ " = getelementptr i64, ptr " ++ obj ++ ", i64 2")
  emit ("store i64 " ++ u ++ ", ptr " ++ ua)
  oi <- fresh "obj"
  emit (oi ++ " = ptrtoint ptr " ++ obj ++ " to i64")
  ti <- fresh "tagged"
  emit (ti ++ " = or i64 " ++ oi ++ ", " ++ show (lPtrTag layout))
  tp <- fresh "tagged"
  emit (tp ++ " = inttoptr i64 " ++ ti ++ " to ptr")
  pure tp

-- | @MutableArray.size@, @read@ and @write@ (the last two given an index
-- slot, write also a value slot and the pool index of unit). The closure
-- must be @Foreign (WrapMutableArray arr)@ and the index in bounds;
-- otherwise the call-out runs the foreign call, which raises the error.
-- A write does what compiled Haskell does: store, set the dirty info
-- pointer, mark the card.
genArrayOp :: FnEnv -> Int -> Int -> Maybe Int -> Maybe (Int, Int) -> GInstr (RComb Val) -> MSection -> Gen ()
genArrayOp fe d ka mki mwrite instr sect = do
  let ls = envLayouts (feEnv fe)
      rts = envRts (feEnv fe)
  slow <- callOutExit True fe d instr sect
  -- a value to write is boxed up front, before anything is loaded from the heap
  newVal <- traverse (allocVal fe . fst) mwrite
  p <- loadB ka
  raw <- fresh "raw"
  emit (raw ++ " = ptrtoint ptr " ++ p ++ " to i64")
  fo <- taggedObject "foreign" raw (lForeignInfo ls) slow
  wrapped <- loadAt fo "wrap" "i64" (lHeaderBytes ls)
  w <- taggedObject "marray" wrapped (lMutableArrayInfo ls) slow
  arr <- loadAt w "arr" "i64" (lHeaderBytes ls)
  count <- loadAt arr "count" "i64" (8 * rPtrsCount rts)
  case mki of
    Nothing -> result fe d (feTagNat fe) count
    Just ki -> do
      ix <- loadU ki
      oob <- fresh "oob"
      emit (oob ++ " = icmp uge i64 " ++ ix ++ ", " ++ count)
      branchIf "inbounds" oob slow
      ea <- fresh "elem.a"
      emit (ea ++ " = add i64 " ++ ix ++ ", " ++ show (rPtrsHeader rts))
      arrP <- fresh "arr.p"
      emit (arrP ++ " = inttoptr i64 " ++ arr ++ " to ptr")
      ep <- fresh "elem.p"
      emit (ep ++ " = getelementptr i64, ptr " ++ arrP ++ ", i64 " ++ ea)
      case (mwrite, newVal) of
        (Just (_, unitIx), Just v) -> do
          emit ("store ptr " ++ v ++ ", ptr " ++ ep)
          emit ("store i64 " ++ show (rArrPtrsDirtyInfo rts) ++ ", ptr " ++ arrP)
          -- the card table follows the elements; one byte per 2^cardBits elements
          cardIx <- fresh "card"
          emit (cardIx ++ " = lshr i64 " ++ ix ++ ", " ++ show (rCardBits rts))
          cardsA <- fresh "cards"
          emit (cardsA ++ " = add i64 " ++ count ++ ", " ++ show (rPtrsHeader rts))
          cardsP <- fresh "cards.p"
          emit (cardsP ++ " = getelementptr i64, ptr " ++ arrP ++ ", i64 " ++ cardsA)
          cardP <- fresh "card.p"
          emit (cardP ++ " = getelementptr i8, ptr " ++ cardsP ++ ", i64 " ++ cardIx)
          emit ("store i8 1, ptr " ++ cardP)
          poolConstant fe d unitIx
        _ -> do
          valp <- fresh "val"
          emit (valp ++ " = load i64, ptr " ++ ep)
          (u, b) <- valFields fe valp slow
          storeU (d + 1) u
          storeB (d + 1) b

-- | Records that the function uses this pool index.
usePool :: Int -> Gen ()
usePool ix = modify' (\s -> s {gsMaxPool = max (gsMaxPool s) ix})

-- | Pushes a pool entry as a boxed value.
poolConstant :: FnEnv -> Int -> Int -> Gen ()
poolConstant _fe d ix = do
  usePool ix
  a <- fresh "pool.a"
  emit (a ++ " = getelementptr ptr, ptr %pool, i64 " ++ show ix)
  v <- fresh "const"
  emit (v ++ " = load ptr, ptr " ++ a)
  storeU (d + 1) "-1"
  storeB (d + 1) v

-- | Stores an unboxed result with its type tag at depth @d + 1@.
result :: FnEnv -> Int -> String -> String -> Gen ()
result _fe d tag v = storeU (d + 1) v >> storeB (d + 1) tag

-- | Stores a boolean result: a boxed enumeration closure.
resultBool :: FnEnv -> Int -> String -> Gen ()
resultBool _fe d c = setBoolKind (d + 1) c

binop :: String -> String -> String -> Gen String
binop op x y = do
  r <- fresh "r"
  emit (r ++ " = " ++ op ++ " i64 " ++ x ++ ", " ++ y)
  pure r

cmp :: String -> String -> String -> Gen String
cmp op x y = do
  r <- fresh "c"
  emit (r ++ " = icmp " ++ op ++ " i64 " ++ x ++ ", " ++ y)
  pure r

-- | Branches to a resume exit if the condition holds, else continues.
exitIf :: FnEnv -> Int -> MSection -> String -> Gen ()
exitIf fe d sect c = do
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  ok <- freshLabel "ok"
  emit ("br i1 " ++ c ++ ", label %" ++ slow ++ ", label %" ++ ok)
  startBlock ok

genPrim1 :: FnEnv -> Int -> Prim1 -> String -> MSection -> Gen ()
genPrim1 fe d op x _sect = case op of
  DECI -> binop "sub" x "1" >>= int
  DECN -> binop "sub" x "1" >>= nat
  INCI -> binop "add" x "1" >>= int
  INCN -> binop "add" x "1" >>= nat
  NEGI -> binop "sub" "0" x >>= int
  COMN -> binop "xor" x "-1" >>= nat
  COMI -> binop "xor" x "-1" >>= int
  TRNC -> do
    neg <- cmp "slt" x "0"
    r <- fresh "r"
    emit (r ++ " = select i1 " ++ neg ++ ", i64 0, i64 " ++ x)
    nat r
  SGNI -> do
    neg <- cmp "slt" x "0"
    pos <- cmp "sgt" x "0"
    a <- fresh "r"
    emit (a ++ " = select i1 " ++ pos ++ ", i64 1, i64 0")
    r <- fresh "r"
    emit (r ++ " = select i1 " ++ neg ++ ", i64 -1, i64 " ++ a)
    int r
  _ -> error "genPrim1: unsupported"
  where
    int = result fe d (feTagInt fe)
    nat = result fe d (feTagNat fe)

genPrim2 :: FnEnv -> Int -> Prim2 -> String -> String -> MSection -> Gen ()
genPrim2 fe d op x y sect = case op of
  ADDN -> binop "add" x y >>= nat
  SUBN -> binop "sub" x y >>= int -- Nat.sub gives an Int
  MULN -> binop "mul" x y >>= nat
  ANDN -> binop "and" x y >>= nat
  IORN -> binop "or" x y >>= nat
  XORN -> binop "xor" x y >>= nat
  DRPN -> do
    -- if n >= m then 0 else m - n
    c <- cmp "uge" y x
    s <- binop "sub" x y
    r <- fresh "r"
    emit (r ++ " = select i1 " ++ c ++ ", i64 0, i64 " ++ s)
    nat r
  DIVN -> divZero >> binop "udiv" x y >>= nat
  MODN -> divZero >> binop "urem" x y >>= nat
  ADDI -> binop "add" x y >>= int
  SUBI -> binop "sub" x y >>= int
  MULI -> binop "mul" x y >>= int
  ANDI -> binop "and" x y >>= int
  IORI -> binop "or" x y >>= int
  XORI -> binop "xor" x y >>= int
  DIVI -> do
    divZero
    divOverflow
    -- Haskell's div rounds toward negative infinity
    q <- binop "sdiv" x y
    r <- binop "srem" x y
    nz <- cmp "ne" r "0"
    signs <- binop "xor" r y
    neg <- cmp "slt" signs "0"
    adjust <- fresh "adj"
    emit (adjust ++ " = and i1 " ++ nz ++ ", " ++ neg)
    q1 <- binop "sub" q "1"
    res <- fresh "r"
    emit (res ++ " = select i1 " ++ adjust ++ ", i64 " ++ q1 ++ ", i64 " ++ q)
    int res
  MODI -> do
    divZero
    divOverflow
    r <- binop "srem" x y
    nz <- cmp "ne" r "0"
    signs <- binop "xor" r y
    neg <- cmp "slt" signs "0"
    adjust <- fresh "adj"
    emit (adjust ++ " = and i1 " ++ nz ++ ", " ++ neg)
    r1 <- binop "add" r y
    res <- fresh "r"
    emit (res ++ " = select i1 " ++ adjust ++ ", i64 " ++ r1 ++ ", i64 " ++ r)
    int res
  EQLN -> cmp "eq" x y >>= bool
  NEQN -> cmp "ne" x y >>= bool
  LEQN -> cmp "ule" x y >>= bool
  LESN -> cmp "ult" x y >>= bool
  EQLI -> cmp "eq" x y >>= bool
  NEQI -> cmp "ne" x y >>= bool
  LEQI -> cmp "sle" x y >>= bool
  LESI -> cmp "slt" x y >>= bool
  SHLN -> shift "shl" "0" >>= nat
  SHRN -> shift "lshr" "0" >>= nat
  SHLI -> shift "shl" "0" >>= int
  SHRI -> do
    -- an arithmetic shift by 64 or more gives the sign
    sign <- fresh "sign"
    emit (sign ++ " = ashr i64 " ++ x ++ ", 63")
    shift "ashr" sign >>= int
  _ -> error "genPrim2: unsupported"
  where
    int = result fe d (feTagInt fe)
    nat = result fe d (feTagNat fe)
    bool = resultBool fe d
    divZero = cmp "eq" y "0" >>= exitIf fe d sect
    divOverflow = cmp "eq" y "-1" >>= exitIf fe d sect
    -- Haskell rejects negative shifts and gives `big` for shifts of 64 or more
    shift instr big = do
      neg <- cmp "slt" y "0"
      exitIf fe d sect neg
      wide <- cmp "sge" y "64"
      s <- binop instr x y
      r <- fresh "r"
      emit (r ++ " = select i1 " ++ wide ++ ", i64 " ++ big ++ ", i64 " ++ s)
      pure r
