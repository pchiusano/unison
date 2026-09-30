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
    genFunction,
    modulePrelude,
  )
where

import Control.Monad (forM, forM_, unless, when)
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
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IM
import Unison.Runtime.MCode
import Unison.Runtime.Machine.Types (MCombs, MSection)
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
    rCardBits :: !Int
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
    envTypes :: Map.Map Reference [Int]
  }

data Function = Function
  { fnName :: String,
    fnIR :: String,
    -- | in index order, starting at the module's base plus the count before this function
    fnExits :: [Exit],
    -- | likewise for the frame table
    fnFrames :: [Frame],
    fnCell :: Ptr NativeCell
  }

-- | Declarations every module needs.
modulePrelude :: String
modulePrelude = "declare ptr @llvm.stacksave.p0()\ndeclare ptr @unison_jit_alloc_words(ptr, i64)\n"

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
    gsFailed :: Maybe String
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
  label <- if base == "grow" then pure base else freshLabel base
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
    feCix :: CombIx,
    feArity :: Int,
    feFrameSize :: Int,
    feCell :: Ptr NativeCell,
    feHead :: String,
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
frameBase fe = case feEnclosing fe of
  [] -> pure "%fp"
  e : _ -> fpPlus (enBase e)

-- | Writes the frame records for every enclosing inline binding,
-- innermost first (the order a chain of native callers would write them).
unwindEnclosing :: FnEnv -> Gen ()
unwindEnclosing fe = go (feEnclosing fe)
  where
    go [] = pure ()
    go (e : outer) = do
      let (fsz, asz) = case outer of
            [] -> (enBase e, Nothing)
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
      emit (a ++ " = sub i64 %fp, %ap")
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
-- an i1 saying whether the callee must be treated as not compiled.
loadCallee :: Env -> Ptr NativeCell -> Gen (String, String)
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
  | otherwise =
      let fe = FnEnv env cix arity frameSize cell "head" "%tag.char" "%tag.float" "%tag.int" "%tag.nat" []
          gs0 = GS 0 [] ("head", []) [] 0 [] 0 arity IM.empty Nothing
          ((), gs) = runState (genHead fe body >> startBlock "unreachable") gs0
          blocks = [b | b@(l, _) <- reverse (gsBlocks gs), l /= "unreachable"]
          maxK = max (gsMaxK gs) (arity + frameSize)
          entry = entryBlock env arity maxK
          text =
            unlines $
              ["define i64 @" ++ name ++ "(ptr %ctx, i64 %ap, i64 %fp, i64 %sp) {"]
                ++ entry
                ++ concat [(l ++ ":") : map ("  " ++) is | (l, is) <- blocks]
                ++ ["}"]
          -- the grow exit asks for what the entry check demanded
          fixGrow (GrowStack _ c) = GrowStack (maxK - arity) c
          fixGrow e = e
       in case gsFailed gs of
            Just why -> Left why
            Nothing -> Right (Function name text (map fixGrow (reverse (gsExits gs))) (reverse (gsFrames gs)) cell)

-- The first instruction decides whether compiling is worth anything.
startsSupported :: MSection -> Bool
startsSupported = \case
  Ins (Lit _) _ -> True
  Ins (Pack {}) _ -> True
  Ins (Prim1 op _) _ -> prim1Supported op
  Ins (Prim2 op _ _) _ -> prim2Supported op
  Match {} -> True
  DMatch {} -> True
  Call {} -> True
  Yield {} -> True
  Let b _ _ _ _ -> startsSupported b
  _ -> False

-- | The entry block: allocas, addresses from @Ctx@, argument loads, the
-- stack check, then a branch to the loop head. @maxK@ is the highest
-- frame offset the function touches, which is at least the frame size and
-- covers the arguments of every call it makes.
entryBlock :: Env -> Int -> Int -> [String]
entryBlock env arity maxK =
  map ("  " ++) $
    ["%u" ++ show k ++ " = alloca i64" | k <- [1 .. maxK]]
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
  Match i br -> do
    x <- loadU (d - i)
    genBranch fe d x br sect
  DMatch mr i br -> genDMatch fe d i mr br sect
  NMatch _ i br -> do
    x <- loadU (d - i)
    genBranch fe d x br sect
  Let binding bcix f body cell -> genLet fe d binding bcix f body cell sect
  _ -> exitResume fe d sect

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
              ix <- addFrame env (Frame bcix f body cell)
              genNonTailCall fe d d ccell srcs sect (Just (ix, d)) $ do
                loadResults d m
                genSection fe (d + m) body
        _ | startsSupported binding -> do
              ix <- addFrame env (Frame bcix f body cell)
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
genNonTailCall fe d base ccell srcs sect ownFrame continue = do
  let env = feEnv fe
      n = length srcs
  (fnp, skip) <- loadCallee env ccell
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
  top <- fpPlus (base + n)
  r <- fresh "r"
  emit (r ++ " = call i64 " ++ fnp ++ "(ptr %ctx, i64 " ++ bp ++ ", i64 " ++ bp ++ ", i64 " ++ top ++ ")")
  ok <- fresh "ok"
  emit (ok ++ " = icmp eq i64 " ++ r ++ ", 0")
  -- the callee is exiting: record the frames the interpreter would have
  -- pushed, and pass the status along
  unwind <- sideBlock "unwind" $ do
    writeFrame base
    forM_ ownFrame $ \(ix, fdepth) -> case feEnclosing fe of
      [] -> writeRecord env ix fdepth Nothing
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
    Just _ -> do
      put before {gsFresh = gsFresh after}
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
      kinds = [lEnum ls, lData1 ls, lData2 ls, lDataG ls]
      arities = mr >>= \r -> Map.lookup r (envTypes env)
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
  let arm u body = case arities >>= \as -> lookup (fromIntegral u) (zip [0 :: Int ..] as) of
        Nothing -> exitResume fe d sect
        Just 0 -> genSection fe d body
        Just n -> do
          pushFields fe d base n
          genSection fe (d + n) body
  genBranchWith fe d tag br sect arm
  where
    commas = foldr1 (\a b -> a ++ ", " ++ b)

-- | Pushes the @n@ fields of the constructor closure at untagged address
-- @base@ onto slots d+1..d+n, first field on top.
pushFields :: FnEnv -> Int -> String -> Int -> Gen ()
pushFields fe d base n = do
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
    base = case feEnclosing fe of
      [] -> 0
      e : _ -> enBase e

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
  -- pending arguments (fp /= ap) mean over-application; leave that to the interpreter
  pending <- fresh "pending"
  emit (pending ++ " = icmp ne i64 %ap, %fp")
  slow <- exitBlock fe d (Resume (feCix fe) sect)
  fast <- freshLabel "yield"
  emit ("br i1 " ++ pending ++ ", label %" ++ slow ++ ", label %" ++ fast)
  startBlock fast
  let srcs = argSources fe d args
      n = length srcs
  vals <- loadSources srcs
  forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
    ua <- stackAddrU (n - j)
    emit ("store i64 " ++ u ++ ", ptr " ++ ua)
    ba <- stackAddrB (n - j)
    emit ("store ptr " ++ b ++ ", ptr " ++ ba)
  ap <- ctxField env oAp
  emit ("store i64 %ap, ptr " ++ ap)
  fp <- ctxField env oFp
  emit ("store i64 %ap, ptr " ++ fp) -- frameArgs: fp = ap
  sp <- ctxField env oSp
  f <- fpPlus n
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
  | cix == feCix fe = do
      let srcs = argSources fe d args
          n = length srcs
      if n /= feArity fe
        then exitResume fe d sect
        else do
          vals <- loadSources srcs
          forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
            storeU (n - j) u
            storeB (n - j) b
          emit ("br label %" ++ feHead fe)
  | otherwise = case unRComb comb of
      Comb (LamI arity _ _ cell) -> do
        let srcs = argSources fe d args
            n = length srcs
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
              ua <- stackAddrU (n - j)
              emit ("store i64 " ++ u ++ ", ptr " ++ ua)
              ba <- stackAddrB (n - j)
              emit ("store ptr " ++ b ++ ", ptr " ++ ba)
            f <- fpPlus n
            r <- fresh "r"
            emit (r ++ " = musttail call i64 " ++ fnp ++ "(ptr %ctx, i64 %ap, i64 %fp, i64 " ++ f ++ ")")
            emit ("ret i64 " ++ r)
      _ -> exitResume fe d sect
  where
    env = feEnv fe

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
  Prim2 op i j | prim2Supported op -> do
    x <- loadU (d - i)
    y <- loadU (d - j)
    genPrim2 fe d op x y sect
    k (d + 1)
  _ -> exitResume fe d sect

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

-- | Pushes a pool entry as a boxed value.
poolConstant :: FnEnv -> Int -> Int -> Gen ()
poolConstant _fe d ix = do
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
