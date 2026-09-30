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
    Function (..),
    genFunction,
  )
where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State.Strict
import Data.Char (ord)
import Data.Int (Int64)
import Data.Primitive.PrimArray (primArrayToList)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, WordPtr (..), ptrToWordPtr)
import GHC.Float (castDoubleToWord64)
import Unison.Runtime.JIT.Exits (Exit (..))
import Unison.Runtime.JIT.Layout
import Unison.Runtime.JIT.Pool
import Unison.Runtime.MCode
import Unison.Runtime.Machine.Types (MSection)
import Unison.Runtime.Stack (Val)
import Unison.Util.EnumContainers qualified as EC

-- | Byte offsets of the fields of the C @Ctx@, from @unison_jit_ctx_layout@.
data CtxOffsets = CtxOffsets
  { oUstk, oBstk, oPool, oStackSize, oHplim, oAp, oFp, oSp, oMaxSp, oStressPoll, oStressPollLeft :: !Int
  }

data Env = Env
  { envLayouts :: Layouts,
    envCtx :: CtxOffsets,
    -- | index of this module's first exit in the global table
    envExitBase :: Int,
    -- | emit the stress-mode poll countdown
    envStressPoll :: Bool
  }

data Function = Function
  { fnName :: String,
    fnIR :: String,
    -- | in index order, starting at the module's base plus the count before this function
    fnExits :: [Exit],
    fnCell :: Ptr NativeCell
  }

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
    -- | highest frame offset used
    gsMaxK :: !Int,
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

loadU :: Int -> Gen String
loadU k = do
  useSlot k
  v <- fresh "u"
  emit (v ++ " = load i64, ptr " ++ uSlot k)
  pure v

loadB :: Int -> Gen String
loadB k = do
  useSlot k
  v <- fresh "b"
  emit (v ++ " = load ptr, ptr " ++ bSlot k)
  pure v

storeU :: Int -> String -> Gen ()
storeU k v = useSlot k >> emit ("store i64 " ++ v ++ ", ptr " ++ uSlot k)

storeB :: Int -> String -> Gen ()
storeB k v = useSlot k >> emit ("store ptr " ++ v ++ ", ptr " ++ bSlot k)

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

-- | Adds an exit and returns its global index.
addExit :: Env -> Exit -> Gen Int
addExit env e = do
  n <- gets gsNExits
  modify' (\s -> s {gsExits = e : gsExits s, gsNExits = n + 1})
  pure (envExitBase env + n)

-- | A block that writes the frame back to the Unison stack, records the
-- stack pointers in @Ctx@, and returns the exit's index. @d@ is the frame
-- depth at the exit point; every slot 1..d is written back.
exitBlock :: Env -> Int -> Exit -> Gen String
exitBlock = exitBlockNamed "exit"

exitBlockNamed :: String -> Env -> Int -> Exit -> Gen String
exitBlockNamed base env d e = do
  ix <- addExit env e
  sideBlock base $ do
    forM_ [1 .. d] $ \k -> do
      u <- loadU k
      ua <- stackAddrU k
      emit ("store i64 " ++ u ++ ", ptr " ++ ua)
      b <- loadB k
      ba <- stackAddrB k
      emit ("store ptr " ++ b ++ ", ptr " ++ ba)
    ap <- ctxField env oAp
    emit ("store i64 %ap, ptr " ++ ap)
    fp <- ctxField env oFp
    emit ("store i64 %fp, ptr " ++ fp)
    sp <- ctxField env oSp
    f <- fpPlus d
    emit ("store i64 " ++ f ++ ", ptr " ++ sp)
    emit ("ret i64 " ++ show ix)

-- | Terminates the current block with a resume exit at this section.
exitResume :: Env -> CombIx -> Int -> MSection -> Gen ()
exitResume env cix d sect = do
  l <- exitBlock env d (Resume cix sect)
  emit ("br label %" ++ l)

-- ---------------------------------------------------------------------------
-- Functions

data FnEnv = FnEnv
  { feEnv :: Env,
    feCix :: CombIx,
    feArity :: Int,
    feFrameSize :: Int,
    feCell :: Ptr NativeCell,
    feHead :: String,
    -- | registers holding the type-tag and boolean closures, loaded at entry
    feTagChar, feTagFloat, feTagInt, feTagNat, feTrue, feFalse :: String
  }

-- | Compiles one combinator, or says why it can't be.
genFunction :: Env -> String -> CombIx -> Int -> Int -> MSection -> Ptr NativeCell -> Either String Function
genFunction env name cix arity frameSize body cell
  | not (startsSupported body) = Left "body starts with something the JIT doesn't compile"
  | otherwise =
      let fe = FnEnv env cix arity frameSize cell "head" "%tag.char" "%tag.float" "%tag.int" "%tag.nat" "%val.true" "%val.false"
          gs0 = GS 0 [] ("head", []) [] 0 arity Nothing
          ((), gs) = runState (genHead fe body >> startBlock "unreachable") gs0
          blocks = [b | b@(l, _) <- reverse (gsBlocks gs), l /= "unreachable"]
          maxK = max (gsMaxK gs) (arity + frameSize)
          entry = entryBlock env arity frameSize maxK
          text =
            unlines $
              ["define i64 @" ++ name ++ "(ptr %ctx, i64 %ap, i64 %fp, i64 %sp) {"]
                ++ entry
                ++ concat [(l ++ ":") : map ("  " ++) is | (l, is) <- blocks]
                ++ ["}"]
       in case gsFailed gs of
            Just why -> Left why
            Nothing -> Right (Function name text (reverse (gsExits gs)) cell)

-- The first instruction decides whether compiling is worth anything.
startsSupported :: MSection -> Bool
startsSupported = \case
  Ins (Lit l) _ -> litSupported l
  Ins (Prim1 op _) _ -> prim1Supported op
  Ins (Prim2 op _ _) _ -> prim2Supported op
  Match {} -> True
  DMatch {} -> True
  Call {} -> True
  Yield {} -> True
  _ -> False

-- | The entry block: allocas, addresses from @Ctx@, argument loads, the
-- stack check, then a branch to the loop head.
entryBlock :: Env -> Int -> Int -> Int -> [String]
entryBlock env arity frameSize maxK =
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
      ++ [ "%need = add i64 %sp, " ++ show (frameSize + 1),
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
  -- the entry block's stack check branches to %grow when the frame doesn't fit
  _ <- exitBlockNamed "grow" env d (GrowStack (feFrameSize fe) (feCell fe))
  -- poll
  -- The load is volatile: another thread sets HpLim, and without volatile
  -- LLVM would hoist the load out of the loop and the poll would never fire.
  hp <- fresh "hplim"
  emit (hp ++ " = load volatile ptr, ptr %hplim.p")
  stop <- fresh "stop"
  emit (stop ++ " = icmp eq ptr " ++ hp ++ ", null")
  reenter <- exitBlock env d (Reenter (feCell fe))
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
  DMatch _ i br -> genDMatch fe d i br sect
  _ -> exitResume (feEnv fe) (feCix fe) d sect

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
genBranch fe d x br sect = case br of
  Test1 u y n -> do
    c <- fresh "is"
    emit (c ++ " = icmp eq i64 " ++ x ++ ", " ++ signed u)
    ly <- arm "yes" y
    ln <- arm "no" n
    emit ("br i1 " ++ c ++ ", label %" ++ ly ++ ", label %" ++ ln)
  Test2 u cu v cv e -> do
    lu <- arm "case" cu
    lv <- arm "case" cv
    le <- arm "default" e
    emit ("switch i64 " ++ x ++ ", label %" ++ le ++ " [ i64 " ++ signed u ++ ", label %" ++ lu ++ "  i64 " ++ signed v ++ ", label %" ++ lv ++ " ]")
  TestW df cs -> do
    ldf <- arm "default" df
    arms <- forM (EC.mapToList cs) $ \(w, s) -> do
      l <- arm "case" s
      pure ("i64 " ++ signed w ++ ", label %" ++ l)
    emit ("switch i64 " ++ x ++ ", label %" ++ ldf ++ " [ " ++ unwords arms ++ " ]")
  _ -> exitResume (feEnv fe) (feCix fe) d sect
  where
    arm base s = do
      label <- freshLabel base
      ok <- attempt (sideBlockNamed label (genSection fe d s))
      unless ok $ sideBlockNamed label (exitResume (feEnv fe) (feCix fe) d sect)
      pure label

signed :: Word64 -> String
signed w = show (fromIntegral w :: Int64)

-- | Branch on the constructor of a data value. Only enumerations (no
-- fields) are handled natively so far; anything else exits.
genDMatch :: FnEnv -> Int -> Int -> GBranch (RComb Val) -> MSection -> Gen ()
genDMatch fe d i br sect = do
  let env = feEnv fe
      layout = lEnum (envLayouts env)
  p <- loadB (d - i)
  raw <- fresh "raw"
  emit (raw ++ " = ptrtoint ptr " ++ p ++ " to i64")
  tagBits <- fresh "ptrtag"
  emit (tagBits ++ " = and i64 " ++ raw ++ ", 7")
  isEnum <- fresh "isenum"
  emit (isEnum ++ " = icmp eq i64 " ++ tagBits ++ ", " ++ show (lPtrTag layout))
  other <- exitBlock env d (Resume (feCix fe) sect)
  enumL <- freshLabel "enum"
  emit ("br i1 " ++ isEnum ++ ", label %" ++ enumL ++ ", label %" ++ other)
  startBlock enumL
  base <- fresh "base"
  emit (base ++ " = sub i64 " ++ raw ++ ", " ++ show (lPtrTag layout))
  addr <- fresh "tag.a"
  emit (addr ++ " = add i64 " ++ base ++ ", " ++ show (lFieldOffset layout (lPtrs layout)))
  ptr <- fresh "tag.p"
  emit (ptr ++ " = inttoptr i64 " ++ addr ++ " to ptr")
  packed <- fresh "packed"
  emit (packed ++ " = load i64, ptr " ++ ptr)
  tag <- fresh "tag"
  emit (tag ++ " = and i64 " ++ packed ++ ", 65535") -- maskTags
  genBranch fe d tag br sect

-- | The slots that an argument list selects, top first, as frame offsets.
argSources :: Int -> Args -> [Int]
argSources d = \case
  ZArgs -> []
  VArg1 i -> [d - i]
  VArg2 i j -> [d - i, d - j]
  VArgR i l -> [d - i - k | k <- [0 .. l - 1]]
  VArgN v -> [d - i | i <- primArrayToList v]
  VArgV i -> [d - k | k <- [0 .. d - i - 1]]

-- | Loads the selected values into registers (a parallel move must read everything first).
loadSources :: [Int] -> Gen [(String, String)]
loadSources ks = forM ks $ \k -> (,) <$> loadU k <*> loadB k

-- | Return: move the results into place as @moveArgs@ then @frameArgs@ would,
-- and hand them to the continuation.
genYield :: FnEnv -> Int -> Args -> MSection -> Gen ()
genYield fe d args sect = do
  let env = feEnv fe
  -- pending arguments (fp /= ap) mean over-application; leave that to the interpreter
  pending <- fresh "pending"
  emit (pending ++ " = icmp ne i64 %ap, %fp")
  slow <- exitBlock env d (Resume (feCix fe) sect)
  fast <- freshLabel "yield"
  emit ("br i1 " ++ pending ++ ", label %" ++ slow ++ ", label %" ++ fast)
  startBlock fast
  let srcs = argSources d args
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
  | cix == feCix fe = do
      let srcs = argSources d args
          n = length srcs
      if n /= feArity fe
        then exitResume env (feCix fe) d sect
        else do
          vals <- loadSources srcs
          forM_ (zip [0 ..] vals) $ \(j, (u, b)) -> do
            storeU (n - j) u
            storeB (n - j) b
          emit ("br label %" ++ feHead fe)
  | otherwise = case unRComb comb of
      Comb (LamI arity _ _ cell) -> do
        let srcs = argSources d args
            n = length srcs
        if n /= arity
          then exitResume env (feCix fe) d sect
          else do
            let WordPtr addr = ptrToWordPtr cell
            fnp <- fresh "fn"
            emit (fnp ++ " = load ptr, ptr inttoptr (i64 " ++ show addr ++ " to ptr)")
            isNull <- fresh "isnull"
            emit (isNull ++ " = icmp eq ptr " ++ fnp ++ ", null")
            slow <- exitBlock env d (Resume (feCix fe) sect)
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
      _ -> exitResume env (feCix fe) d sect
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
  Prim1 op i | prim1Supported op -> do
    x <- loadU (d - i)
    genPrim1 fe d op x sect
    k (d + 1)
  Prim2 op i j | prim2Supported op -> do
    x <- loadU (d - i)
    y <- loadU (d - j)
    genPrim2 fe d op x y sect
    k (d + 1)
  _ -> exitResume (feEnv fe) (feCix fe) d sect

-- | Stores an unboxed result with its type tag at depth @d + 1@.
result :: FnEnv -> Int -> String -> String -> Gen ()
result _fe d tag v = storeU (d + 1) v >> storeB (d + 1) tag

-- | Stores a boolean result: a boxed enumeration closure.
resultBool :: FnEnv -> Int -> String -> Gen ()
resultBool fe d c = do
  p <- fresh "bool"
  emit (p ++ " = select i1 " ++ c ++ ", ptr " ++ feTrue fe ++ ", ptr " ++ feFalse fe)
  storeU (d + 1) "-1"
  storeB (d + 1) p

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
  slow <- exitBlock (feEnv fe) d (Resume (feCix fe) sect)
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
