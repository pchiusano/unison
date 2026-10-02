{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Calling into native code. See docs/jit-design.md, "How the interpreter
-- interacts with native code". With the @jit@ flag off, nothing here can
-- be reached, because no cell ever holds code.
module Unison.Runtime.JIT.Native
  ( NativeFn,
    Status,
    FrameRecord (..),
    statusOK,
    statusError,
    enterNative,
    outAp,
    outFp,
    outSp,
    outFrameCount,
    outFrames,
    configureNative,
    ctxLayout,
    rtsFacts,
    probeClosure,
    listInit,
    listCheck,
    listTest,
    textInit,
    textCheck,
    textTest,
    closureInit,
    nameTest,
    hplimValue,
  )
where

import Data.Primitive.Array (MutableArray (..), sizeofMutableArray)
import Data.Primitive.ByteArray (MutableByteArray (..), readByteArray)
import Foreign.Ptr (FunPtr)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.Stack (Closure)

#ifdef UNISON_JIT
import Data.Int (Int64)
import Foreign.Marshal.Alloc (allocaBytes, free)
import Foreign.Ptr (Ptr, WordPtr (..), wordPtrToPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Exts (MutableArray#, MutableByteArray#)
#endif

-- | The uniform signature of compiled code: @(ctx, ap, fp, sp) -> status@.
data NativeFn

type Status = Int

-- | What a native caller writes down when its callee exits: which @Let@
-- body to continue with (an index into the frame table), and the two sizes
-- a @Push@ frame needs.
data FrameRecord = FrameRecord {frIndex :: !Int, frFrameSize :: !Int, frPendingArgs :: !Int}
  deriving (Show)

statusOK, statusError :: Status
statusOK = 0
statusError = -1

-- 'enterNative' leaves what native code hands back besides its status in
-- the spare words past the last slot of the unboxed stack (see
-- 'Unison.Runtime.Stack.nativeOutWords'): nothing is allocated for a round
-- trip. These read them, given the stack and its size in slots.
outAp, outFp, outSp, outFrameCount :: MutableByteArray RealWorld -> Int -> IO Int
outAp ustk size = readByteArray ustk size
outFp ustk size = readByteArray ustk (size + 1)
outSp ustk size = readByteArray ustk (size + 2)
outFrameCount ustk size = readByteArray ustk (size + 3)
{-# INLINE outAp #-}
{-# INLINE outFp #-}
{-# INLINE outSp #-}
{-# INLINE outFrameCount #-}

-- | The frame records, in the order they were written (innermost caller
-- first). Call at most once per entry: a long list is freed as it is read.
outFrames :: MutableByteArray RealWorld -> Int -> IO [FrameRecord]
outFrames = frameRecords

#ifdef UNISON_JIT

foreign import ccall unsafe "unison_jit_enter"
  c_enter ::
    FunPtr NativeFn ->
    MutableByteArray# RealWorld ->
    MutableArray# RealWorld Closure ->
    MutableArray# RealWorld Closure ->
    Int64 ->
    Int64 ->
    Int64 ->
    Int64 ->
    IO Int64

foreign import ccall unsafe "unison_jit_configure" c_configure :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO ()

foreign import ccall unsafe "unison_jit_rts_facts" c_rtsFacts :: Ptr Int64 -> Int64 -> IO Int64

-- | Facts about the runtime that generated code needs: see
-- unison_jit_rts_facts in jit_rt.c for what each entry is.
rtsFacts :: IO [Int]
rtsFacts = allocaBytes (8 * 16) $ \out -> do
  n <- c_rtsFacts out 16
  map fromIntegral <$> mapM (peekElemOff out) [0 .. fromIntegral n - 1]

foreign import ccall unsafe "unison_jit_ctx_layout" c_ctxLayout :: Ptr Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_hplim_value" c_hplimValue :: IO Int64


-- | For tracing: what the poll would see right now.
hplimValue :: IO Int
hplimValue = fromIntegral <$> c_hplimValue

foreign import ccall unsafe "unison_jit_probe" c_probe :: MutableArray# RealWorld Any -> Int64 -> Ptr Int64 -> IO Int64

-- | Runs native code with the given stacks (and their size in slots) and
-- pointers, and returns its status. Where it stopped and the frame records
-- it wrote are read with 'outAp' and the rest.
enterNative ::
  FunPtr NativeFn ->
  MutableByteArray RealWorld ->
  MutableArray RealWorld Closure ->
  MutableArray RealWorld Closure ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO Status
enterNative fn (MutableByteArray ustk) (MutableArray bstk) (MutableArray pool) size ap fp sp = do
  status <- c_enter fn ustk bstk pool (fromIntegral size) (fromIntegral ap) (fromIntegral fp) (fromIntegral sp)
  pure $! fromIntegral status
{-# INLINE enterNative #-}

frameRecords :: MutableByteArray RealWorld -> Int -> IO [FrameRecord]
frameRecords out size = do
  n <- readByteArray out (size + 3) :: IO Int
  if n <= inlineFrames
    then mapM (\i -> FrameRecord <$> readByteArray out (size + 4 + 3 * i) <*> readByteArray out (size + 5 + 3 * i) <*> readByteArray out (size + 6 + 3 * i)) [0 .. n - 1]
    else do
      addr <- readByteArray out (size + 4) :: IO Word
      let buf = wordPtrToPtr (WordPtr addr) :: Ptr Int64
          record i =
            FrameRecord
              <$> (fromIntegral <$> peekElemOff buf (3 * i))
              <*> (fromIntegral <$> peekElemOff buf (3 * i + 1))
              <*> (fromIntegral <$> peekElemOff buf (3 * i + 2))
      fs <- mapM record [0 .. n - 1]
      free buf
      pure fs

-- | Must match UNISON_JIT_INLINE_FRAMES in jit_rt.h.
inlineFrames :: Int
inlineFrames = 8

-- | Passes stress settings to the C side: poll every N entries, treat every
-- Nth callee as uncompiled, C stack budget in bytes (0 for the default),
-- trace. Called once at startup.
configureNative :: Int -> Int -> Int -> Int -> Bool -> IO ()
configureNative pollEvery calleeEvery cstack alloc tr =
  c_configure (fromIntegral pollEvery) (fromIntegral calleeEvery) (fromIntegral cstack) (fromIntegral alloc) (if tr then 1 else 0)

-- | The offsets of the fields of the C @Ctx@, in the order they are
-- declared, and its total size. The code generator uses these.
ctxLayout :: IO ([Int], Int)
ctxLayout = allocaBytes (8 * 32) $ \out -> do
  size <- c_ctxLayout out 32
  offs <- mapM (peekElemOff out) [0 .. 18]
  pure (map fromIntegral offs, fromIntegral size)

-- | Inspects a closure. Element 0 of the array is the sample, the rest are
-- the objects in its pointer fields. Returns the raw probe output: info
-- pointer, pointer tag, closure type, pointer count, non-pointer count,
-- constructor tag, the payload words, then a match index per pointer field.
probeClosure :: MutableArray RealWorld Any -> IO [Int]
probeClosure arr@(MutableArray arr#) = allocaBytes (8 * 64) $ \out -> do
  total <- c_probe arr# (fromIntegral (sizeofMutableArray arr)) out
  vals <- mapM (peekElemOff out) [0 .. 5 + 2 * fromIntegral total]
  pure (map fromIntegral vals)

foreign import ccall unsafe "unison_jit_list_init" c_listInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_list_check" c_listCheck :: MutableArray# RealWorld Any -> IO Int64

foreign import ccall unsafe "unison_jit_list_test" c_listTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> Int64 -> IO Int64

-- | Teaches the C list helpers the constructors of a list from samples
-- (see unison_jit_list_init), given the info pointers of Foreign, Val,
-- Data1 and Data2. 1 if all is well, else the number of the check that failed.
listInit :: MutableArray RealWorld Any -> [Int] -> IO Int
listInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_listInit arr# p

-- | Checks the structure of the list in slot 0: -1 if it isn't what the C
-- helpers assume, else the set of constructors met.
listCheck :: MutableArray RealWorld Any -> IO Int
listCheck (MutableArray arr#) = fromIntegral <$> c_listCheck arr#

-- | Runs a C list helper on slot 0 (see unison_jit_list_test); the result
-- lands in slot 3. False if the helper left the case to the interpreter.
listTest :: MutableArray RealWorld Any -> Int -> Int -> Int -> IO Bool
listTest (MutableArray arr#) op a b = (== 1) <$> c_listTest arr# (fromIntegral op) (fromIntegral a) (fromIntegral b)

foreign import ccall unsafe "unison_jit_text_init" c_textInit :: MutableArray# RealWorld Any -> Ptr Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_text_check" c_textCheck :: MutableArray# RealWorld Any -> IO Int64

foreign import ccall unsafe "unison_jit_text_test" c_textTest :: MutableArray# RealWorld Any -> Int64 -> Int64 -> IO Int64

-- | The same three for the text helpers (see unison_jit_text_init and
-- unison_jit_text_test). 'textTest' returns what the C function does.
textInit :: MutableArray RealWorld Any -> [Int] -> IO Int
textInit (MutableArray arr#) infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  fromIntegral <$> c_textInit arr# p

textCheck :: MutableArray RealWorld Any -> IO Bool
textCheck (MutableArray arr#) = (== 1) <$> c_textCheck arr#

textTest :: MutableArray RealWorld Any -> Int -> Int -> IO Int
textTest (MutableArray arr#) op a = fromIntegral <$> c_textTest arr# (fromIntegral op) (fromIntegral a)

foreign import ccall unsafe "unison_jit_closure_init" c_closureInit :: Ptr Int64 -> IO ()

foreign import ccall unsafe "unison_jit_name_test" c_nameTest :: MutableArray# RealWorld Any -> Int64 -> IO Int64

-- | Gives the partial-application helper the info pointers of PAp and of
-- the boxes around a segment's two arrays.
closureInit :: [Int] -> IO ()
closureInit infos = allocaBytes (8 * length infos) $ \p -> do
  mapM_ (\(i, v) -> pokeElemOff p i (fromIntegral v)) (zip [0 ..] infos)
  c_closureInit p

-- | Runs the helper on slot 0 with n arguments (see unison_jit_name_test).
nameTest :: MutableArray RealWorld Any -> Int -> IO Bool
nameTest (MutableArray arr#) n = (== 1) <$> c_nameTest arr# (fromIntegral n)

#else

enterNative ::
  FunPtr NativeFn ->
  MutableByteArray RealWorld ->
  MutableArray RealWorld Closure ->
  MutableArray RealWorld Closure ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO Status
enterNative _ _ _ _ _ _ _ _ = error "JIT: not built in, but a native code cell holds code"

frameRecords :: MutableByteArray RealWorld -> Int -> IO [FrameRecord]
frameRecords _ _ = pure []

configureNative :: Int -> Int -> Int -> Int -> Bool -> IO ()
configureNative _ _ _ _ _ = pure ()

rtsFacts :: IO [Int]
rtsFacts = pure []

ctxLayout :: IO ([Int], Int)
ctxLayout = pure ([], 0)

probeClosure :: MutableArray RealWorld Any -> IO [Int]
probeClosure _ = pure []

listInit :: MutableArray RealWorld Any -> [Int] -> IO Int
listInit _ _ = pure 0

listCheck :: MutableArray RealWorld Any -> IO Int
listCheck _ = pure (-1)

listTest :: MutableArray RealWorld Any -> Int -> Int -> Int -> IO Bool
listTest _ _ _ _ = pure False

textInit :: MutableArray RealWorld Any -> [Int] -> IO Int
textInit _ _ = pure 0

textCheck :: MutableArray RealWorld Any -> IO Bool
textCheck _ = pure False

textTest :: MutableArray RealWorld Any -> Int -> Int -> IO Int
textTest _ _ _ = pure 0

closureInit :: [Int] -> IO ()
closureInit _ = pure ()

nameTest :: MutableArray RealWorld Any -> Int -> IO Bool
nameTest _ _ = pure False

hplimValue :: IO Int
hplimValue = pure 0


#endif
