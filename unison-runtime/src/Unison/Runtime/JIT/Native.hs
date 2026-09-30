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
    configureNative,
    ctxLayout,
    probeClosure,
    hplimValue,
  )
where

import Data.Primitive.Array (MutableArray (..))
import Data.Primitive.ByteArray (MutableByteArray (..))
import Foreign.Ptr (FunPtr)
import GHC.Exts (Any, RealWorld)
import Unison.Runtime.Stack (Closure)

#ifdef UNISON_JIT
import Data.Int (Int64)
import Data.Primitive.Array (sizeofMutableArray)
import Foreign.Marshal.Alloc (allocaBytes, free)
import Foreign.Ptr (Ptr, wordPtrToPtr)
import Foreign.Storable (peekElemOff)
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
    Ptr Int64 ->
    IO Int64

foreign import ccall unsafe "unison_jit_configure" c_configure :: Int64 -> Int64 -> Int64 -> Int64 -> IO ()

foreign import ccall unsafe "unison_jit_ctx_layout" c_ctxLayout :: Ptr Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "unison_jit_hplim_value" c_hplimValue :: IO Int64


-- | For tracing: what the poll would see right now.
hplimValue :: IO Int
hplimValue = fromIntegral <$> c_hplimValue

foreign import ccall unsafe "unison_jit_probe" c_probe :: MutableArray# RealWorld Any -> Int64 -> Ptr Int64 -> IO Int64

-- | Runs native code with the given stacks and pointers. Returns the
-- status, the new @(ap, fp, sp)@, and the frame records in the order they
-- were written (innermost caller first).
enterNative ::
  FunPtr NativeFn ->
  MutableByteArray RealWorld ->
  MutableArray RealWorld Closure ->
  MutableArray RealWorld Closure ->
  Int ->
  Int ->
  Int ->
  IO (Status, Int, Int, Int, [FrameRecord])
enterNative fn (MutableByteArray ustk) bstk@(MutableArray bstk#) (MutableArray pool) ap fp sp =
  allocaBytes 40 $ \out -> do
    status <- c_enter fn ustk bstk# pool (fromIntegral (sizeofMutableArray bstk)) (fromIntegral ap) (fromIntegral fp) (fromIntegral sp) out
    ap' <- peekElemOff out 0
    fp' <- peekElemOff out 1
    sp' <- peekElemOff out 2
    n <- peekElemOff out 3
    recs <- peekElemOff out 4
    frames <-
      if n == 0
        then pure []
        else do
          let buf = wordPtrToPtr (fromIntegral recs) :: Ptr Int64
              record i =
                FrameRecord
                  <$> (fromIntegral <$> peekElemOff buf (3 * i))
                  <*> (fromIntegral <$> peekElemOff buf (3 * i + 1))
                  <*> (fromIntegral <$> peekElemOff buf (3 * i + 2))
          fs <- mapM record [0 .. fromIntegral n - 1]
          free buf
          pure fs
    pure (fromIntegral status, fromIntegral ap', fromIntegral fp', fromIntegral sp', frames)

-- | Passes stress settings to the C side: poll every N entries, treat every
-- Nth callee as uncompiled, C stack budget in bytes (0 for the default),
-- trace. Called once at startup.
configureNative :: Int -> Int -> Int -> Bool -> IO ()
configureNative pollEvery calleeEvery cstack tr =
  c_configure (fromIntegral pollEvery) (fromIntegral calleeEvery) (fromIntegral cstack) (if tr then 1 else 0)

-- | The offsets of the fields of the C @Ctx@, in the order they are
-- declared, and its total size. The code generator uses these.
ctxLayout :: IO ([Int], Int)
ctxLayout = allocaBytes (8 * 32) $ \out -> do
  size <- c_ctxLayout out 32
  offs <- mapM (peekElemOff out) [0 .. 16]
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

#else

enterNative ::
  FunPtr NativeFn ->
  MutableByteArray RealWorld ->
  MutableArray RealWorld Closure ->
  MutableArray RealWorld Closure ->
  Int ->
  Int ->
  Int ->
  IO (Status, Int, Int, Int, [FrameRecord])
enterNative _ _ _ _ _ _ _ = error "JIT: not built in, but a native code cell holds code"

configureNative :: Int -> Int -> Int -> Bool -> IO ()
configureNative _ _ _ _ = pure ()

ctxLayout :: IO ([Int], Int)
ctxLayout = pure ([], 0)

probeClosure :: MutableArray RealWorld Any -> IO [Int]
probeClosure _ = pure []

hplimValue :: IO Int
hplimValue = pure 0


#endif
