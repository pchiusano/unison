{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- M0 spike 2: can code running inside an unsafe foreign call allocate Unison runtime
-- values that Haskell then reads correctly, store them where the GC will find them,
-- and notice when the runtime wants the thread to stop?
-- See docs/jit-implementation-plan.md, milestone M0.
--
-- This uses the real types from unison-runtime, so the layouts probed are the real ones.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Monad (forM, forM_, forever, unless, when)
import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Primitive.Array (MutableArray (..), newArray, readArray, writeArray)
import Data.Word (Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Exts (Any, MutableArray#, RealWorld)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Mem (performMajorGC, performMinorGC)
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Runtime.Stack
import Unison.Type qualified as Ty
import Unsafe.Coerce (unsafeCoerce)

type Elems = MutableArray# RealWorld Any

foreign import ccall unsafe "rt_probe" rtProbe :: Elems -> Int64 -> Ptr () -> IO ()

foreign import ccall unsafe "rt_build_list"
  rtBuildList :: Elems -> Int64 -> Int64 -> Int64 -> Ptr () -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "rt_spin" rtSpin :: Int64 -> IO Int64

-- ---------------------------------------------------------------------------
-- Layout probe

data Layout = Layout
  { lInfo :: Word64,
    lPtrTag :: Word64,
    lType :: Word64,
    lPtrs :: Int,
    lNptrs :: Int,
    lConTag :: Word64,
    lRaw :: [Word64],
    lMatch :: [Int64]
  }
  deriving (Show)

-- The sample goes in element 0. The values in its pointer fields follow.
probe :: Any -> [Any] -> IO Layout
probe sample fields = do
  let n = 1 + length fields
  arr@(MutableArray arr#) <- newArray n sample
  forM_ (zip [1 ..] fields) $ \(i, f) -> writeArray arr i f
  allocaBytes (8 * 38) $ \p -> do
    rtProbe arr# (fromIntegral n) p
    [info, tag, ty, ptrs, nptrs, con] <- peekArray 6 (p `plusPtr` 0) :: IO [Word64]
    raw <- peekArray 16 (p `plusPtr` 48) :: IO [Word64]
    match <- peekArray 16 (p `plusPtr` (48 + 128)) :: IO [Int64]
    let total = fromIntegral (ptrs + nptrs)
    pure (Layout info tag ty (fromIntegral ptrs) (fromIntegral nptrs) con (take total raw) (take total match))

any' :: a -> Any
any' = unsafeCoerce

showLayout :: String -> Layout -> IO ()
showLayout name l =
  putStrLn
    ( "    "
        ++ name
        ++ ": pointer tag "
        ++ show (lPtrTag l)
        ++ ", constructor "
        ++ show (lConTag l)
        ++ ", "
        ++ show (lPtrs l)
        ++ " pointer + "
        ++ show (lNptrs l)
        ++ " other fields"
    )

data Cons = Cons
  { cLayout :: Layout,
    offRef, offCon, offHeadU, offHeadB, offTailU, offTailB :: Int
  }
  deriving (Show)

conNil, conCons :: Word64
conNil = 0
conCons = 1

listRef :: Reference
listRef = Ty.listRef

nil :: Closure
nil = Enum listRef (PackedTag conNil)

-- Works out where each field of a two-field constructor is, by building one with
-- recognizable values and looking for them.
probeCons :: IO Cons
probeCons = do
  let !ref = listRef
      !nat = natTypeTag
      !tl = nil
      sample = Data2 ref (PackedTag 0x1111) (Val 0x2222 nat) (Val 0x3333 tl)
  l <- sample `seq` probe (any' sample) [any' ref, any' nat, any' tl]
  let ptrField k = maybe (fail' ("pointer field " ++ show k)) pure (elemIndex k (lMatch l))
      wordField w = maybe (fail' ("word field " ++ show w)) pure (elemIndex w (lRaw l))
      fail' what = putStrLn ("  FAILED: probe could not find " ++ what ++ " in " ++ show l) >> exitFailure
  Cons l <$> ptrField 1 <*> wordField 0x1111 <*> wordField 0x2222 <*> ptrField 2 <*> wordField 0x3333 <*> ptrField 3

withConsLayout :: Cons -> (Ptr () -> IO a) -> IO a
withConsLayout c k = allocaBytes (8 * 10) $ \p -> do
  let l = cLayout c
      put :: Int -> Word64 -> IO ()
      put i = pokeByteOff p (8 * i)
  put 0 (lInfo l)
  put 1 (lPtrTag l)
  put 2 (fromIntegral (lPtrs l + lNptrs l))
  forM_ (zip [3 ..] [offRef c, offCon c, offHeadU c, offHeadB c, offTailU c, offTailB c]) $ \(i, o) ->
    put i (fromIntegral o)
  put 9 conCons
  k p

-- ---------------------------------------------------------------------------
-- Building a list natively

listSlot, refSlot, natSlot :: Int
listSlot = 0
refSlot = 1
natSlot = 2

-- Stands in for the boxed stack: a long-lived array in the old generation.
newStack :: IO (MutableArray RealWorld Any)
newStack = do
  arr <- newArray 64 (any' nil)
  writeArray arr refSlot (any' $! listRef)
  writeArray arr natSlot (any' $! natTypeTag)
  performMajorGC
  performMajorGC
  pure arr

-- Builds a list of `total` cells, a few at a time, returning to Haskell in between.
buildList :: Cons -> Bool -> Int64 -> Int64 -> IO (MutableArray RealWorld Any, Int)
buildList c mark total budget = do
  arr@(MutableArray arr#) <- newStack
  let go !done !calls
        | done >= total = pure calls
        | otherwise = do
            made <-
              withConsLayout c $ \p ->
                rtBuildList arr# (fromIntegral listSlot) (fromIntegral refSlot) (fromIntegral natSlot) p done (total - done) budget (if mark then 1 else 0)
            when (made <= 0) (putStrLn "  FAILED: budget too small for one cell" >> exitFailure)
            -- what the interpreter does between native runs: allocate a little
            let !junk = length (show calls)
            when (calls `mod` 64 == 0) performMinorGC
            junk `seq` go (done + made) (calls + 1 :: Int)
  calls <- go 0 0
  pure (arr, calls)

-- Reads the list back with ordinary Haskell pattern matching.
walk :: Closure -> Either String (Int64, Word64)
walk = go 0 0
  where
    go !n !s (Enum _ (PackedTag t)) | t == conNil = Right (n, s)
    go !n !s (Data2 _ (PackedTag t) (NatVal h) (BoxedVal tl)) | t == conCons = go (n + 1) (s + h) tl
    go !n _ other = Left ("unexpected value after " ++ show n ++ " cells: " ++ take 200 (show other))

checkList :: MutableArray RealWorld Any -> Int64 -> IO Bool
checkList arr total = do
  performMajorGC
  v <- readArray arr listSlot
  let expected = fromIntegral (total * (total - 1) `div` 2) :: Word64
  case walk (unsafeCoerce v) of
    Right (n, s)
      | n == total && s == expected -> do
          putStrLn ("  ok      " ++ show n ++ " cells read back, sum " ++ show s)
          pure True
      | otherwise -> do
          putStrLn ("  WRONG   " ++ show n ++ " cells, sum " ++ show s ++ ", expected " ++ show total ++ " and " ++ show expected)
          pure False
    Left e -> putStrLn ("  WRONG   " ++ e) >> pure False

ms :: Word64 -> Word64 -> String
ms t0 t1 = show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms"

-- ---------------------------------------------------------------------------

testProbe :: IO Cons
testProbe = do
  putStrLn "1. layout probe, on the real runtime types"
  let !ref = listRef
      !nat = natTypeTag
  showLayout "Enum " =<< probe (any' $! Enum ref (PackedTag 7)) [any' ref]
  showLayout "Data1" =<< probe (any' $! Data1 ref (PackedTag 7) (Val 9 nat)) [any' ref, any' nat]
  c <- probeCons
  showLayout "Data2" (cLayout c)
  showLayout "type tag" =<< probe (any' nat) []
  putStrLn
    ( "    Data2 payload offsets: reference "
        ++ show (offRef c)
        ++ ", head type tag "
        ++ show (offHeadB c)
        ++ ", tail "
        ++ show (offTailB c)
        ++ ", constructor "
        ++ show (offCon c)
        ++ ", head value "
        ++ show (offHeadU c)
        ++ ", tail value "
        ++ show (offTailU c)
    )
  putStrLn "  ok      every field located"
  pure c

testList :: Cons -> Bool -> IO ()
testList c mark = do
  putStrLn ("2. build a 2 million cell list natively, " ++ (if mark then "marking the array" else "WITHOUT marking the array (expected to fail)"))
  t0 <- getMonotonicTimeNSec
  (arr, calls) <- buildList c mark 2000000 4096
  t1 <- getMonotonicTimeNSec
  putStrLn ("    " ++ show calls ++ " native calls, " ++ ms t0 t1)
  ok <- checkList arr 2000000
  unless ok exitFailure

testMemory :: Cons -> IO ()
testMemory c = do
  putStrLn "3. memory stays bounded when native code allocates garbage"
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "    skipped: run with +RTS -T"
    else do
      arr@(MutableArray arr#) <- newStack
      before <- getRTSStats
      let cells = 4096 `div` 8 :: Int64
          rounds = 200000 :: Int
      forM_ [1 .. rounds] $ \_ -> do
        writeArray arr listSlot (any' nil) -- drop the previous list
        _ <- withConsLayout c $ \p -> rtBuildList arr# 0 1 2 p 0 cells 4096 1
        pure ()
      after <- getRTSStats
      let allocated = allocated_bytes after - allocated_bytes before
          peak = max_mem_in_use_bytes after
          mb x = show (x `div` (1024 * 1024)) ++ " MB"
      putStrLn ("    allocated " ++ mb allocated ++ " in total, " ++ show (gcs after - gcs before) ++ " collections, peak memory in use " ++ mb peak)
      if peak < 256 * 1024 * 1024
        then putStrLn "  ok      peak memory is small compared to the total allocated"
        else putStrLn "  WRONG   memory grew without bound" >> exitFailure

testPoll :: IO ()
testPoll = do
  putStrLn "4. native code notices when the runtime wants the thread to stop"
  _ <- forkIO (forever (threadDelay 1000))
  yield
  times <- forM [1 .. 5 :: Int] $ \_ -> do
    t0 <- getMonotonicTimeNSec
    n <- rtSpin 20000000000
    t1 <- getMonotonicTimeNSec
    when (n < 0) (putStrLn "  WRONG   the flag was never set" >> exitFailure)
    yield
    pure (fromIntegral (t1 - t0) / 1e6 :: Double)
  putStrLn ("    time spinning before the flag was seen, 5 tries: " ++ show times ++ " ms")
  if maximum times < 100
    then putStrLn "  ok      seen within 100 ms every time"
    else putStrLn "  WRONG   took too long" >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  c <- testProbe
  case args of
    ["nomark"] -> testList c False
    _ -> do
      testList c True
      testMemory c
      testPoll
      putStrLn "ALL PASSED"
