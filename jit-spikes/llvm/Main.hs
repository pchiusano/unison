{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- M0 spike: can a Stack-built Haskell program link LLVM, compile IR text, and call the
-- result through an unsafe foreign call? Do guaranteed tail calls work with the C
-- calling convention? See docs/jit-implementation-plan.md, milestone M0.
module Main (main) where

import Control.Monad (unless, when)
import Data.Bits (xor)
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, WordPtr (..), castPtrToFunPtr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (poke)
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (exitFailure)

foreign import ccall safe "jit_init" jitInit :: IO CInt
foreign import ccall safe "jit_add_module" jitAddModule :: CString -> CSize -> CString -> IO CInt
foreign import ccall safe "jit_lookup" jitLookup :: CString -> IO Word64
foreign import ccall safe "jit_define_symbol" jitDefineSymbol :: CString -> Word64 -> IO CInt
foreign import ccall unsafe "jit_last_error" jitLastError :: IO CString
foreign import ccall unsafe "jit_triple" jitTriple :: IO CString
foreign import ccall unsafe "spike_helper_addr" spikeHelperAddr :: IO Word64

-- The uniform signature from the design doc.
type UnisonNativeFn = Ptr () -> Int64 -> Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic" callNative :: FunPtr UnisonNativeFn -> UnisonNativeFn

die :: String -> IO a
die what = do
  e <- peekCString =<< jitLastError
  putStrLn ("FAILED: " ++ what ++ ": " ++ e)
  exitFailure

addModule :: String -> String -> IO ()
addModule passes ir =
  withCStringLen ir $ \(p, n) -> withCString passes $ \ps -> do
    r <- jitAddModule p (fromIntegral n) ps
    when (r /= 0) (die "add module")

lookupFn :: String -> IO (FunPtr UnisonNativeFn)
lookupFn name = do
  a <- withCString name jitLookup
  when (a == 0) (die ("lookup " ++ name))
  pure (castPtrToFunPtr (wordPtrToPtr (WordPtr (fromIntegral a))))

timed :: String -> IO a -> IO a
timed label act = do
  t0 <- getMonotonicTimeNSec
  a <- act
  t1 <- getMonotonicTimeNSec
  putStrLn ("    " ++ label ++ ": " ++ show (fromIntegral (t1 - t0) / 1e6 :: Double) ++ " ms")
  pure a

check :: String -> Int64 -> Int64 -> IO ()
check label expected actual = do
  putStrLn ((if expected == actual then "  ok      " else "  WRONG   ") ++ label ++ " = " ++ show actual)
  unless (expected == actual) exitFailure

sig :: String
sig = "(ptr %ctx, i64 %a, i64 %b, i64 %c)"

-- Test 1: a loop written the way the code generator will write it (decision D11):
-- stack slots are allocas, and LLVM is expected to turn them into registers.
loopIR :: String
loopIR =
  unlines
    [ "define i64 @sum_to" ++ sig ++ " {",
      "entry:",
      "  %acc = alloca i64",
      "  %i = alloca i64",
      "  store i64 0, ptr %acc",
      "  store i64 0, ptr %i",
      "  br label %loop",
      "loop:",
      "  %iv = load i64, ptr %i",
      "  %done = icmp ugt i64 %iv, %a",
      "  br i1 %done, label %exit, label %body",
      "body:",
      "  %av = load i64, ptr %acc",
      "  %x = xor i64 %iv, %av",
      "  %av2 = add i64 %av, %x",
      "  store i64 %av2, ptr %acc",
      "  %iv2 = add i64 %iv, 1",
      "  store i64 %iv2, ptr %i",
      "  br label %loop",
      "exit:",
      "  %r = load i64, ptr %acc",
      "  ret i64 %r",
      "}"
    ]

-- Test 2: guaranteed tail calls between different functions, through function pointers
-- held in cells. Each function reads its callee from a cell whose address is a constant
-- in the IR, as native code cells work in the design. A chain of 100 million calls
-- overflows the C stack unless every one is a real tail call.
tailIR :: Word64 -> Word64 -> String
tailIR evenCell oddCell =
  unlines
    [ fn "is_even" "1" oddCell,
      fn "is_odd" "0" evenCell
    ]
  where
    fn name base cell =
      unlines
        [ "define i64 @" ++ name ++ sig ++ " {",
          "entry:",
          "  %z = icmp eq i64 %a, 0",
          "  br i1 %z, label %done, label %rec",
          "done:",
          "  ret i64 " ++ base,
          "rec:",
          "  %a1 = sub i64 %a, 1",
          "  %f = load ptr, ptr inttoptr (i64 " ++ show cell ++ " to ptr)",
          "  %r = musttail call i64 %f(ptr %ctx, i64 %a1, i64 %b, i64 %c)",
          "  ret i64 %r",
          "}"
        ]

-- Test 3: generated code calls a helper written in C, found by name.
helperIR :: String
helperIR =
  unlines
    [ "declare i64 @spike_helper(i64)",
      "define i64 @use_helper" ++ sig ++ " {",
      "  %r = call i64 @spike_helper(i64 %a)",
      "  %s = add i64 %r, %b",
      "  ret i64 %s",
      "}"
    ]

-- Test 4: a non-tail call followed by a status check, the shape of every native-to-native call.
callIR :: String
callIR =
  unlines
    [ "define i64 @leaf" ++ sig ++ " {",
      "  %neg = icmp slt i64 %a, 0",
      "  %r = select i1 %neg, i64 17, i64 0", -- 17 stands for an exit index
      "  ret i64 %r",
      "}",
      "define i64 @caller" ++ sig ++ " {",
      "entry:",
      "  %r = call i64 @leaf(ptr %ctx, i64 %a, i64 %b, i64 %c)",
      "  %ok = icmp eq i64 %r, 0",
      "  br i1 %ok, label %cont, label %unwind",
      "cont:",
      "  ret i64 0",
      "unwind:",
      "  ret i64 %r",
      "}"
    ]

main :: IO ()
main = do
  r <- jitInit
  when (r /= 0) (die "init")
  triple <- peekCString =<< jitTriple
  putStrLn ("LLVM JIT ready, target " ++ triple)

  putStrLn "1. loop with allocas, optimized at O2"
  timed "add module" (addModule "default<O2>" loopIR)
  sumTo <- timed "lookup (compiles)" (lookupFn "sum_to")
  v <- timed "run, 100 million iterations" (callNative sumTo nullPtr 100000000 0 0)
  check "sum_to 100000000" (sumToLoop 100000000) v

  putStrLn "2. guaranteed tail calls through cells"
  evenCell <- mallocBytes 8 :: IO (Ptr (FunPtr UnisonNativeFn))
  oddCell <- mallocBytes 8 :: IO (Ptr (FunPtr UnisonNativeFn))
  let addr p = let WordPtr w = ptrToWordPtr p in fromIntegral w :: Word64
  addModule "default<O2>" (tailIR (addr evenCell) (addr oddCell))
  isEven <- lookupFn "is_even"
  isOdd <- lookupFn "is_odd"
  poke evenCell isEven
  poke oddCell isOdd
  e1 <- timed "run, 100 million tail calls" (callNative isEven nullPtr 100000000 0 0)
  check "is_even 100000000" 1 e1
  e2 <- callNative isEven nullPtr 100000001 0 0
  check "is_even 100000001" 0 e2

  putStrLn "3. calling a C helper by name"
  h <- spikeHelperAddr
  r3 <- withCString "spike_helper" (\n -> jitDefineSymbol n h)
  when (r3 /= 0) (die "define symbol")
  addModule "default<O2>" helperIR
  useHelper <- lookupFn "use_helper"
  check "use_helper 20 1" 42 =<< callNative useHelper nullPtr 20 1 0

  putStrLn "4. non-tail call with a status check"
  addModule "default<O2>" callIR
  caller <- lookupFn "caller"
  check "caller, callee returns OK" 0 =<< callNative caller nullPtr 5 0 0
  check "caller, callee exits with 17" 17 =<< callNative caller nullPtr (-5) 0 0

  putStrLn "5. a parse error is reported, not fatal"
  bad <- withCStringLen "define i64 @broken( {" $ \(p, n) -> withCString "" (jitAddModule p (fromIntegral n))
  msg <- peekCString =<< jitLastError
  putStrLn ((if bad /= 0 then "  ok      " else "  WRONG   ") ++ "error: " ++ takeWhile (/= '\n') msg)
  when (bad == 0) exitFailure

  putStrLn "ALL PASSED"
  where
    -- the same loop as sum_to, in Haskell, for the expected value
    sumToLoop :: Int64 -> Int64
    sumToLoop n = go 0 0
      where
        go :: Int64 -> Int64 -> Int64
        go !acc !i
          | i > n = acc
          | otherwise = go (acc + xor i acc) (i + 1)
