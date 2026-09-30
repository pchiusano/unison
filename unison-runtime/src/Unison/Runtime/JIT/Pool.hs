-- | The constant pool: Haskell heap objects that native code needs, in an
-- array whose address is passed on every entry. See docs/jit-design.md,
-- "Appendix: the constant pool". For now there is one global pool holding
-- the type-tag closures; per-module pools come with allocation (M3).
module Unison.Runtime.JIT.Pool
  ( globalPool,
    poolIndexCharTag,
    poolIndexFloatTag,
    poolIndexIntTag,
    poolIndexNatTag,
    poolIndexTrue,
    poolIndexFalse,
  )
where

import Data.Primitive.Array (MutableArray, newArray, writeArray)
import GHC.Exts (RealWorld)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Runtime.Stack (Closure, Val (..), charTypeTag, falseVal, floatTypeTag, intTypeTag, natTypeTag, trueVal)

-- The order matches unboxedTypeTagToInt in Stack.hs.
poolIndexCharTag, poolIndexFloatTag, poolIndexIntTag, poolIndexNatTag :: Int
poolIndexCharTag = 0
poolIndexFloatTag = 1
poolIndexIntTag = 2
poolIndexNatTag = 3

-- | The boolean closures, for comparison results.
poolIndexTrue, poolIndexFalse :: Int
poolIndexTrue = 4
poolIndexFalse = 5

globalPool :: MutableArray RealWorld Closure
globalPool = unsafePerformIO $ do
  -- Everything is forced before it goes in: native code reads these
  -- objects' fields directly, so they must be values, not thunks.
  arr <- newArray 6 $! natTypeTag
  let put i c = writeArray arr i $! c
  put poolIndexCharTag charTypeTag
  put poolIndexFloatTag floatTypeTag
  put poolIndexIntTag intTypeTag
  put poolIndexNatTag natTypeTag
  put poolIndexTrue (getBoxedVal trueVal)
  put poolIndexFalse (getBoxedVal falseVal)
  pure arr
{-# NOINLINE globalPool #-}
