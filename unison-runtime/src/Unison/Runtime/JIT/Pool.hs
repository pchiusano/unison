-- | The constant pool: Haskell heap objects that native code needs, in an
-- array whose address is passed on every entry. See docs/jit-design.md,
-- "Appendix: the constant pool", and docs/jit-m3.md for why there is one
-- global pool rather than one per module.
--
-- Indices are assigned when a module is compiled ('poolIndices') and are
-- constants in the generated code. The array grows by copying; the old
-- arrays are kept, so an address read at entry stays valid for as long
-- as that native run lasts.
module Unison.Runtime.JIT.Pool
  ( PoolKey (..),
    poolIndices,
    currentPool,
    poolIndexCharTag,
    poolIndexFloatTag,
    poolIndexIntTag,
    poolIndexNatTag,
    poolIndexTrue,
    poolIndexFalse,
  )
where

import Control.Monad (forM)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, sizeofMutableArray, writeArray)
import GHC.Exts (RealWorld)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag)
import Unison.Runtime.MCode (MLit (..))
import Unison.Runtime.Stack (Closure, Val (..), charTypeTag, falseVal, floatTypeTag, intTypeTag, natTypeTag, trueVal, pattern Enum, pattern Foreign)
import Unison.Runtime.Stack qualified as Stack

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

fixedEntries :: Int
fixedEntries = 6

-- | What a pool entry is. Entries are interned by key.
data PoolKey
  = -- | a constructor with no fields. Also how a 'Reference' is kept: the
    -- entry for @KeyEnum r 0@ is an 'Enum' whose reference field native
    -- code reads when it builds a constructor of that type.
    KeyEnum !Reference !PackedTag
  | -- | a boxed literal (text, term link, type link)
    KeyLit !MLit
  deriving (Eq, Ord, Show)

data Pool = Pool
  { poolArray :: !(MutableArray RealWorld Closure),
    poolNext :: !Int,
    poolKeys :: !(Map.Map PoolKey Int),
    -- | superseded arrays, kept alive on purpose
    poolOld :: [MutableArray RealWorld Closure]
  }

pool :: IORef Pool
pool = unsafePerformIO $ do
  -- Everything is forced before it goes in: native code reads these
  -- objects' fields directly, so they must be values, not thunks.
  arr <- newArray 256 $! natTypeTag
  let put i c = writeArray arr i $! c
  put poolIndexCharTag charTypeTag
  put poolIndexFloatTag floatTypeTag
  put poolIndexIntTag intTypeTag
  put poolIndexNatTag natTypeTag
  put poolIndexTrue (getBoxedVal trueVal)
  put poolIndexFalse (getBoxedVal falseVal)
  newIORef (Pool arr fixedEntries Map.empty [])
{-# NOINLINE pool #-}

-- | The array to pass to native code now.
currentPool :: IO (MutableArray RealWorld Closure)
currentPool = poolArray <$> readIORef pool

-- | The index of each key, adding the ones not seen before. Called when
-- a module is compiled, before its code is generated.
poolIndices :: [PoolKey] -> IO (Map.Map PoolKey Int)
poolIndices keys = do
  ixs <- forM keys $ \k -> do
    p <- readIORef pool
    case Map.lookup k (poolKeys p) of
      Just i -> pure (k, i)
      Nothing -> do
        p <- grow p
        let i = poolNext p
        writeArray (poolArray p) i $! closureFor k
        writeIORef pool p {poolNext = i + 1, poolKeys = Map.insert k i (poolKeys p)}
        pure (k, i)
  pure (Map.fromList ixs)
  where
    grow p
      | poolNext p < sizeofMutableArray (poolArray p) = pure p
      | otherwise = do
          let old = poolArray p
              n = sizeofMutableArray old
          new <- newArray (2 * n) $! natTypeTag
          copyMutableArray new 0 old 0 n
          pure p {poolArray = new, poolOld = old : poolOld p}

closureFor :: PoolKey -> Closure
closureFor = \case
  KeyEnum r t -> Enum r t
  KeyLit l -> case l of
    MT t -> Foreign (Stack.WrapText t)
    MM r -> Foreign (Stack.WrapReferent r)
    MY r -> Foreign (Stack.WrapReference r)
    _ -> error "closureFor: unboxed literal"
