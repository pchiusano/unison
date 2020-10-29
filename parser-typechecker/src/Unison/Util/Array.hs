{-# Language GADTs #-}
{-# Language BangPatterns #-}

module Unison.Util.Array where

import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Core.Operations as A
import Data.Bit (Bit(..))
import Data.Word
import Data.Int

type Array ix a = A.Array A.U ix a

data Type a where
  IntT :: Type Int64
  NatT :: Type Word64
  FloatT :: Type Double
  BitT :: Type Bit

size :: (A.Unbox a, A.Index ix) => Array ix a -> ix
size a = A.unSz $ A.size a
{-# INLINE size #-}

fill :: (A.Unbox a, A.Index ix) => ix -> a -> Array ix a
fill n a = A.makeArray A.Seq (A.Sz n) (const a)
{-# INLINE fill #-}

unsafeAt :: (A.Unbox a, A.Index ix) => ix -> Array ix a -> a
unsafeAt i a = A.index' a i
{-# INLINE unsafeAt #-}

extend :: (A.Unbox a, A.Index ix) => ix -> a -> Array ix a -> Array ix a
extend ix a arr =
  if size arr >= ix then arr
  else A.makeArray A.Seq (A.Sz (max ix (size arr))) at
       where at i = if i < size arr then unsafeAt i arr else a
{-# INLINE extend #-}

ptMin, ptMax :: (Ord a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptMin = pt min
ptMax = pt max
{-# INLINE ptMin #-}
{-# INLINE ptMax #-}

pt :: (A.Index ix, A.Unbox e1, A.Unbox e2, A.Unbox e) => (e1 -> e2 -> e) -> Array ix e1 -> Array ix e2 -> Array ix e
pt f a1 a2 = A.compute (A.zipWith f a1 a2)
{-# INLINE pt #-}

ptb :: (A.Unbox e1, A.Unbox e2, A.Index ix) => (e1 -> e2 -> Bool) -> Array ix e1 -> Array ix e2 -> Array ix Bit
ptb f a a2 = pt (\a b -> Bit (f a b)) a a2
{-# INLINE ptb #-}

ptExp, ptLog, ptSqrt, ptSin, ptCos, ptTan, ptAsin, ptAcos,
  ptAtan, ptSinh, ptCosh, ptTanh, ptAsinh, ptAcosh, ptAtanh
  :: (Floating a, A.NumericFloat A.U a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a
ptExp = A.expA
ptLog = A.logA
ptSqrt = A.sqrtA
ptSin = A.sinA
ptCos = A.cosA
ptTan = A.tanA
ptAsin = A.asinA
ptAcos = A.acosA
ptAtan = A.atanA
ptSinh = A.sinhA
ptCosh = A.coshA
ptTanh = A.tanhA
ptAsinh = A.asinhA
ptAcosh = A.acoshA
ptAtanh = A.atanhA
{-# INLINE ptExp #-}
{-# INLINE ptLog #-}
{-# INLINE ptSqrt #-}
{-# INLINE ptSin #-}
{-# INLINE ptCos #-}
{-# INLINE ptTan #-}
{-# INLINE ptAsin #-}
{-# INLINE ptAcos #-}
{-# INLINE ptAtan #-}
{-# INLINE ptSinh #-}
{-# INLINE ptCosh #-}
{-# INLINE ptTanh #-}
{-# INLINE ptAsinh #-}
{-# INLINE ptAcosh #-}
{-# INLINE ptAtanh #-}

ptCeiling, ptFloor, ptTruncate, ptRound
  :: (RealFrac a, A.Unbox a, A.Numeric A.U b, Integral b, A.Unbox b, A.Index ix) => Array ix a -> Array ix b
ptCeiling = A.ceilingA
ptFloor = A.floorA
ptTruncate = A.truncateA
ptRound = A.roundA
{-# INLINE ptCeiling #-}
{-# INLINE ptFloor #-}
{-# INLINE ptTruncate #-}
{-# INLINE ptRound #-}

mux2 :: (A.Unbox a, A.Index ix) => Array ix Bit -> Array ix a -> Array ix a -> Array ix a
mux2 b if0 if1 =
  A.compute (A.zipWith3 (\(Bit b) a1 a2 -> if b then a2 else a1) b if0 if1)
{-# INLINE mux2 #-}

pick :: (A.Unbox ix2, A.Unbox a, A.Index ix, A.Index ix2)
     => Array ix ix2 -> Array ix2 a -> Maybe (Array ix a)
pick inds src
  | A.isEmpty inds                         = Just A.empty
  | A.maximum' inds >= A.unSz (A.size src) = Nothing
  | otherwise                              = Just $ A.compute (A.backpermute' (A.size inds) (A.index' inds) src)
{-# INLINE pick #-}

indicesOf :: (A.Unbox a, Eq a, A.Unbox ix, A.Index ix) => a -> Array ix a -> Array A.Ix1 ix
indicesOf q a =
  A.flatten . A.compute . A.imapMaybeS (\i a -> if a == q then Just i else Nothing) $ a
{-# INLINE indicesOf #-}

flatten :: (A.Index ix, A.Unbox a) => Array ix a -> Array A.Ix1 a
flatten = A.flatten
{-# INLINE flatten #-}

tally :: (Ord a, A.Index ix, A.Unbox a) => Array ix a -> Array A.Ix1 (a, Word64)
tally a =
  let (a', n) = A.unzip (A.computeAs A.U $ A.tally a)
  in A.compute $ A.zip a' (A.map fromIntegral n)
{-# INLINE tally #-}
