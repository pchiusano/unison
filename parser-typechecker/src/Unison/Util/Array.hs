{-# Language GADTs #-}
{-# Language BangPatterns #-}

module Unison.Util.Array where

import qualified Data.Massiv.Array as A
import Data.Bits as Bits
import Data.Bit (Bit(..))
import Data.Word
import Data.Int
import Data.Functor

data Array ix a = A !(A.Array A.D ix a) (A.Array A.U ix a)

data Type a where
  IntT :: Type Int64
  NatT :: Type Word64
  FloatT :: Type Double
  BitT :: Type Bit

force :: (A.Unbox a, A.Index ix) => Array ix a -> Array ix a
force (A _ !af) = A (delay af) af

par :: (A.Unbox a, A.Index ix) => Array ix a -> Array ix a
par (A a _) = let a' = A.setComp (A.ParN 0) a in A a' (A.compute a')

parN :: (A.Unbox a, A.Index ix) => Word16 -> Array ix a -> Array ix a
parN n (A a _) = let a' = A.setComp (A.ParN n) a in A a' (A.compute a')

unsafeAt :: (A.Unbox a, A.Index ix) => ix -> Array ix a -> a
unsafeAt ix (A _ af) = A.index' af ix

size :: (A.Unbox a, A.Index ix) => Array ix a -> ix
size (A _ af) = A.unSz $ A.size af

delay :: A.Source sh ix a => A.Index ix => A.Array sh ix a -> A.Array A.D ix a
delay = A.map id

fill :: (A.Unbox a, A.Index ix) => ix -> a -> Array ix a
fill n a =
  let !arr = A.makeArray A.Seq (A.Sz n) (const a)
  in A arr (A.compute arr)

extend :: (A.Unbox a, A.Index ix) => ix -> a -> Array ix a -> Array ix a
extend ix a arr =
  if size arr >= ix then arr
  else let
    !arr' = A.makeArray A.Seq (A.Sz (max ix (size arr))) at
    at i = if i < size arr then unsafeAt i arr else a
    in A arr' (A.compute arr')

ptMin, ptMax :: (Ord a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Maybe (Array ix a)
ptMin = pt min
ptMax = pt max

pt :: (A.Index ix, A.Unbox e) => (e1 -> e2 -> e) -> Array ix e1 -> Array ix e2 -> Maybe (Array ix e)
pt f (A a1 _) (A a2 _) =
  if A.size a1 == A.size a2 then
    let !a3 = A.zipWith f a1 a2 in pure (A a3 (A.compute a3))
  else Nothing
{-# INLINE pt #-}

ptb :: (A.Index ix) => (e1 -> e2 -> Bool) -> Array ix e1 -> Array ix e2 -> Maybe (Array ix Bit)
ptb f a a2 = pt (\a b -> Bit (f a b)) a a2
{-# INLINE ptb #-}

ptLt, ptLteq, ptEq, ptGt, ptGteq
  :: (Ord a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Maybe (Array ix Bit)
ptLt = ptb (<)
ptLteq = ptb (<=)
ptEq = ptb (==)
ptGt = ptb (>)
ptGteq = ptb (>=)

ptAdd, ptSubtract, ptMultiply
  :: (Num a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Maybe (Array ix a)
ptAdd (A a1 _) (A a2 _) = a1 A..+. a2 <&> \a3 -> A a3 (A.compute a3)
ptSubtract (A a1 _) (A a2 _) = a1 A..-. a2 <&> \a3 -> A a3 (A.compute a3)
ptMultiply (A a1 _) (A a2 _) = a1 A..*. a2 <&> \a3 -> A a3 (A.compute a3)

ptGcd, ptLcm, ptQuot, ptDiv, ptMod, ptRem
  :: (Integral a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Maybe (Array ix a)
ptGcd = pt gcd
ptLcm = pt lcm
ptQuot = pt quot
ptDiv = pt div
ptMod = pt mod
ptRem = pt rem

ptDivide, ptPow, ptLogBase :: (Floating a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Maybe (Array ix a)
ptDivide (A a1 _) (A a2 _) = a1 A../. a2 <&> \a3 -> A a3 (A.compute a3)
ptPow = pt (**)
ptLogBase = pt logBase

ptPowi :: (Num a, Integral b, A.Unbox a, A.Index ix) => Array ix a -> Array ix b -> Array ix a
ptPowi (A a1 _) (A a2 _) = let !a3 = A.zipWith (^) a1 a2 in A a3 (A.compute a3)

ptExp, ptLog, ptSqrt, ptSin, ptCos, ptTan, ptAsin, ptAcos,
  ptAtan, ptSinh, ptCosh, ptTanh, ptAsinh, ptAcosh, ptAtanh
  :: (Floating a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a
ptExp (A a1 _) = let !a' = A.expA a1 in A a' (A.compute a')
ptLog (A a1 _) = let !a' = A.logA a1 in A a' (A.compute a')
ptSqrt (A a1 _) = let !a' = A.sqrtA a1 in A a' (A.compute a')
ptSin (A a1 _) = let !a' = A.sinA a1 in A a' (A.compute a')
ptCos (A a1 _) = let !a' = A.cosA a1 in A a' (A.compute a')
ptTan (A a1 _) = let !a' = A.tanA a1 in A a' (A.compute a')
ptAsin (A a1 _) = let !a' = A.asinA a1 in A a' (A.compute a')
ptAcos (A a1 _) = let !a' = A.acosA a1 in A a' (A.compute a')
ptAtan (A a1 _) = let !a' = A.atanA a1 in A a' (A.compute a')
ptSinh (A a1 _) = let !a' = A.sinhA a1 in A a' (A.compute a')
ptCosh (A a1 _) = let !a' = A.coshA a1 in A a' (A.compute a')
ptTanh (A a1 _) = let !a' = A.tanhA a1 in A a' (A.compute a')
ptAsinh (A a1 _) = let !a' = A.asinhA a1 in A a' (A.compute a')
ptAcosh (A a1 _) = let !a' = A.acoshA a1 in A a' (A.compute a')
ptAtanh (A a1 _) = let !a' = A.atanhA a1 in A a' (A.compute a')

ptCeiling, ptFloor, ptTruncate, ptRound
  :: (RealFrac a, A.Unbox a, Integral b, A.Unbox b, A.Index ix) => Array ix a -> Array ix b
ptCeiling (A a1 _) = let !a' = A.ceilingA a1 in A a' (A.compute a')
ptFloor (A a1 _) = let !a' = A.floorA a1 in A a' (A.compute a')
ptTruncate (A a1 _) = let !a' = A.truncateA a1 in A a' (A.compute a')
ptRound (A a1 _) = let !a' = A.roundA a1 in A a' (A.compute a')

mux2 :: (A.Unbox a, A.Index ix) => Array ix Bit -> Array ix a -> Array ix a -> Array ix a
mux2 (A b _) (A a1 _) (A a2 _) =
  let !a = A.zipWith3 (\(Bit b) a1 a2 -> if b then a2 else a1) b a1 a2
  in A a (A.compute a)

pick :: (A.Manifest A.D ix ix2, A.Unbox a, A.Index ix, A.Index ix2) => Array ix ix2 -> Array ix2 a -> Maybe (Array ix a)
pick (A inds _) (A _ src) =
  if A.isEmpty inds then pure $ A A.empty A.empty
  else let
    max = A.maximum' inds
    in if max >= A.unSz (A.size src) then Nothing
       else let a = A.backpermute' (A.size inds) (A.index' inds) src
            in pure $ A a (A.compute a)

and, or, xor :: (A.Unbox a, Bits a, A.Index ix) => Array ix a -> Array ix a -> Maybe (Array ix a)
and = pt (.&.)
or = pt (.|.)
xor = pt Bits.xor

complement :: (A.Unbox a, Bits a, A.Index ix) => Array ix a -> Array ix a
complement (A a _) = let !a' = A.map Bits.complement a in A a' (A.compute a')

popCount :: (A.Unbox a, Bits a, A.Index ix) => Array ix a -> Array ix Word64
popCount (A a _) = let !a' = A.map (fromIntegral . Bits.popCount) a in A a' (A.compute a')

elemIndices :: (A.Unbox a, Eq a, A.Unbox ix, A.Index ix) => a -> Array ix a -> Array A.Ix1 ix
elemIndices q (A a _) =
  let !a' = A.flatten . A.compute . A.imapMaybeS (\i a -> if a == q then Just i else Nothing) $ a
  in A (delay a') a'
