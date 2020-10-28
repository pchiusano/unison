{-# Language GADTs #-}
{-# Language BangPatterns #-}

module Unison.Util.Array where

import qualified Data.Massiv.Array as A
import Data.Bit (Bit(..))
import Data.Word

data Array ix a = A !(A.Array A.D ix a) (A.Array A.U ix a)

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

ptMin :: (Ord a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptMin (A a1 _) (A a2 _) = let !a3 = A.zipWith min a1 a2 in A a3 (A.compute a3)

ptMax :: (Ord a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptMax (A a1 _) (A a2 _) = let !a3 = A.zipWith max a1 a2 in A a3 (A.compute a3)

ptLt :: (Ord a, A.Index ix) => Array ix a -> Array ix a -> Array ix Bit
ptLt (A a1 _) (A a2 _) = let !a3 = A.map Bit (A.zipWith (<) a1 a2) in A a3 (A.compute a3)

ptLteq :: (Ord a, A.Index ix) => Array ix a -> Array ix a -> Array ix Bit
ptLteq (A a1 _) (A a2 _) = let !a3 = A.map Bit (A.zipWith (<=) a1 a2) in A a3 (A.compute a3)

ptEq :: (Ord a, A.Index ix) => Array ix a -> Array ix a -> Array ix Bit
ptEq (A a1 _) (A a2 _) = let !a3 = A.map Bit (A.zipWith (==) a1 a2) in A a3 (A.compute a3)

ptGt :: (Ord a, A.Index ix) => Array ix a -> Array ix a -> Array ix Bit
ptGt (A a1 _) (A a2 _) = let !a3 = A.map Bit (A.zipWith (>) a1 a2) in A a3 (A.compute a3)

ptGteq :: (Ord a, A.Index ix) => Array ix a -> Array ix a -> Array ix Bit
ptGteq (A a1 _) (A a2 _) = let !a3 = A.map Bit (A.zipWith (>=) a1 a2) in A a3 (A.compute a3)

ptAdd :: (Num a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptAdd (A a1 _) (A a2 _) = let !a3 = A.zipWith (+) a1 a2 in A a3 (A.compute a3)

ptSubtract :: (Num a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptSubtract (A a1 _) (A a2 _) = let !a3 = A.zipWith (-) a1 a2 in A a3 (A.compute a3)

ptMultiply :: (Num a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptMultiply (A a1 _) (A a2 _) = let !a3 = A.zipWith (*) a1 a2 in A a3 (A.compute a3)

ptDivide :: (Fractional a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptDivide (A a1 _) (A a2 _) = let !a3 = A.zipWith (/) a1 a2 in A a3 (A.compute a3)

ptGcd, ptLcm, ptQuot, ptDiv, ptMod, ptRem :: (Integral a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptGcd (A a1 _) (A a2 _) = let !a3 = A.zipWith gcd a1 a2 in A a3 (A.compute a3)
ptLcm (A a1 _) (A a2 _) = let !a3 = A.zipWith lcm a1 a2 in A a3 (A.compute a3)
ptQuot (A a1 _) (A a2 _) = let !a3 = A.zipWith quot a1 a2 in A a3 (A.compute a3)
ptDiv (A a1 _) (A a2 _) = let !a3 = A.zipWith div a1 a2 in A a3 (A.compute a3)
ptMod (A a1 _) (A a2 _) = let !a3 = A.zipWith mod a1 a2 in A a3 (A.compute a3)
ptRem (A a1 _) (A a2 _) = let !a3 = A.zipWith rem a1 a2 in A a3 (A.compute a3)

ptExp, ptLog, ptSqrt, ptSin, ptCos, ptTan, ptAsin, ptAcos,
  ptAtan, ptSinh, ptCosh, ptTanh, ptAsinh, ptAcosh, ptAtanh
  :: (Floating a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a
ptExp (A a1 _) = let !a' = A.map exp a1 in A a' (A.compute a')
ptLog (A a1 _) = let !a' = A.map log a1 in A a' (A.compute a')
ptSqrt (A a1 _) = let !a' = A.map sqrt a1 in A a' (A.compute a')
ptSin (A a1 _) = let !a' = A.map sin a1 in A a' (A.compute a')
ptCos (A a1 _) = let !a' = A.map cos a1 in A a' (A.compute a')
ptTan (A a1 _) = let !a' = A.map tan a1 in A a' (A.compute a')
ptAsin (A a1 _) = let !a' = A.map asin a1 in A a' (A.compute a')
ptAcos (A a1 _) = let !a' = A.map acos a1 in A a' (A.compute a')
ptAtan (A a1 _) = let !a' = A.map atan a1 in A a' (A.compute a')
ptSinh (A a1 _) = let !a' = A.map sinh a1 in A a' (A.compute a')
ptCosh (A a1 _) = let !a' = A.map cosh a1 in A a' (A.compute a')
ptTanh (A a1 _) = let !a' = A.map tanh a1 in A a' (A.compute a')
ptAsinh (A a1 _) = let !a' = A.map asinh a1 in A a' (A.compute a')
ptAcosh (A a1 _) = let !a' = A.map acosh a1 in A a' (A.compute a')
ptAtanh (A a1 _) = let !a' = A.map atanh a1 in A a' (A.compute a')

ptCeiling, ptFloor, ptTruncate, ptRound
  :: (RealFrac a, A.Unbox a, Integral b, A.Unbox b, A.Index ix) => Array ix a -> Array ix b
ptCeiling (A a1 _) = let !a' = A.map ceiling a1 in A a' (A.compute a')
ptFloor (A a1 _) = let !a' = A.map floor a1 in A a' (A.compute a')
ptTruncate (A a1 _) = let !a' = A.map truncate a1 in A a' (A.compute a')
ptRound (A a1 _) = let !a' = A.map round a1 in A a' (A.compute a')


ptPow, ptLogBase :: (Floating a, A.Unbox a, A.Index ix) => Array ix a -> Array ix a -> Array ix a
ptPow (A a1 _) (A a2 _) = let !a3 = A.zipWith (**) a1 a2 in A a3 (A.compute a3)
ptLogBase (A base _) (A a _) = let !a' = A.zipWith logBase base a in A a' (A.compute a')

ptPowi :: (Num a, Integral b, A.Unbox a, A.Index ix) => Array ix a -> Array ix b -> Array ix a
ptPowi (A a1 _) (A a2 _) = let !a3 = A.zipWith (^) a1 a2 in A a3 (A.compute a3)

choose :: (A.Unbox a, A.Index ix) => Array ix Bit -> Array ix a -> Array ix a -> Array ix a
choose (A b _) (A a1 _) (A a2 _) =
  let !a = A.zipWith3 (\(Bit b) a1 a2 -> if b then a2 else a1) b a1 a2
  in A a (A.compute a)
