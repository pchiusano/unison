{-# Language GADTs #-}
{-# Language BangPatterns #-}

module Unison.Util.Array (
  Array(..),
  at,
  force,
  natReplicate,
  intReplicate,
  floatReplicate,
  floatPtLt,
  floatPtGt,
  floatPtGteq,
  floatPtLteq,
  floatPtEq,
  floatSwitch,
  boolReplicate,
  size,
  unsafeAt) 
  where

import qualified Data.Massiv.Array as A
import Data.Bit (Bit(..))
import Data.Int
import Data.Word

data Array ix a where
  N :: !(A.Array A.D ix Word64) -> A.Array A.U ix Word64 -> Array ix Word64
  I :: !(A.Array A.D ix Int64) -> A.Array A.U ix Int64 -> Array ix Int64
  F :: !(A.Array A.D ix Double) -> A.Array A.U ix Double -> Array ix Double
  B :: !(A.Array A.D ix Bit) -> A.Array A.U ix Bit -> Array ix Bool

force :: A.Index ix => Array ix a -> Array ix a
force (N _ af) = N (A.map id af) af
force (I _ af) = I (A.map id af) af
force (F _ af) = F (A.map id af) af
force (B _ af) = B (A.map id af) af

unsafeAt :: A.Index ix => ix -> Array ix a -> a
unsafeAt ix (N _ af) = A.index' af ix
unsafeAt ix (I _ af) = A.index' af ix
unsafeAt ix (F _ af) = A.index' af ix
unsafeAt ix (B _ af) = unBit $ A.index' af ix

at :: A.Index ix => ix -> Array ix a -> Maybe a
at ix (N _ af) = A.index af ix
at ix (I _ af) = A.index af ix
at ix (F _ af) = A.index af ix
at ix (B _ af) = unBit <$> A.index af ix

size :: A.Index ix => Array ix a -> ix
size (N _ af) = A.unSz $ A.size af
size (I _ af) = A.unSz $ A.size af
size (F _ af) = A.unSz $ A.size af
size (B _ af) = A.unSz $ A.size af

natReplicate :: A.Index ix => ix -> Word64 -> Array ix Word64
natReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const n) 
  in N (delay a) a

intReplicate :: A.Index ix => ix -> Int64 -> Array ix Int64
intReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const n) 
  in I (delay a) a

floatReplicate :: A.Index ix => ix -> Double -> Array ix Double
floatReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const n) 
  in F (delay a) a

-- Pointwise less-than
floatPtLt :: A.Index ix => Array ix Double -> Array ix Double -> Array ix Bool
floatPtLt (F a1 _) (F a2 _) = let !a3 = A.map Bit (A.zipWith (<) a1 a2) in B a3 (A.compute a3)

floatPtLteq :: A.Index ix => Array ix Double -> Array ix Double -> Array ix Bool
floatPtLteq (F a1 _) (F a2 _) = let !a3 = A.map Bit (A.zipWith (<=) a1 a2) in B a3 (A.compute a3)

floatPtGteq :: A.Index ix => Array ix Double -> Array ix Double -> Array ix Bool
floatPtGteq (F a1 _) (F a2 _) = let !a3 = A.map Bit (A.zipWith (>=) a1 a2) in B a3 (A.compute a3)

floatPtGt :: A.Index ix => Array ix Double -> Array ix Double -> Array ix Bool
floatPtGt (F a1 _) (F a2 _) = let !a3 = A.map Bit (A.zipWith (>) a1 a2) in B a3 (A.compute a3)

floatPtEq :: A.Index ix => Array ix Double -> Array ix Double -> Array ix Bool
floatPtEq (F a1 _) (F a2 _) = let !a3 = A.map Bit (A.zipWith (==) a1 a2) in B a3 (A.compute a3)

floatSwitch :: A.Index ix => Array ix Bool -> Array ix Double -> Array ix Double -> Array ix Double
floatSwitch (B b _) (F a1 _) (F a2 _) = 
  let !a = A.zipWith3 (\(Bit b) a1 a2 -> if b then a1 else a2) b a1 a2 
  in F a (A.compute a)

boolReplicate :: A.Index ix => ix -> Bool -> Array ix Bool
boolReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const (Bit n))
  in B (delay a) a

delay :: A.Source sh ix a => A.Index ix => A.Array sh ix a -> A.Array A.D ix a
delay = A.map id
{-# INLINE delay #-}