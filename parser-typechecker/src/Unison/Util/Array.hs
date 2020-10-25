{-# Language GADTs #-}
{-# Language BangPatterns #-}

module Unison.Util.Array (
  Array(..),
  at,
  force,
  natReplicate,
  intReplicate,
  floatReplicate,
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
  B :: !(A.Array A.D ix Bit) -> A.Array A.U ix Bit -> Array ix Bit

force :: A.Index ix => Array ix a -> Array ix a
force (N _ af) = N (A.map id af) af
force (I _ af) = I (A.map id af) af
force (F _ af) = F (A.map id af) af
force (B _ af) = B (A.map id af) af

unsafeAt :: A.Index ix => ix -> Array ix a -> a
unsafeAt ix (N _ af) = A.index' af ix
unsafeAt ix (I _ af) = A.index' af ix
unsafeAt ix (F _ af) = A.index' af ix
unsafeAt ix (B _ af) = A.index' af ix

at :: A.Index ix => ix -> Array ix a -> Maybe a
at ix (N _ af) = A.index af ix
at ix (I _ af) = A.index af ix
at ix (F _ af) = A.index af ix
at ix (B _ af) = A.index af ix

size :: A.Index ix => Array ix a -> ix
size (N _ af) = A.unSz $ A.size af
size (I _ af) = A.unSz $ A.size af
size (F _ af) = A.unSz $ A.size af
size (B _ af) = A.unSz $ A.size af

natReplicate :: A.Index ix => ix -> Word64 -> Array ix Word64
natReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const n) 
  in N (A.map id a) a

intReplicate :: A.Index ix => ix -> Int64 -> Array ix Int64
intReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const n) 
  in I (A.map id a) a

floatReplicate :: A.Index ix => ix -> Double -> Array ix Double
floatReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const n) 
  in F (A.map id a) a

boolReplicate :: A.Index ix => ix -> Bool -> Array ix Bit
boolReplicate ix n = 
  let !a = A.makeArray A.Seq (A.Sz ix) (const (Bit n))
  in B (A.map id a) a