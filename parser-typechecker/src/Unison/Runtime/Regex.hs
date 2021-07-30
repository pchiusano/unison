module Unison.Runtime.Regex where

import Prelude hiding (or,and)
import Data.Word
import qualified Data.Vector as V

-- Machine = More (Word8 -> Machine) | Done Status
-- Done Int Int
-- it's just a Moore machine
-- and is pretty clear, just extend the accept states, converting them to failures
-- or calls both continuations, and if both say keep going, or those
data Regex
  = Fail
  | Ok Int Int
  | More IsOk (Word8 -> Regex)

type IsOk = Bool

or Fail r = r
or Ok (More _ k) = More True k
or Ok _ = Ok
or (More ok1 k1) (More ok2 k2) =
  More (ok1 || ok2) k12
  where k12 b = or (k1 b) (k2 b)

many Ok = Ok
many Fail = Fail
many r = mr
  where
  mr = go r
  go Ok = or Ok self
  go Fail = Fail
  go (More ok k) = More ok (go.k)

type IsAccept = Bool

allFail = V.replicate 256 Fail

dot = Or False (V.replicate 256 Succeed)
dotStar =
  let n = Or (V.replicate 256 n)
  in n

succeed = Or True allFail
fail = Or False allFail

lit :: [Word8] -> Regex
lit [] = succeed
lit (h:t) = Or False (allFail V.// [(fromIntegral h, lit t)])

and :: Regex -> Regex -> Regex
and Succeed r = r
and Fail _ = Fail
and (Or False bs) r = Or (V.map (`and` r) bs)

or :: Regex -> Regex -> Regex
or Fail r = r
or r Fail = r
or Succeed _ = Succeed
or _ Succeed = Succeed
or (Or v1) (Or v2) = Or (V.zipWith or v1 v2)

many :: Regex -> Regex
many Fail = Fail
many Succeed = Succeed
many (Or bs) = _hmm

