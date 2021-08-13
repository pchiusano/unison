{-# Language BangPatterns, PatternSynonyms, ViewPatterns #-}
module Unison.Util.Regex where

import Prelude hiding (or,and)

import Data.Sequence (Seq)
import Unison.Util.Bytes (Bytes)
import qualified Data.Sequence as Sequence
import qualified Unison.Util.Bytes as Bytes

--  , B "Regex.bytes" $ text --> regexOf bytes
--  , B "Regex.text" $ text --> regexOf text
--  , B "Regex.many" $ forall1 "a" (\a -> regexOf a --> regexOf a)
--  , B "Regex.few" $ forall1 "a" (\a -> regexOf a --> regexOf a)
--  , B "Regex.or" $ forall1 "a" (\a -> regexOf a --> regexOf a --> regexOf a)
--  , B "Regex.append" $ forall1 "a" (\a -> regexOf a --> regexOf a --> regexOf a)
--  , B "Regex.fail" $ forall1 "a" (\a -> regexOf a)
--  , B "Regex.capture" $ forall1 "a" (\a -> regexOf a --> regexOf a)

data Compiled txt
  = Fail
  | Ok (Seq txt) -- captures
  | More {-# unpack #-} !Int !(Maybe txt -> Compiled txt)

bytes_c :: Bytes -> Compiled Bytes
bytes_c bs | Bytes.null bs = Ok
bytes_c bs =
  More (Bytes.size bs)
       (\bs' -> if Just bs == bs' then Ok mempty else Fail)

many_c :: Compiled b -> Compiled b
many_c = go0
  where
  go0 !acc Fail = Fail
  go0 !acc (Ok caps) = Fail
  go0 !acc (More n k) = More n step where
    step Nothing = Ok acc
    step bs = go (k bs)
    where
      go Fail = Fail
      go (Ok caps) = go0 (acc <> caps) c
      go (More n k) = More n (\bs -> go (k bs))

or_c :: Compiled b -> Compiled b -> Compiled b
or_c Fail c = c
or_c c Fail = c
or_c Ok _ = Ok
or_c _ Ok = Ok

{-
or_c :: Compiled -> Compiled -> Compiled
or_c (More Ok_s _) _ = ok_c
or_c _ (More Ok_s _) = ok_c
or_c (More Fail_s _) c = c
or_c c (More Fail_s _) = c
or_c (More Waiting_s k1) (More s k2) = More s (\i b -> k1 i b `or_c` k2 i b)
or_c (More s k1) (More Waiting_s k2) = More s (\i b -> k1 i b `or_c` k2 i b)
or_c (More (Capture_s from1) k1) (More (Capture_s from2) k2) =
  More (capture_s (from1 `min` from2))
       (\i b -> k1 i b `or_c` k2 i b)

{-
data Regex txt
  = Fail Compiled
  | Concat Compiled (Seq (Regex txt))
  | Many Compiled (Regex txt)
  | Lit Compiled txt
  | Or Compiled (Regex txt) (Regex txt)
  | Capture Compiled (Regex txt)


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
-}
-}
