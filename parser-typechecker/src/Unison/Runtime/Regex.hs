{-# Language BangPatterns, PatternSynonyms, ViewPatterns #-}

module Unison.Runtime.Regex where

import Prelude hiding (or,and)
import Data.Word
import Data.Bits (shiftL, shiftR)
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

--  , B "Regex.bytes" $ text --> regexOf bytes
--  , B "Regex.text" $ text --> regexOf text
--  , B "Regex.many" $ forall1 "a" (\a -> regexOf a --> regexOf a)
--  , B "Regex.few" $ forall1 "a" (\a -> regexOf a --> regexOf a)
--  , B "Regex.or" $ forall1 "a" (\a -> regexOf a --> regexOf a --> regexOf a)
--  , B "Regex.append" $ forall1 "a" (\a -> regexOf a --> regexOf a --> regexOf a)
--  , B "Regex.fail" $ forall1 "a" (\a -> regexOf a)
--  , B "Regex.capture" $ forall1 "a" (\a -> regexOf a --> regexOf a)

data Compiled
  = More {-# unpack #-} !Status (Int -> Word8 -> Compiled)

type Status = Int
pattern Waiting_s = 0
pattern Ok_s = 1
pattern Fail_s = 2
pattern Capture_s start <- (uncapture_s -> start)
{-# COMPLETE Waiting_s, Ok_s, Fail_s, Capture_s #-}

-- when interpreter encounters a capture, it should repeat the same
-- index for the continuation

uncapture_s :: Status -> Int
uncapture_s s = s `shiftR` 2

capture_s :: Int -> Status
capture_s from = from `shiftL` 2

ok_c, fail_c :: Compiled
ok_c = More Ok_s (\_ _ -> ok_c)
fail_c = More Fail_s (\_ _ -> fail_c)

bytes_c :: [Word8] -> Compiled
bytes_c [] = ok_c
bytes_c (h:t) =
  let ct = bytes_c t
  in More Waiting_s (\_ w -> if w == h then ct else fail_c)

many_c :: Compiled -> Compiled
many_c c = mr
  where
  mr = go c
  go (More Ok_s _) = mr
  go (More Fail_s _) = fail_c
  go (More Waiting_s k) = More Waiting_s (\i b -> go (k i b))
  go (More capture k) = More capture (\i b -> go (k i b))

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
