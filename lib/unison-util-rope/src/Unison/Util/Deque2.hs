{-# LANGUAGE BangPatterns, UnboxedTuples, MagicHash, PatternSynonyms, ViewPatterns, TypeFamilies #-}
{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
-- | A strict finger tree.
--
-- * amortized O(1) 'cons', 'snoc', 'uncons', 'unsnoc' when a sequence is used
--   once; O(log n) in the worst case, which a program can hit repeatedly only
--   by going back to the same old version
-- * O(1) 'size'
-- * O(log n) 'lookup', 'append', 'take', 'drop'
-- * no laziness anywhere: every field is strict, nothing is a thunk
--
-- The shape is Hinze and Paterson's: a prefix digit, a middle that is a
-- sequence of nodes, a suffix digit.  The differences are that the middle is
-- strict, digits are short lists of up to 'maxD' items that may be empty, and
-- nodes are mostly eight wide.  There are no other invariants: a digit that
-- fills up sheds a node of eight into the middle, and an operation that needs
-- an item from an empty digit takes a node out of the middle.
module Unison.Util.Deque2
  ( -- * The type
    Deque
  , Seq
  , pattern Empty, pattern (:<|), pattern (:|>)
    -- * Construction
  , empty, singleton
  , cons, snoc, append
  , (<|), (|>), (><)
  , fromList, fromListN, fromFunction
    -- * Queries
  , size, length, null
  , lookup
  , toList
    -- * Taking apart
  , uncons, unsnoc
  , take, drop, splitAt
    -- * Whole-sequence transformations (these rebuild the sequence)
  , sort, sortBy, unstableSort, unstableSortBy
  , intersperse, reverse
    -- * For tests
  , valid
  ) where

import Prelude hiding (take, drop, lookup, length, null, splitAt, reverse)
import Control.DeepSeq (NFData (..))
import Control.Monad (unless, when)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import GHC.Exts (Int (I#), Int#)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified GHC.Exts as Exts

------------------------------------------------------------------------
-- Strict lists (digits) and nodes

data SList a = SNil | SCons !a !(SList a)

-- A node's first field is the number of leaves under it.  Digits that fill
-- up make nodes of eight; 'append' makes the smaller ones.
data Node a
  = N8 !Int !a !a !a !a !a !a !a !a
  | N7 !Int !a !a !a !a !a !a !a
  | N6 !Int !a !a !a !a !a !a
  | N5 !Int !a !a !a !a !a
  | N4 !Int !a !a !a !a
  | N3 !Int !a !a !a
  | N2 !Int !a !a

nodeSize :: Node a -> Int
nodeSize (N8 n _ _ _ _ _ _ _ _) = n
nodeSize (N7 n _ _ _ _ _ _ _) = n
nodeSize (N6 n _ _ _ _ _ _) = n
nodeSize (N5 n _ _ _ _ _) = n
nodeSize (N4 n _ _ _ _) = n
nodeSize (N3 n _ _ _) = n
nodeSize (N2 n _ _) = n
{-# INLINE nodeSize #-}

nodeArity :: Node a -> Int
nodeArity (N8 {}) = 8
nodeArity (N7 {}) = 7
nodeArity (N6 {}) = 6
nodeArity (N5 {}) = 5
nodeArity (N4 {}) = 4
nodeArity (N3 {}) = 3
nodeArity (N2 {}) = 2
{-# INLINE nodeArity #-}

-- a node's children in sequence order, and in reverse
nodeToFwd, nodeToBwd :: Node a -> SList a
nodeToFwd (N8 _ a b c d e f g h) = SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h SNil)))))))
nodeToFwd (N7 _ a b c d e f g) = SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g SNil))))))
nodeToFwd (N6 _ a b c d e f) = SCons a (SCons b (SCons c (SCons d (SCons e (SCons f SNil)))))
nodeToFwd (N5 _ a b c d e) = SCons a (SCons b (SCons c (SCons d (SCons e SNil))))
nodeToFwd (N4 _ a b c d) = SCons a (SCons b (SCons c (SCons d SNil)))
nodeToFwd (N3 _ a b c) = SCons a (SCons b (SCons c SNil))
nodeToFwd (N2 _ a b) = SCons a (SCons b SNil)
nodeToBwd (N8 _ a b c d e f g h) = SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a SNil)))))))
nodeToBwd (N7 _ a b c d e f g) = SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a SNil))))))
nodeToBwd (N6 _ a b c d e f) = SCons f (SCons e (SCons d (SCons c (SCons b (SCons a SNil)))))
nodeToBwd (N5 _ a b c d e) = SCons e (SCons d (SCons c (SCons b (SCons a SNil))))
nodeToBwd (N4 _ a b c d) = SCons d (SCons c (SCons b (SCons a SNil)))
nodeToBwd (N3 _ a b c) = SCons c (SCons b (SCons a SNil))
nodeToBwd (N2 _ a b) = SCons b (SCons a SNil)

-- the k-th child (0-based)
nodeAt :: Int -> Node a -> a
nodeAt k (N8 _ a b c d e f g h) = case k of { 0 -> a; 1 -> b; 2 -> c; 3 -> d; 4 -> e; 5 -> f; 6 -> g; _ -> h }
nodeAt k (N7 _ a b c d e f g) = case k of { 0 -> a; 1 -> b; 2 -> c; 3 -> d; 4 -> e; 5 -> f; _ -> g }
nodeAt k (N6 _ a b c d e f) = case k of { 0 -> a; 1 -> b; 2 -> c; 3 -> d; 4 -> e; _ -> f }
nodeAt k (N5 _ a b c d e) = case k of { 0 -> a; 1 -> b; 2 -> c; 3 -> d; _ -> e }
nodeAt k (N4 _ a b c d) = case k of { 0 -> a; 1 -> b; 2 -> c; _ -> d }
nodeAt k (N3 _ a b c) = case k of { 0 -> a; 1 -> b; _ -> c }
nodeAt k (N2 _ a b) = case k of { 0 -> a; _ -> b }

-- a node of the first k items of a list, and the rest of the list; the
-- items are leaves
leafNode :: Int -> SList a -> (# Node a, SList a #)
leafNode 8 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h r)))))))) = (# N8 8 a b c d e f g h, r #)
leafNode 7 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g r))))))) = (# N7 7 a b c d e f g, r #)
leafNode 6 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f r)))))) = (# N6 6 a b c d e f, r #)
leafNode 5 (SCons a (SCons b (SCons c (SCons d (SCons e r))))) = (# N5 5 a b c d e, r #)
leafNode 4 (SCons a (SCons b (SCons c (SCons d r)))) = (# N4 4 a b c d, r #)
leafNode 3 (SCons a (SCons b (SCons c r))) = (# N3 3 a b c, r #)
leafNode 2 (SCons a (SCons b r)) = (# N2 2 a b, r #)
leafNode _ _ = error "Deque2: short list"

-- the same where the items are nodes
innerNode :: Int -> SList (Node a) -> (# Node (Node a), SList (Node a) #)
innerNode 8 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h r)))))))) = (# N8 (nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f + nodeSize g + nodeSize h) a b c d e f g h, r #)
innerNode 7 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g r))))))) = (# N7 (nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f + nodeSize g) a b c d e f g, r #)
innerNode 6 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f r)))))) = (# N6 (nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f) a b c d e f, r #)
innerNode 5 (SCons a (SCons b (SCons c (SCons d (SCons e r))))) = (# N5 (nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e) a b c d e, r #)
innerNode 4 (SCons a (SCons b (SCons c (SCons d r)))) = (# N4 (nodeSize a + nodeSize b + nodeSize c + nodeSize d) a b c d, r #)
innerNode 3 (SCons a (SCons b (SCons c r))) = (# N3 (nodeSize a + nodeSize b + nodeSize c) a b c, r #)
innerNode 2 (SCons a (SCons b r)) = (# N2 (nodeSize a + nodeSize b) a b, r #)
innerNode _ _ = error "Deque2: short list"

-- the first k children, back to front
nodeTakeRev :: Int -> Node a -> SList a
nodeTakeRev k nd = go 0 SNil
  where go !i acc | i == k    = acc
                  | otherwise = go (i + 1) (SCons (nodeAt i nd) acc)

-- all but the first k children, front to back
nodeDropFwd :: Int -> Node a -> SList a
nodeDropFwd k nd = go k
  where !n = nodeArity nd
        go !i | i >= n    = SNil
              | otherwise = SCons (nodeAt i nd) (go (i + 1))

takeS :: Int -> SList a -> SList a
takeS 0 _            = SNil
takeS k (SCons x r)  = SCons x (takeS (k - 1) r)
takeS _ SNil         = SNil

dropS :: Int -> SList a -> SList a
dropS 0 l            = l
dropS k (SCons _ r)  = dropS (k - 1) r
dropS _ SNil         = SNil

appendS :: SList a -> SList a -> SList a
appendS SNil ys         = ys
appendS (SCons x r) ys  = SCons x (appendS r ys)

lenS :: SList a -> Int
lenS = go 0 where go !n SNil = n
                  go !n (SCons _ r) = go (n + 1) r

revS :: SList a -> SList a
revS l = revOnto l SNil

-- the first list reversed, then the second
revOnto :: SList a -> SList a -> SList a
revOnto SNil acc        = acc
revOnto (SCons x r) acc = revOnto r (SCons x acc)

nthS :: Int -> SList a -> a
nthS k (SCons x r) | k == 0    = x
                   | otherwise = nthS (k - 1) r
nthS _ SNil = error "Deque2: index past the end of a digit"

sumS :: SList (Node a) -> Int
sumS = go 0 where go !n SNil = n
                  go !n (SCons x r) = go (n + nodeSize x) r

------------------------------------------------------------------------
-- The tree.
--
-- A prefix runs front to back and a suffix back to front, so the item at the
-- outer end of either is the head of its list.
--
-- The first field of 'Deep' and 'MDeep' packs three numbers: the number of
-- leaves in the tree (bits 8 and up), the number of items in the suffix (bits
-- 4 to 7) and in the prefix (bits 0 to 3).  'MDeep' also has the number of
-- leaves under its prefix, which 'lookup' needs to step over the prefix
-- without reading it.
--
-- 'Mid' is the same thing as 'Deque' one level down, where the items are
-- nodes.  Having two types costs nothing, since every function needs a
-- version for leaves (which all have size one) and a version for nodes.

data Deque a
  = Nil
  | Deep !Int !(SList a) !(Mid a) !(SList a)

data Mid a
  = MNil
  | MDeep !Int !Int !(SList (Node a)) !(Mid (Node a)) !(SList (Node a))

-- the most items a digit holds; it must fit in four bits and be at least nine
maxD :: Int
maxD = 10

mk :: Int -> Int -> Int -> Int          -- leaves, prefix count, suffix count
mk n p s = (n `unsafeShiftL` 8) .|. (s `unsafeShiftL` 4) .|. p
{-# INLINE mk #-}

tsize, tpc, tsc :: Int -> Int
tsize t = t `unsafeShiftR` 8
tpc t = t .&. 15
tsc t = (t `unsafeShiftR` 4) .&. 15
{-# INLINE tsize #-}
{-# INLINE tpc #-}
{-# INLINE tsc #-}

sizeM :: Mid a -> Int
sizeM MNil = 0
sizeM (MDeep t _ _ _ _) = tsize t
{-# INLINE sizeM #-}

empty :: Deque a
empty = Nil

singleton :: a -> Deque a
singleton x = Deep 0x101 (SCons x SNil) MNil SNil

size :: Deque a -> Int
size Nil = 0
size (Deep t _ _ _) = tsize t
{-# INLINE size #-}

------------------------------------------------------------------------
-- Adding at the ends.  The wrappers are INLINE and the paths that touch the
-- middle are not, so that a call site gets only the common case.

cons :: a -> Deque a -> Deque a
cons x Nil = Deep 0x101 (SCons x SNil) MNil SNil
cons x (Deep t pr m sf)
  | t .&. 15 < maxD = Deep (t + 0x101) (SCons x pr) m sf
  | otherwise       = consFull x t pr m sf
{-# INLINE cons #-}

snoc :: Deque a -> a -> Deque a
snoc Nil x = Deep 0x110 SNil MNil (SCons x SNil)
snoc (Deep t pr m sf) x
  | t .&. 0xF0 < maxD * 16 = Deep (t + 0x110) pr m (SCons x sf)
  | otherwise              = snocFull x t pr m sf
{-# INLINE snoc #-}

-- A full prefix keeps its two outermost items and sheds the other eight.
consFull :: a -> Int -> SList a -> Mid a -> SList a -> Deque a
consFull x !t (SCons p1 (SCons p2 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h _)))))))))) m sf
  = Deep (t + 0x100 - 7) (SCons x (SCons p1 (SCons p2 SNil))) (consM (N8 8 a b c d e f g h) m) sf
consFull _ _ _ _ _ = error "Deque2.cons: short prefix"
{-# NOINLINE consFull #-}

snocFull :: a -> Int -> SList a -> Mid a -> SList a -> Deque a
snocFull x !t pr m (SCons s1 (SCons s2 (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a _))))))))))
  = Deep (t + 0x100 - 0x70) pr (snocM m (N8 8 a b c d e f g h)) (SCons x (SCons s1 (SCons s2 SNil)))
snocFull _ _ _ _ _ = error "Deque2.snoc: short suffix"
{-# NOINLINE snocFull #-}

consM :: Node a -> Mid a -> Mid a
consM n MNil = let !s = nodeSize n in MDeep (mk s 1 0) s (SCons n SNil) MNil SNil
consM n (MDeep t ps pr m sf)
  | t .&. 15 < maxD = MDeep (t + (s `unsafeShiftL` 8) + 1) (ps + s) (SCons n pr) m sf
  | otherwise = case pr of
      SCons p1 (SCons p2 (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h _))))))))) ->
        let !sn = nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f + nodeSize g + nodeSize h
        in MDeep (t + (s `unsafeShiftL` 8) - 7) (ps + s - sn) (SCons n (SCons p1 (SCons p2 SNil)))
                 (consM (N8 sn a b c d e f g h) m) sf
      _ -> error "Deque2.consM: short prefix"
  where !s = nodeSize n

snocM :: Mid a -> Node a -> Mid a
snocM MNil n = MDeep (mk (nodeSize n) 0 1) 0 SNil MNil (SCons n SNil)
snocM (MDeep t ps pr m sf) n
  | t .&. 0xF0 < maxD * 16 = MDeep (t + (s `unsafeShiftL` 8) + 0x10) ps pr m (SCons n sf)
  | otherwise = case sf of
      SCons s1 (SCons s2 (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a _))))))))) ->
        let !sn = nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f + nodeSize g + nodeSize h
        in MDeep (t + (s `unsafeShiftL` 8) - 0x70) ps pr (snocM m (N8 sn a b c d e f g h))
                 (SCons n (SCons s1 (SCons s2 SNil)))
      _ -> error "Deque2.snocM: short suffix"
  where !s = nodeSize n

------------------------------------------------------------------------
-- Removing at the ends

uncons :: Deque a -> Maybe (a, Deque a)
uncons Nil = Nothing
uncons (Deep t pr m sf) = case pr of
  SCons x pr' | t < 0x200 -> Just (x, Nil)
              | otherwise -> Just (x, Deep (t - 0x101) pr' m sf)
  SNil -> unconsSlow t m sf
{-# INLINE uncons #-}

unsnoc :: Deque a -> Maybe (Deque a, a)
unsnoc Nil = Nothing
unsnoc (Deep t pr m sf) = case sf of
  SCons x sf' | t < 0x200 -> Just (Nil, x)
              | otherwise -> Just (Deep (t - 0x110) pr m sf', x)
  SNil -> unsnocSlow t pr m
{-# INLINE unsnoc #-}

-- The prefix is empty.  With a middle, its first node becomes the prefix.
-- Without one, everything is in the suffix: half of it moves over.
unconsSlow :: Int -> Mid a -> SList a -> Maybe (a, Deque a)
unconsSlow !t MNil sf
  | sc == 1 = case sf of
      SCons x _ -> Just (x, Nil)
      SNil -> error "Deque2.uncons: empty"
  | otherwise =
      let !q = (sc - 1) `div` 2
          !p = sc - 1 - q
      in case revS (dropS q sf) of
           SCons x pr -> Just (x, Deep (mk (sc - 1) p q) pr MNil (takeS q sf))
           SNil -> error "Deque2.uncons: empty"
  where !sc = tsc t
unconsSlow t m sf = case unconsM m of
  (# nd, m' #) -> case nd of
    N8 _ a b c d e f g h ->
      Just (a, Deep (t - 0x100 + 7) (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h SNil))))))) m' sf)
    _ -> Just (nodeAt 0 nd, Deep (t - 0x100 + (nodeArity nd - 1)) (nodeDropFwd 1 nd) m' sf)
{-# NOINLINE unconsSlow #-}

unsnocSlow :: Int -> SList a -> Mid a -> Maybe (Deque a, a)
unsnocSlow !t pr MNil
  | pc == 1 = case pr of
      SCons x _ -> Just (Nil, x)
      SNil -> error "Deque2.unsnoc: empty"
  | otherwise =
      let !p = (pc - 1) `div` 2
          !q = pc - 1 - p
      in case revS (dropS p pr) of
           SCons x sf -> Just (Deep (mk (pc - 1) p q) (takeS p pr) MNil sf, x)
           SNil -> error "Deque2.unsnoc: empty"
  where !pc = tpc t
unsnocSlow t pr m = case unsnocM m of
  (# m', nd #) -> case nd of
    N8 _ a b c d e f g h ->
      Just (Deep (t - 0x100 + 0x70) pr m' (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a SNil))))))), h)
    _ -> let !k = nodeArity nd - 1
         in Just (Deep (t - 0x100 + (k `unsafeShiftL` 4)) pr m' (nodeTakeRev k nd), nodeAt k nd)
{-# NOINLINE unsnocSlow #-}

-- the first node and the rest; the argument is not empty
unconsM :: Mid a -> (# Node a, Mid a #)
unconsM MNil = error "Deque2.unconsM: empty"
unconsM (MDeep t ps pr m sf) = case pr of
  SCons n pr' ->
    let !s = nodeSize n
        !t' = t - (s `unsafeShiftL` 8) - 1
    in if t' < 0x100 then (# n, MNil #) else (# n, MDeep t' (ps - s) pr' m sf #)
  SNil -> case m of
    MNil ->
      let !sc = tsc t
          !q = (sc - 1) `div` 2
          !p = sc - 1 - q
      in case revS (dropS q sf) of
           SCons n pr'
             | sc == 1   -> (# n, MNil #)
             | otherwise -> (# n, MDeep (mk (tsize t - nodeSize n) p q) (sumS pr') pr' MNil (takeS q sf) #)
           SNil -> error "Deque2.unconsM: empty"
    _ -> case unconsM m of
      (# nn, m' #) -> case nodeToFwd nn of
        SCons n pr' ->
          let !s = nodeSize n
          in (# n, MDeep (t - (s `unsafeShiftL` 8) + (nodeArity nn - 1)) (nodeSize nn - s) pr' m' sf #)
        SNil -> error "Deque2.unconsM: empty node"

unsnocM :: Mid a -> (# Mid a, Node a #)
unsnocM MNil = error "Deque2.unsnocM: empty"
unsnocM (MDeep t ps pr m sf) = case sf of
  SCons n sf' ->
    let !t' = t - (nodeSize n `unsafeShiftL` 8) - 0x10
    in if t' < 0x100 then (# MNil, n #) else (# MDeep t' ps pr m sf', n #)
  SNil -> case m of
    MNil ->
      let !pc = tpc t
          !p = (pc - 1) `div` 2
          !q = pc - 1 - p
      in case revS (dropS p pr) of
           SCons n sf'
             | pc == 1   -> (# MNil, n #)
             | otherwise ->
                 let !pr' = takeS p pr
                 in (# MDeep (mk (tsize t - nodeSize n) p q) (sumS pr') pr' MNil sf', n #)
           SNil -> error "Deque2.unsnocM: empty"
    _ -> case unsnocM m of
      (# m', nn #) -> case nodeToBwd nn of
        SCons n sf' ->
          (# MDeep (t - (nodeSize n `unsafeShiftL` 8) + ((nodeArity nn - 1) `unsafeShiftL` 4)) ps pr m' sf', n #)
        SNil -> error "Deque2.unsnocM: empty node"

------------------------------------------------------------------------
-- Lookup.  Results flow upward in unboxed tuples: a level hands back the
-- node holding the leaf and the leaf's offset in it, and the level above
-- picks the child.

lookup :: Int -> Deque a -> Maybe a
lookup !_ Nil = Nothing
lookup i (Deep t pr m sf)
  | i < 0 || i >= n = Nothing
  | i < pc          = let !r = nthS i pr in Just r
  | j < tsc t       = let !r = nthS j sf in Just r
  | otherwise       = case lookM 0 (i - pc) m of
                        (# nd, off# #) -> let !r = nodeAt (I# off#) nd in Just r
  where !n = tsize t
        !pc = tpc t
        !j = n - 1 - i

-- sh: a full child of this level's nodes has 2^sh leaves
lookM :: Int -> Int -> Mid a -> (# Node a, Int# #)
lookM !_ !_ MNil = error "Deque2.lookup: empty middle"
lookM sh i (MDeep t ps pr m sf)
  | i < ps    = scanFwd i pr
  | i' < ms   = case lookM (sh + 3) i' m of (# nn, off# #) -> childAt (sh + 3) (I# off#) nn
  | otherwise = scanBwd (tsize t - 1 - i) sf
  where !i' = i - ps
        !ms = sizeM m

scanFwd :: Int -> SList (Node a) -> (# Node a, Int# #)
scanFwd !i (SCons n r)
  | i < s     = case i of I# i# -> (# n, i# #)
  | otherwise = scanFwd (i - s) r
  where !s = nodeSize n
scanFwd _ SNil = error "Deque2.lookup: past the end of a prefix"

-- r: the number of leaves after the one looked for
scanBwd :: Int -> SList (Node a) -> (# Node a, Int# #)
scanBwd !r (SCons n rest)
  | r < s     = case s - 1 - r of I# i# -> (# n, i# #)
  | otherwise = scanBwd (r - s) rest
  where !s = nodeSize n
scanBwd _ SNil = error "Deque2.lookup: past the end of a suffix"

-- the child holding leaf number off; a full child has 2^sh leaves
childAt :: Int -> Int -> Node (Node a) -> (# Node a, Int# #)
childAt !sh !off nn
  | nodeSize nn == 8 `unsafeShiftL` sh =
      case off .&. ((1 `unsafeShiftL` sh) - 1) of
        I# o# -> (# nodeAt (off `unsafeShiftR` sh) nn, o# #)
  | otherwise = go 0 off
  where go !i !o = let !c = nodeAt i nn
                       !s = nodeSize c
                   in if o < s then (case o of I# o# -> (# c, o# #)) else go (i + 1) (o - s)

------------------------------------------------------------------------
-- take and drop.  The cut falls in a prefix, in a suffix, or in the middle;
-- in the middle, the level below hands back the node the cut falls in and
-- the part of itself that is wholly on the side being kept, and the kept
-- children of that node become this level's digit on the cut side.

take :: Int -> Deque a -> Deque a
take !_ Nil = Nil
take i d@(Deep t pr m sf)
  | i <= 0      = Nil
  | i >= n      = d
  | i <= pc     = Deep (mk i i 0) (takeS i pr) MNil SNil
  | i >= n - sc = let !k = i - (n - sc) in Deep (mk i pc k) pr m (dropS (sc - k) sf)
  | otherwise   = case takeM (i - pc) m of
      (# m', nd, k# #) -> let !k = I# k# in Deep (mk i pc k) pr m' (nodeTakeRev k nd)
  where !n = tsize t
        !pc = tpc t
        !sc = tsc t

drop :: Int -> Deque a -> Deque a
drop !_ Nil = Nil
drop i d@(Deep t pr m sf)
  | i <= 0      = d
  | i >= n      = Nil
  | i >= n - sc = let !r = n - i in Deep (mk r 0 r) SNil MNil (takeS r sf)
  | i <= pc     = Deep (mk (n - i) (pc - i) sc) (dropS i pr) m sf
  | otherwise   = case dropM (i - pc) m of
      (# nd, k#, m' #) ->
        let !k = I# k#
        in Deep (mk (n - i) (nodeArity nd - k) sc) (nodeDropFwd k nd) m' sf
  where !n = tsize t
        !pc = tpc t
        !sc = tsc t

-- | @splitAt i d = (take i d, drop i d)@
splitAt :: Int -> Deque a -> (Deque a, Deque a)
splitAt i d = (take i d, drop i d)

-- The first j leaves, 0 < j <= size: the node holding the last of them, the
-- number of its leaves that are among them, and everything before that node.
takeM :: Int -> Mid a -> (# Mid a, Node a, Int# #)
takeM !_ MNil = error "Deque2.take: empty middle"
takeM j (MDeep t ps pr m sf)
  | j <= ps = goP 0 0 pr
  | j <= ps + ms = case takeM (j - ps) m of
      (# m', nn, k# #) -> goC m' (I# k#) 0 0 SNil (nodeToFwd nn)
  | otherwise = goS (n - j) sc sf
  where
    !n = tsize t
    !pc = tpc t
    !sc = tsc t
    !ms = sizeM m
    -- q whole nodes of the prefix, with sb leaves, come before the cut
    goP !q !sb (SCons nd r)
      | j <= sb + nodeSize nd =
          case j - sb of
            I# k# | q == 0    -> (# MNil, nd, k# #)
                  | otherwise -> (# MDeep (mk sb q 0) sb (takeS q pr) MNil SNil, nd, k# #)
      | otherwise = goP (q + 1) (sb + nodeSize nd) r
    goP _ _ SNil = error "Deque2.take: past the end of a prefix"
    -- the children of the node the level below cut in: q of them, with sb
    -- leaves, are kept whole and make the new suffix
    goC m' !k !q !sb acc (SCons c r)
      | k <= sb + nodeSize c =
          let !total = ps + sizeM m' + sb
          in case k - sb of
               I# k# | total == 0 -> (# MNil, c, k# #)
                     | otherwise  -> (# MDeep (mk total pc q) ps pr m' acc, c, k# #)
      | otherwise = goC m' k (q + 1) (sb + nodeSize c) (SCons c acc) r
    goC _ _ _ _ _ SNil = error "Deque2.take: past the end of a node"
    -- r leaves are to go from the back; cnt nodes of the suffix are left
    goS !r !cnt (SCons nd rest)
      | r >= s = goS (r - s) (cnt - 1) rest
      | otherwise =
          let !keep = s - r
              !total = j - keep
          in case keep of
               I# k# | total == 0 -> (# MNil, nd, k# #)
                     | otherwise  -> (# MDeep (mk total pc (cnt - 1)) ps pr m rest, nd, k# #)
      where !s = nodeSize nd
    goS _ _ SNil = error "Deque2.take: past the end of a suffix"

-- All but the first j leaves, 0 <= j < size: the node holding leaf j, the
-- number of its leaves to drop, and everything after that node.
dropM :: Int -> Mid a -> (# Node a, Int#, Mid a #)
dropM !_ MNil = error "Deque2.drop: empty middle"
dropM j (MDeep t ps pr m sf)
  | j < ps = goP j pc ps n pr
  | j < ps + ms = case dropM (j - ps) m of
      (# nn, k#, m' #) -> goC m' (I# k#) (nodeToFwd nn)
  | otherwise = goS (n - 1 - j) 0 0 sf
  where
    !n = tsize t
    !pc = tpc t
    !sc = tsc t
    !ms = sizeM m
    -- k leaves still to drop; the prefix has cnt nodes and psz leaves left,
    -- the tree tot leaves
    goP !k !cnt !psz !tot (SCons nd r)
      | k >= s = goP (k - s) (cnt - 1) (psz - s) (tot - s) r
      | otherwise =
          case k of
            I# k# | tot == s  -> (# nd, k#, MNil #)
                  | otherwise -> (# nd, k#, MDeep (mk (tot - s) (cnt - 1) sc) (psz - s) r m sf #)
      where !s = nodeSize nd
    goP _ _ _ _ SNil = error "Deque2.drop: past the end of a prefix"
    -- the children of the node the level below cut in: those after the one
    -- holding the cut make the new prefix
    goC m' !k (SCons c r)
      | k >= nodeSize c = goC m' (k - nodeSize c) r
      | otherwise =
          let !sa = sumS r
              !total = sa + sizeM m' + (n - ps - ms)
          in case k of
               I# k# | total == 0 -> (# c, k#, MNil #)
                     | otherwise  -> (# c, k#, MDeep (mk total (lenS r) sc) sa r m' sf #)
    goC _ _ SNil = error "Deque2.drop: past the end of a node"
    -- r leaves come after leaf j; c nodes of the suffix, with sa leaves, are
    -- wholly after it
    goS !r !c !sa (SCons nd rest)
      | r >= s = goS (r - s) (c + 1) (sa + s) rest
      | otherwise =
          case s - 1 - r of
            I# k# | c == 0    -> (# nd, k#, MNil #)
                  | otherwise -> (# nd, k#, MDeep (mk sa 0 c) 0 SNil MNil (takeS c sf) #)
      where !s = nodeSize nd
    goS _ _ _ SNil = error "Deque2.drop: past the end of a suffix"

------------------------------------------------------------------------
-- Append.  When both sides have a middle, the digits that end up inside (the
-- left side's suffix and the right side's prefix) are packed into nodes and
-- handed down between the two middles.  When one side has no middle it has
-- at most two digits' worth of items, which join the other side's near digit;
-- what does not fit goes into the other side's middle in nodes of eight.

append :: Deque a -> Deque a -> Deque a
append Nil b = b
append a Nil = a
append a@(Deep t1 pr1 m1 sf1) b@(Deep t2 pr2 m2 sf2) = case m1 of
  MNil | MNil <- m2, tsc t1 + tsize t2 <= maxD ->
    Deep (mk n (tpc t1) (tsc t1 + tsize t2)) pr1 MNil (appendS sf2 (revOnto pr2 sf1))
  MNil ->
    let !c = tsize t1 + tpc t2
        !f = appendS pr1 (revOnto sf1 pr2)
    in if c <= maxD
         then Deep (mk n c (tsc t2)) f m2 sf2
         else let !k = (c - maxD + 7) `unsafeShiftR` 3
                  !p = c - 8 * k
              in Deep (mk n p (tsc t2)) (takeS p f) (consLeaves8 k (dropS p f) m2) sf2
  _ -> case m2 of
    MNil ->
      let !c = tsc t1 + tsize t2
          !r = appendS sf2 (revOnto pr2 sf1)
      in if c <= maxD
           then Deep (mk n (tpc t1) c) pr1 m1 r
           else let !k = (c - maxD + 7) `unsafeShiftR` 3
                    !q = c - 8 * k
                in Deep (mk n (tpc t1) q) pr1 (snocLeaves8 k (dropS q r) m1) (takeS q r)
    _ | c == 1 -> if tsc t1 == 0 then append (refillBack t1 pr1 m1) b
                                 else append a (refillFront t2 m2 sf2)
      | otherwise ->
          let !ns = packLeaves c (revOnto sf1 pr2)
          in Deep (mk n (tpc t1) (tsc t2)) pr1 (appM m1 ns m2) sf2
      where !c = tsc t1 + tpc t2
  where !n = tsize t1 + tsize t2

-- 8k items, front to back, go on the front of a middle as k nodes
consLeaves8 :: Int -> SList a -> Mid a -> Mid a
consLeaves8 0 _ m = m
consLeaves8 k (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h r)))))))) m =
  consM (N8 8 a b c d e f g h) (consLeaves8 (k - 1) r m)
consLeaves8 _ _ _ = error "Deque2.append: short list"

-- 8k items, back to front, go on the back of a middle as k nodes
snocLeaves8 :: Int -> SList a -> Mid a -> Mid a
snocLeaves8 0 _ m = m
snocLeaves8 k (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a r)))))))) m =
  snocM (snocLeaves8 (k - 1) r m) (N8 8 a b c d e f g h)
snocLeaves8 _ _ _ = error "Deque2.append: short list"

consNodes8 :: Int -> SList (Node a) -> Mid (Node a) -> Mid (Node a)
consNodes8 0 _ m = m
consNodes8 k (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h r)))))))) m =
  let !s = nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f + nodeSize g + nodeSize h
  in consM (N8 s a b c d e f g h) (consNodes8 (k - 1) r m)
consNodes8 _ _ _ = error "Deque2.append: short list"

snocNodes8 :: Int -> SList (Node a) -> Mid (Node a) -> Mid (Node a)
snocNodes8 0 _ m = m
snocNodes8 k (SCons h (SCons g (SCons f (SCons e (SCons d (SCons c (SCons b (SCons a r)))))))) m =
  let !s = nodeSize a + nodeSize b + nodeSize c + nodeSize d + nodeSize e + nodeSize f + nodeSize g + nodeSize h
  in snocM (snocNodes8 (k - 1) r m) (N8 s a b c d e f g h)
snocNodes8 _ _ _ = error "Deque2.append: short list"

consEachM :: SList (Node a) -> Mid a -> Mid a
consEachM SNil d = d
consEachM (SCons x r) d = consEachM r (consM x d)

snocEachM :: Mid a -> SList (Node a) -> Mid a
snocEachM d SNil = d
snocEachM d (SCons x r) = snocEachM (snocM d x) r

-- an empty digit gets the nearest node of a middle that is not empty
refillBack :: Int -> SList a -> Mid a -> Deque a
refillBack t pr m = case unsnocM m of
  (# m', nd #) -> Deep (t + (nodeArity nd `unsafeShiftL` 4)) pr m' (nodeToBwd nd)

refillFront :: Int -> Mid a -> SList a -> Deque a
refillFront t m sf = case unconsM m of
  (# nd, m' #) -> Deep (t + nodeArity nd) (nodeToFwd nd) m' sf

-- the nodes between the two middles run front to back
appM :: Mid a -> SList (Node a) -> Mid a -> Mid a
appM MNil ns b = consEachM (revS ns) b
appM a ns MNil = snocEachM a ns
appM a@(MDeep t1 ps1 pr1 m1 sf1) ns b@(MDeep t2 ps2 pr2 m2 sf2) = case m1 of
  MNil ->
    let !c = tpc t1 + tsc t1 + k + tpc t2
        !f = appendS pr1 (revOnto sf1 (appendS ns pr2))
    in if c <= maxD
         then MDeep (mk n c (tsc t2)) (tsize t1 + sns + ps2) f m2 sf2
         else let !kk = (c - maxD + 7) `unsafeShiftR` 3
                  !p = c - 8 * kk
                  !pr' = takeS p f
              in MDeep (mk n p (tsc t2)) (sumS pr') pr' (consNodes8 kk (dropS p f) m2) sf2
  _ -> case m2 of
    MNil ->
      let !c = tsc t1 + k + tpc t2 + tsc t2
          !r = appendS sf2 (revOnto pr2 (revOnto ns sf1))
      in if c <= maxD
           then MDeep (mk n (tpc t1) c) ps1 pr1 m1 r
           else let !kk = (c - maxD + 7) `unsafeShiftR` 3
                    !q = c - 8 * kk
                in MDeep (mk n (tpc t1) q) ps1 pr1 (snocNodes8 kk (dropS q r) m1) (takeS q r)
    _ | c == 1 ->
          if tsc t1 == 0
            then case unsnocM m1 of
              (# m1', nd #) ->
                appM (MDeep (t1 + (nodeArity nd `unsafeShiftL` 4)) ps1 pr1 m1' (nodeToBwd nd)) ns b
            else case unconsM m2 of
              (# nd, m2' #) ->
                appM a ns (MDeep (t2 + nodeArity nd) (nodeSize nd) (nodeToFwd nd) m2' sf2)
      | otherwise ->
          let !nns = packNodes c (revOnto sf1 (appendS ns pr2))
          in MDeep (mk n (tpc t1) (tsc t2)) ps1 pr1 (appM m1 nns m2) sf2
      where !c = tsc t1 + k + tpc t2
  where !k = lenS ns
        !sns = sumS ns
        !n = tsize t1 + sns + tsize t2

-- c items, front to back, as nodes: eights while that leaves none or at
-- least two, then one node of what is left (nine make a five and a four).
-- c is not 1.
packLeaves :: Int -> SList a -> SList (Node a)
packLeaves c l
  | c == 0 = SNil
  | c == 9 = case leafNode 5 l of (# x, r #) -> case leafNode 4 r of (# y, _ #) -> SCons x (SCons y SNil)
  | c <= 8 = case leafNode c l of (# x, _ #) -> SCons x SNil
  | otherwise = case leafNode 8 l of (# x, r #) -> SCons x (packLeaves (c - 8) r)

packNodes :: Int -> SList (Node a) -> SList (Node (Node a))
packNodes c l
  | c == 0 = SNil
  | c == 9 = case innerNode 5 l of (# x, r #) -> case innerNode 4 r of (# y, _ #) -> SCons x (SCons y SNil)
  | c <= 8 = case innerNode c l of (# x, _ #) -> SCons x SNil
  | otherwise = case innerNode 8 l of (# x, r #) -> SCons x (packNodes (c - 8) r)

------------------------------------------------------------------------
-- Lists

-- | The elements in order.  Lazy: the list is produced as it is consumed.
toList :: Deque a -> [a]
toList = foldrI (:) []

fromList :: [a] -> Deque a
fromList xs = case shortL 0 xs of
  (# n#, pl #) | n <= maxD -> if n == 0 then Nil else Deep (mk n n 0) pl MNil SNil
               | otherwise -> fromListN (L.length xs) xs
    where n = I# n#

-- the length of a list and its items, if there are at most maxD; a larger
-- number otherwise
shortL :: Int -> [a] -> (# Int#, SList a #)
shortL (I# k#) [] = (# k#, SNil #)
shortL k (y : ys)
  | k >= maxD = case k + 1 of I# k# -> (# k#, SNil #)
  | otherwise = case shortL (k + 1) ys of (# n#, l #) -> (# n#, SCons y l #)

-- | 'fromList' for a list whose length is known.  O(n).  Builds the levels
-- directly: each gets a prefix of two to nine items and a suffix of five, and
-- what is between them goes, in nodes of eight, to the level below.
fromListN :: Int -> [a] -> Deque a
fromListN n xs
  | n <= 0 = Nil
  | n <= maxD =
      let !p = (n + 1) `div` 2
      in case takeL p xs of
           (# pl, rest #) -> Deep (mk n p (n - p)) pl MNil (revL rest)
  | otherwise =
      let !k = (n - 7) `div` 8
          !p = n - 5 - 8 * k
      in case takeL p xs of
           (# pl, rest #) -> case leafNodes k rest of
             (# nodes, rest' #) -> Deep (mk n p 5) pl (buildMid 8 k nodes) (revL rest')

-- the first k elements, in order, and the rest (k is small)
takeL :: Int -> [a] -> (# SList a, [a] #)
takeL 0 ys       = (# SNil, ys #)
takeL k (y : ys) = case takeL (k - 1) ys of (# l, r #) -> (# SCons y l, r #)
takeL _ []       = (# SNil, [] #)

revL :: [a] -> SList a
revL = go SNil where go acc []       = acc
                     go acc (y : ys) = go (SCons y acc) ys

-- k nodes of eight elements each from the front of a list, in order
leafNodes :: Int -> [a] -> (# SList (Node a), [a] #)
leafNodes = go SNil
  where
    go acc 0 ys = (# revS acc, ys #)
    go acc k (a : b : c : d : e : f : g : h : ys) = go (SCons (N8 8 a b c d e f g h) acc) (k - 1) ys
    go _ _ _ = error "Deque2.fromList: the list is shorter than its length"

-- c nodes of s leaves each, in order
buildMid :: Int -> Int -> SList (Node a) -> Mid a
buildMid !s !c ns
  | c == 0 = MNil
  | c <= maxD =
      let !p = (c + 1) `div` 2
      in MDeep (mk (c * s) p (c - p)) (p * s) (takeS p ns) MNil (revS (dropS p ns))
  | otherwise =
      let !k = (c - 7) `div` 8
          !p = c - 5 - 8 * k
      in case innerNodes (8 * s) SNil k (dropS p ns) of
           (# nodes, rest #) -> MDeep (mk (c * s) p 5) (p * s) (takeS p ns) (buildMid (8 * s) k nodes) (revS rest)

innerNodes :: Int -> SList (Node (Node a)) -> Int -> SList (Node a) -> (# SList (Node (Node a)), SList (Node a) #)
innerNodes !_ acc 0 ys = (# revS acc, ys #)
innerNodes s acc k (SCons a (SCons b (SCons c (SCons d (SCons e (SCons f (SCons g (SCons h ys))))))))
  = innerNodes s (SCons (N8 s a b c d e f g h) acc) (k - 1) ys
innerNodes _ _ _ _ = error "Deque2.fromList: short list of nodes"

-- | @fromFunction n f@ is @f 0, f 1, ..., f (n - 1)@.
fromFunction :: Int -> (Int -> a) -> Deque a
fromFunction n f
  | n < 0     = error "Deque.fromFunction called with negative len"
  | otherwise = fromListN n (L.map f [0 .. n - 1])

------------------------------------------------------------------------
-- Folds

foldrD :: (a -> r -> r) -> r -> Deque a -> r
foldrD _ z Nil = z
foldrD f z (Deep _ pr m sf) = foldrS f (foldrM (foldrNode f) (foldrRev f z sf) m) pr

-- The same, inlined where it is used, so that a known function is applied
-- directly to the children of the leaf nodes, where nearly all the elements
-- are.  With (:) that builds a leaf's eight cells with no calls in between.
foldrI :: (a -> r -> r) -> r -> Deque a -> r
foldrI _ z Nil = z
foldrI f z (Deep _ pr m sf) = foldrS f (foldrM (\nd acc -> foldrNodeI f nd acc) (foldrRev f z sf) m) pr
{-# INLINE foldrI #-}

foldrNodeI :: (a -> r -> r) -> Node a -> r -> r
foldrNodeI fn (N8 _ a b c d e f g h) z = fn a (fn b (fn c (fn d (fn e (fn f (fn g (fn h z)))))))
foldrNodeI fn (N7 _ a b c d e f g) z = fn a (fn b (fn c (fn d (fn e (fn f (fn g z))))))
foldrNodeI fn (N6 _ a b c d e f) z = fn a (fn b (fn c (fn d (fn e (fn f z)))))
foldrNodeI fn (N5 _ a b c d e) z = fn a (fn b (fn c (fn d (fn e z))))
foldrNodeI fn (N4 _ a b c d) z = fn a (fn b (fn c (fn d z)))
foldrNodeI fn (N3 _ a b c) z = fn a (fn b (fn c z))
foldrNodeI fn (N2 _ a b) z = fn a (fn b z)
{-# INLINE foldrNodeI #-}

foldrM :: (Node a -> r -> r) -> r -> Mid a -> r
foldrM _ z MNil = z
foldrM f z (MDeep _ _ pr m sf) = foldrS f (foldrM (foldrNode f) (foldrRev f z sf) m) pr

foldrS :: (a -> r -> r) -> r -> SList a -> r
foldrS f z = go where go SNil = z
                      go (SCons x r) = f x (go r)

-- a right fold over a list that runs back to front
foldrRev :: (a -> r -> r) -> r -> SList a -> r
foldrRev f = go where go acc SNil = acc
                      go acc (SCons x r) = go (f x acc) r

foldrNode :: (a -> r -> r) -> Node a -> r -> r
foldrNode fn (N8 _ a b c d e f g h) z = fn a (fn b (fn c (fn d (fn e (fn f (fn g (fn h z)))))))
foldrNode fn (N7 _ a b c d e f g) z = fn a (fn b (fn c (fn d (fn e (fn f (fn g z))))))
foldrNode fn (N6 _ a b c d e f) z = fn a (fn b (fn c (fn d (fn e (fn f z)))))
foldrNode fn (N5 _ a b c d e) z = fn a (fn b (fn c (fn d (fn e z))))
foldrNode fn (N4 _ a b c d) z = fn a (fn b (fn c (fn d z)))
foldrNode fn (N3 _ a b c) z = fn a (fn b (fn c z))
foldrNode fn (N2 _ a b) z = fn a (fn b z)

foldlD' :: (r -> a -> r) -> r -> Deque a -> r
foldlD' _ !z Nil = z
foldlD' f z (Deep _ pr m sf) = foldlRev f (foldlM (foldlNode f) (foldlS f z pr) m) sf

foldlM :: (r -> Node a -> r) -> r -> Mid a -> r
foldlM _ !z MNil = z
foldlM f z (MDeep _ _ pr m sf) = foldlRev f (foldlM (foldlNode f) (foldlS f z pr) m) sf

foldlS :: (r -> a -> r) -> r -> SList a -> r
foldlS f = go where go !z SNil = z
                    go !z (SCons x r) = go (f z x) r

-- a strict left fold over a list that runs back to front
foldlRev :: (r -> a -> r) -> r -> SList a -> r
foldlRev f !z = go where go SNil = z
                         go (SCons x r) = let !a = go r in f a x

foldlNode :: (r -> a -> r) -> r -> Node a -> r
foldlNode fn !z (N8 _ a b c d e f g h) = let !z1 = fn z a; !z2 = fn z1 b; !z3 = fn z2 c; !z4 = fn z3 d; !z5 = fn z4 e; !z6 = fn z5 f; !z7 = fn z6 g in fn z7 h
foldlNode fn !z (N7 _ a b c d e f g) = let !z1 = fn z a; !z2 = fn z1 b; !z3 = fn z2 c; !z4 = fn z3 d; !z5 = fn z4 e; !z6 = fn z5 f in fn z6 g
foldlNode fn !z (N6 _ a b c d e f) = let !z1 = fn z a; !z2 = fn z1 b; !z3 = fn z2 c; !z4 = fn z3 d; !z5 = fn z4 e in fn z5 f
foldlNode fn !z (N5 _ a b c d e) = let !z1 = fn z a; !z2 = fn z1 b; !z3 = fn z2 c; !z4 = fn z3 d in fn z4 e
foldlNode fn !z (N4 _ a b c d) = let !z1 = fn z a; !z2 = fn z1 b; !z3 = fn z2 c in fn z3 d
foldlNode fn !z (N3 _ a b c) = let !z1 = fn z a; !z2 = fn z1 b in fn z2 c
foldlNode fn !z (N2 _ a b) = let !z1 = fn z a in fn z1 b

------------------------------------------------------------------------
-- Mapping keeps the shape

mapD :: (a -> b) -> Deque a -> Deque b
mapD _ Nil = Nil
mapD f (Deep t pr m sf) = Deep t (mapS f pr) (mapM' (mapNode f) m) (mapS f sf)

mapM' :: (Node a -> Node b) -> Mid a -> Mid b
mapM' _ MNil = MNil
mapM' f (MDeep t ps pr m sf) = MDeep t ps (mapS f pr) (mapM' (mapNode f) m) (mapS f sf)

mapS :: (a -> b) -> SList a -> SList b
mapS f = go where go SNil = SNil
                  go (SCons x r) = SCons (f x) (go r)
{-# INLINE mapS #-}

mapNode :: (a -> b) -> Node a -> Node b
mapNode fn (N8 n a b c d e f g h) = N8 n (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g) (fn h)
mapNode fn (N7 n a b c d e f g) = N7 n (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g)
mapNode fn (N6 n a b c d e f) = N6 n (fn a) (fn b) (fn c) (fn d) (fn e) (fn f)
mapNode fn (N5 n a b c d e) = N5 n (fn a) (fn b) (fn c) (fn d) (fn e)
mapNode fn (N4 n a b c d) = N4 n (fn a) (fn b) (fn c) (fn d)
mapNode fn (N3 n a b c) = N3 n (fn a) (fn b) (fn c)
mapNode fn (N2 n a b) = N2 n (fn a) (fn b)
{-# INLINE mapNode #-}

------------------------------------------------------------------------
-- Invariant checker, for tests

valid :: Deque a -> Either String ()
valid Nil = Right ()
valid (Deep t pr m sf) = do
  let n = tsize t
  when (n <= 0) $ Left "a Deep with no leaves"
  unless (lenS pr == tpc t) $ Left "top prefix count"
  unless (lenS sf == tsc t) $ Left "top suffix count"
  when (tpc t > maxD || tsc t > maxD) $ Left "top digit too long"
  ms <- validM (1 :: Int) (\_ -> Right 1) m
  unless (n == tpc t + ms + tsc t) $ Left ("top size: " ++ show n ++ " /= " ++ show (tpc t, ms, tsc t))

validM :: Int -> (a -> Either String Int) -> Mid a -> Either String Int
validM _ _ MNil = Right 0
validM lvl chk (MDeep t ps pr m sf) = do
  let n = tsize t
      at s = "level " ++ show lvl ++ ": " ++ s
  when (n <= 0) $ Left (at "an MDeep with no leaves")
  unless (lenS pr == tpc t) $ Left (at "prefix count")
  unless (lenS sf == tsc t) $ Left (at "suffix count")
  when (tpc t > maxD || tsc t > maxD) $ Left (at "digit too long")
  psz <- sum <$> traverse (checkNode chk) (listS pr)
  unless (psz == ps) $ Left (at "prefix size")
  ssz <- sum <$> traverse (checkNode chk) (listS sf)
  ms <- validM (lvl + 1) (checkNode chk) m
  unless (n == psz + ms + ssz) $ Left (at ("size: " ++ show n ++ " /= " ++ show (psz, ms, ssz)))
  pure n

checkNode :: (a -> Either String Int) -> Node a -> Either String Int
checkNode chk nd = do
  s <- sum <$> traverse chk (listS (nodeToFwd nd))
  unless (s == nodeSize nd) $ Left "node size"
  when (s <= 0) $ Left "empty node"
  pure s

listS :: SList a -> [a]
listS SNil = []
listS (SCons x r) = x : listS r

------------------------------------------------------------------------
-- The names Data.Sequence uses, so that this module can stand in for it.

type Seq = Deque

length :: Deque a -> Int
length = size
{-# INLINE length #-}

null :: Deque a -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

infixr 5 <|
infixl 5 |>
infixr 5 ><

(<|) :: a -> Deque a -> Deque a
(<|) = cons
{-# INLINE (<|) #-}

(|>) :: Deque a -> a -> Deque a
(|>) = snoc
{-# INLINE (|>) #-}

(><) :: Deque a -> Deque a -> Deque a
(><) = append
{-# INLINE (><) #-}

infixr 5 :<|
infixl 5 :|>

pattern Empty :: Deque a
pattern Empty <- (null -> True) where
  Empty = Nil

pattern (:<|) :: a -> Deque a -> Deque a
pattern x :<| xs <- (uncons -> Just (x, xs)) where
  x :<| xs = cons x xs

pattern (:|>) :: Deque a -> a -> Deque a
pattern xs :|> x <- (unsnoc -> Just (xs, x)) where
  xs :|> x = snoc xs x

{-# COMPLETE (:<|), Empty #-}
{-# COMPLETE (:|>), Empty #-}

-- | A stable sort.  O(n log n); goes through a list.
sort :: (Ord a) => Deque a -> Deque a
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> Deque a -> Deque a
sortBy cmp d = fromListN (size d) (L.sortBy cmp (toList d))

-- | The same as 'sort' here; Data.Sequence has both.
unstableSort :: (Ord a) => Deque a -> Deque a
unstableSort = sort

unstableSortBy :: (a -> a -> Ordering) -> Deque a -> Deque a
unstableSortBy = sortBy

intersperse :: a -> Deque a -> Deque a
intersperse sep = fromList . L.intersperse sep . toList

reverse :: Deque a -> Deque a
reverse = foldlD' (flip cons) empty

------------------------------------------------------------------------
-- Instances

instance Semigroup (Deque a) where
  (<>) = append

instance Monoid (Deque a) where
  mempty = empty

instance Foldable Deque where
  foldr = foldrD
  foldl' = foldlD'
  toList = toList
  length = size
  null = null

instance Functor Deque where
  fmap = mapD

-- | Rebuilds the sequence.  O(n).
instance Traversable Deque where
  traverse f d = fromListN (size d) <$> traverse f (toList d)

instance (Eq a) => Eq (Deque a) where
  a == b = size a == size b && toList a == toList b

-- | Lexicographic, as for lists.
instance (Ord a) => Ord (Deque a) where
  compare a b = compare (toList a) (toList b)

instance Eq1 Deque where
  liftEq eq a b = size a == size b && liftEq eq (toList a) (toList b)

instance Ord1 Deque where
  liftCompare cmp a b = liftCompare cmp (toList a) (toList b)

instance (Show a) => Show (Deque a) where
  showsPrec p d = showParen (p > 10) $ showString "fromList " . shows (toList d)

instance Show1 Deque where
  liftShowsPrec _ sl p d = showParen (p > 10) $ showString "fromList " . sl (toList d)

-- | The structure is always evaluated; this evaluates the elements fully.
instance (NFData a) => NFData (Deque a) where
  rnf = foldlD' (\() x -> rnf x) ()

instance Exts.IsList (Deque a) where
  type Item (Deque a) = a
  fromList = fromList
  toList = toList
