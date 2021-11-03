module Unison.Util.Sequence where

import qualified Data.Vector as V
import qualified Unison.Util.Rope as R

-- the Int is the size of l and mid combined
data Chunk a = Chunk !(V.Vector a) (Chunk (V.Vector a)) !Int !(V.Vector a)

arity :: Int
arity = 32

chunkCons :: a -> Chunk a -> Chunk a
chunkCons a (Chunk l mid n r) 
  | V.length l == arity = Chunk (V.singleton a) (chunkCons l mid) (n+1) r 
  | otherwise           = Chunk (V.snoc l a) mid (n+1) r

chunkSnoc :: Chunk a -> a -> Chunk a
chunkSnoc (Chunk l mid n r) a
  | V.length r == arity = Chunk l (chunkSnoc mid r) (n+1) (V.singleton a)
  | otherwise           = Chunk l mid n (V.snoc r a)

unsafeIndex :: Int -> Chunk a -> a
unsafeIndex i (Chunk l mid n r)
  | i < V.length l = l `V.unsafeIndex` i
  | i < n          = let i' = i - n 
                     in unsafeIndex (i' `div` arity) mid `V.unsafeIndex` (i' `rem` arity) 
  | otherwise      = r V.! (i - n) 

foldl :: (b -> a -> b) -> b -> Chunk a -> b
foldl f z0 (Chunk l mid _ r) = 
  let
    z1 = V.foldl' f z0 l 
    z2 = Unison.Util.Sequence.foldl (V.foldl' f) z1 mid
    z3 = V.foldl' f z2 r
  in
    z3

instance Semigroup (Chunk a) where (<>) = mappend
instance Monoid (Chunk a) where 
  mempty = Chunk mempty mempty 0 mempty
  mappend l r = Unison.Util.Sequence.foldl chunkSnoc l r  

instance R.Take (Chunk a) where
  take k (Chunk l mid n r) 
    | k < V.length l = Chunk (V.take k l) mempty (min k (V.length l)) mempty
    | k >= n         = Chunk l mid n (V.take (k-n) r)
    | otherwise      = Chunk l mid' k V.empty 
      where
        k' = k - V.length l
        mid' = mapLast (V.take (k' `rem` arity)) (R.take ((k' `div` arity) + 1) mid)

mapLast :: (V.Vector a -> V.Vector a) -> Chunk a -> Chunk a
mapLast f (Chunk l mid n r)
  | not (V.null r)  = Chunk l mid n (f r)
  | n == V.length l = let fl = f l in Chunk fl mid (V.length fl) r
  | otherwise       = undefined -- Chunk l (mapLast (mapLast f) mid) n r

instance R.Sized (Chunk a) where
  size (Chunk _ _ n r) = n + V.length r

instance R.Index (Chunk a) a where
  index i c | i >= 0 && i < R.size c = Just (unsafeIndex i c)
            | otherwise              = Nothing