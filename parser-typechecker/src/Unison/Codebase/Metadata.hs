module Unison.Codebase.Metadata where

import Data.Foldable (toList)
import Data.Map (Map)
import Data.Set (Set)
import Data.List (foldl')
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Unison.Util.Star3 (Star3)
import qualified Data.Map as Map
import qualified Unison.Util.Star3 as Star3

type Type = Referent
type Value = Reference

-- keys can be terms or types
type Metadata = Map Type (Set Value)

inserts :: (Ord a, Ord n) => [(a, Type, Value)] -> Star3 a n Type Value -> Star3 a n Type Value
inserts tups s = foldl' (flip insert) s tups

insertWithMetadata
  :: (Ord a, Ord b)
  => (a, Metadata)
  -> Star3 a b Type Value
  -> Star3 a b Type Value
insertWithMetadata (a, md) =
  inserts [ (a, ty, v) | (ty, vs) <- Map.toList md, v <- toList vs ]

insert :: (Ord a, Ord n) => (a, Type, Value) -> Star3 a n Type Value -> Star3 a n Type Value
insert = Star3.insertD23

delete :: (Ord a, Ord n) => (a, Type, Value) -> Star3 a n Type Value -> Star3 a n Type Value
delete = Star3.deleteD23

-- parallel composition - commutative and associative
merge :: Metadata -> Metadata -> Metadata
merge = Map.unionWith (<>)

-- sequential composition, right-biased
append :: Metadata -> Metadata -> Metadata
append = Map.unionWith (flip const)

empty :: Metadata
empty = mempty
