{-# LANGUAGE PatternSynonyms   #-}

module Unison.Codebase.NameSegment where

import           Data.Text                      ( Text )
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)

toName :: NameSegment -> Name.Name
toName = Name.Name . toText

type HQSegment = HQ.HashQualified' NameSegment
type HQ'Segment = HQ'.HashQualified' NameSegment

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]

isEmpty :: NameSegment -> Bool
isEmpty ns = toText ns == mempty
