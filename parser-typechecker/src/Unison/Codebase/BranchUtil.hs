module Unison.Codebase.BranchUtil where
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Branch2 as Branch
import qualified Unison.Codebase.NameSegment as NameSegment
import Unison.Codebase.Branch2 (Branch, Branch0)
import qualified Unison.Names2 as Names
import Unison.Names2 (Names0)
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Unison.HashQualified (HashQualified'(NameOnly, HashOnly, HashQualified))
-- import qualified Unison.HashQualified' as HQ'
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Star3 as Star3
import Unison.Codebase.Metadata (Metadata)
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Util.List as List

addFromNames0 :: Applicative m => Names0 -> Branch0 m -> Branch0 m
addFromNames0 names0 = Branch.stepManyAt0 (typeActions <> termActions)
  where
  typeActions = map doType . R.toList $ Names.types names0
  termActions = map doTerm . R.toList $ Names.terms names0
--  doTerm :: (Name, Referent) -> (Path, Branch0 m -> Branch0 m)
  doTerm (n, r) = case Path.splitFromName n of
    Nothing -> errorEmptyName
    Just split -> makeAddTermName split r mempty -- no metadata
--  doType :: (Name, Reference) -> (Path, Branch0 m -> Branch0 m)
  doType (n, r) = case Path.splitFromName n of
             Nothing -> errorEmptyName
             Just split -> makeAddTypeName split r mempty -- no metadata
  errorEmptyName = error "encountered an empty name"

-- getNamedTerm :: Path.HQ'Split -> Branch0 m -> Set (Path.NameSegment, Referent)
-- getNamedTerm (p, hq') b = case hq' of
--   HQ'.NameOnly n -> Set.map (n,) (R.lookupDom n terms)
--   HQ'.HashQualified n sh -> Set.map (n,) . filter sh $ R.lookupDom n terms
--   where
--   filter sh = Set.filter (\r -> sh `SH.isPrefixOf` Referent.toShortHash r)
--   terms = Branch._terms (Branch.getAt0 p b)
--
-- getNamedType :: Path.HQ'Split -> Branch0 m -> Set (Path.NameSegment, Reference)
-- getNamedType (p, hq') b = case hq' of
--   HQ'.NameOnly n -> Set.map (n,) (R.lookupDom n types)
--   HQ'.HashQualified n sh -> Set.map (n,) . filter sh $ R.lookupDom n types
--   where
--   filter sh = Set.filter (\r -> sh `SH.isPrefixOf` Reference.toShortHash r)
--   types = Branch._types (Branch.getAt0 p b)

getTerm :: Path.HQSplit -> Branch0 m -> Set Referent
getTerm (p, hq) =
  Set.fromList . Star3.d1s . selectTermHQSplit hq . Branch.deepTerms . Branch.getAt0 p

getType :: Path.HQSplit -> Branch0 m -> Set Reference
getType (p, hq) =
  Set.fromList . Star3.d1s . selectTypeHQSplit hq . Branch.deepTypes . Branch.getAt0 p

getTypeMetadataAt :: Path.HQSplit -> Reference -> Branch0 m -> Metadata
getTypeMetadataAt (path,hq) r b = Set.fromList <$> List.multimap mdList
  where
  mdList :: [(Metadata.Type, Metadata.Value)]
  mdList = Star3.d23s' . selectTypeHQSplit hq . Star3.selectD1 (Set.singleton r)
         . Branch.deepTypes $ Branch.getAt0 path b

getTermMetadataAt :: Path.HQSplit -> Referent -> Branch0 m -> Metadata
getTermMetadataAt (path,hq) r b = Set.fromList <$> List.multimap mdList
  where
  mdList :: [(Metadata.Type, Metadata.Value)]
  mdList = Star3.d23s' . selectTermHQSplit hq . Star3.selectD1 (Set.singleton r)
         . Branch.deepTerms $ Branch.getAt0 path b

selectTermHQSplit :: NameSegment.HQSegment -> Branch.DeepTerms -> Branch.DeepTerms
selectTermHQSplit hq terms = case hq of
  NameOnly n -> Star3.selectFact (Set.singleton $ NameSegment.toName n) terms
  HashOnly sh -> filter sh terms
  HashQualified n sh ->
    filter sh $ Star3.selectFact (Set.singleton $ NameSegment.toName n) terms
  where
    filter sh = Star3.filterD1 (\r -> sh `SH.isPrefixOf` Referent.toShortHash r)

selectTypeHQSplit :: NameSegment.HQSegment -> Branch.DeepTypes -> Branch.DeepTypes
selectTypeHQSplit hq types = case hq of
  NameOnly n -> Star3.selectFact (Set.singleton $ NameSegment.toName n) types
  HashOnly sh -> filter sh types
  HashQualified n sh ->
    filter sh $ Star3.selectFact (Set.singleton $ NameSegment.toName n) types
  where
    filter sh = Star3.filterD1 (\r -> sh `SH.isPrefixOf` Reference.toShortHash r)

getBranch :: Path.Split -> Branch0 m -> Maybe (Branch m)
getBranch (p, seg) b = case Path.toList p of
  [] -> snd <$> Map.lookup seg (Branch._children b)
  h : p ->
    (Branch.head . snd <$> Map.lookup h (Branch._children b)) >>=
      getBranch (Path.fromList p, seg)


makeAddTermName :: Path.Split -> Referent -> Metadata -> (Path, Branch0 m -> Branch0 m)
makeAddTermName (p, name) r md = (p, Branch.addTermName r name md)

makeDeleteTermName :: Path.Split -> Referent -> (Path, Branch0 m -> Branch0 m)
makeDeleteTermName (p, name) r = (p, Branch.deleteTermName r name)

makeAddTypeName :: Path.Split -> Reference -> Metadata -> (Path, Branch0 m -> Branch0 m)
makeAddTypeName (p, name) r md = (p, Branch.addTypeName r name md)

makeDeleteTypeName :: Path.Split -> Reference -> (Path, Branch0 m -> Branch0 m)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

-- to delete, just set with Branch.empty
makeSetBranch ::
  Path.Split -> Branch m -> (Path, Branch0 m -> Branch0 m)
makeSetBranch (p, name) b = (p, Branch.setChildBranch name b)


-- setTerm :: Path.PathSplit' -> Set Referent -> Branch0 m -> Branch0 m
-- setType :: Path.PathSplit' -> Set Reference -> Branch0 m -> Branch0 m
-- setBranch :: Path.PathSplit' -> Branch0 m -> Branch0 m -> Branch0 m
-- setBranch somewhere new oldroot === newroot
-- deleteAt :: Target -> Branch0 m -> Branch0 m
-- move :: Path.HQPathSplit Path.Absolute
--      -> Path.PathSplit Path.Absolute
--      -> Branch0 m
--      -> Branch0 m
-- move src dest b = foldl' step b (toList (targetTypes src)) where
--   step b TargetType = set
