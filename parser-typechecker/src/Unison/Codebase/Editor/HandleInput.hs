{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Codebase.Editor.HandleInput () where

import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output as Output

import           Control.Applicative
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Control.Monad                  ( when
                                                )
import           Control.Monad.State            ( StateT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Foldable                  ( find
                                                , toList
                                                )
import qualified Data.List                      as List
import           Data.List.Extra                (nubOrd)
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Unison.ABT                    as ABT
import           Unison.Codebase.Branch2        ( Branch
                                                , Branch0
                                                )
import qualified Unison.Codebase.Branch2       as Branch
import qualified Unison.Codebase.BranchUtil    as BranchUtil
import           Unison.Codebase.Editor2        ( Event(..) )
import qualified Unison.Codebase.Editor2        as Editor
import           Unison.Codebase.Path           ( Path
                                                , Path'
                                                , HQSegment
                                                )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import           Unison.HashQualified           ( HashQualified )
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import           Unison.Name                    ( Name )
import           Unison.Names2                  ( Names, Names0, NamesSeg )
import qualified Unison.Names2                  as Names
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Result                  ( pattern Result )
import qualified Unison.ShortHash as SH
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Result                 as Result
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Find              as Find
import           Unison.Util.Free               ( Free )
import qualified Unison.Util.Free              as Free
import           Unison.Util.List               ( uniqueBy )
import qualified Unison.Util.Relation          as R
import           Unison.Var                     ( Var )

type F m i v = Free (Command m i v)
type Type v a = Type.AnnotatedType v a

-- type (Action m i v) a
type Action m i v = MaybeT (StateT (LoopState m v) (F m i v))

liftToAction :: m a -> Action m i v a
liftToAction = lift . lift . Free.eval . Eval

data LoopState m v
  = LoopState
      { _root :: Branch m
      -- the current position in the namespace
      , _currentPath :: Path.Absolute

      -- TBD
      -- , _activeEdits :: Set Branch.EditGuid

      -- The file name last modified, and whether to skip the next file
      -- change event for that path (we skip file changes if the file has
      -- just been modified programmatically)
      , _latestFile :: Maybe (FilePath, SkipNextUpdate)
      , _latestTypecheckedFile :: Maybe (UF.TypecheckedUnisonFile v Ann)

      -- The previous user input. Used to request confirmation of
      -- questionable user commands.
      , _lastInput :: Maybe Input

      -- A 1-indexed list of strings that can be referenced by index at the
      -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
      -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
      , _numberedArgs :: [String]
      }

type SkipNextUpdate = Bool

makeLenses ''LoopState

loopState0 :: Branch m -> Path.Absolute -> LoopState m v
loopState0 b p = LoopState b p Nothing Nothing Nothing []

loop :: forall m v . (Monad m, Var v) => Action m (Either Event Input) v ()
loop = do
  _uf          <- use latestTypecheckedFile
  root'        <- use root
  currentPath' <- use currentPath
  latestFile'  <- use latestFile
  currentBranch' <- getAt currentPath'
  let
      root0 = Branch.head root'
      resolvePath' :: (Path', a) -> (Path, a)
      resolvePath' = Path.fromAbsoluteSplit . Path.toAbsoluteSplit currentPath'
      resolveAsAbsolute = Path.fromAbsoluteSplit . Path.toAbsoluteSplit Path.absoluteEmpty
      -- unsnocPath' :: Path' -> (Absolute, NameSegment)
      -- unsnocPath' = fromJust
      --           . fmap (first Absolute)
      --           . (\(Absolute p) -> Path.unsnoc p)
      --           . Path.toAbsolutePath currentPath'

      -- todo: don't need to use this version, because the NamesSeg and deepReferentes are built into the Branch0 now.
      -- loadHqSrc ::
      --   Path.HQSplit' -> _ (Branch m, NamesSeg, Names0, Absolute, HQSegment)
      -- loadHqSrc hq = do
      --   let (p, seg) = toAbsoluteSplit hq
      --   b <- getAt p
      --   pure ( b
      --        , Branch.toNamesSeg (Branch.head b)
      --        , Branch.toNames0 (Branch.head b)
      --        , p, seg)

  let names' = Branch.toNames (Branch.head currentBranch')
  e           <- eval Input
  let withFile ambient sourceName text k = do
        Result notes r <- eval
             $ Typecheck ambient (error "todo") sourceName text
          -- $ Typecheck ambient (view Branch.history $ get nav') sourceName text
        case r of
          -- Parsing failed
          Nothing -> error "todo"
          -- respond
          --   $ ParseErrors text [ err | Result.Parsing err <- toList notes ]
          Just (names, r) ->
            let h = respond $ TypeErrors
                  text
                  names
                  [ err | Result.TypeError err <- toList notes ]
            in  maybe h (k names) r
  case e of
    Left (IncomingRootBranch _names) -> error "todo: merge multiple heads"
      -- when (Set.member currentBranchName' names)
      --   $ switchBranch currentBranchName'
    Left (UnisonFileChanged sourceName text) ->
      -- We skip this update if it was programmatically generated
      if fromMaybe False . fmap snd $ latestFile'
        then modifying latestFile $ (fmap (const False) <$>)
        else do
          eval (Notify $ FileChangeEvent sourceName text)
          withFile [] sourceName text $ \errorEnv unisonFile -> do
            eval (Notify $ Typechecked sourceName errorEnv unisonFile)
            (bindings, e) <- error "todo"
--               eval . Evaluate (view currentBranch s) $
--                    UF.discardTypes unisonFile
            let e' = Map.map go e
                go (ann, _hash, _uneval, eval, isHit) = (ann, eval, isHit)
            -- todo: this would be a good spot to update the cache
            -- with all the (hash, eval) pairs, even if it's just an
            -- in-memory cache
            eval . Notify $ Evaluated
              text
              (error$"todo: produce Names2 for displaying evaluated Result.\n"
                  ++ "It should include names from the file and the Branch,\n"
                  ++ "and distinguish somehow between existing and new defns\n"
                  ++ "having the same name.")
              -- (errorEnv <> Branch.prettyPrintEnv (Branch.head currentBranch'))
              bindings
              e'
            latestFile .= Just (Text.unpack sourceName, False)
            latestTypecheckedFile .= Just unisonFile
    Right input -> case input of
      -- ForkBranchI (RepoLink src srcPath0) destPath0 ->
      --   maybe srcNotFound srcOk getSrcBranch
      --   where
      --   srcPath = case (src, srcPath0) of
      --     (Local, p) -> resolvePath' p
      --     (Github{}, p) -> resolveAsAbsolute p
      --   srcOk srcBranch = maybe (destOk srcBranch) destExists getDestBranch
      --   destOk srcBranch = setAt destPath srcBranch
      --   srcNotFound = respond $ Output.SourceBranchNotFound input srcPath0
      --   destExists = respond $ Output.DestBranchAlreadyExists input destPath0


        -- let srcPath = case (src, srcPath0) of
        --       (Local, p) -> resolvePath' p
        --       (Github{}, p) -> resolveAsAbsolute p
        --     destPath = resolvePath' destPath0
        -- maybe
        --   (srcNotFound)
        --   (\srcBranch ->
        --     maybe
        --       (\destBranch ->
        --           undefined)
        --       (destAlreadyExists)
        --       (getDest))
        --   (getSrc)
        -- (Branch.head -> destBranch) <- loadBranchAt Local destPath
        -- if not . Branch.isEmpty $ destBranch
        -- then respond $ DestBranchAlreadyExists input destPath0
        -- else do
        --   srcBranch <- loadBranchAt src srcPath
        --   setAt destPath srcBranch
        --   success -- could give rando stats about new defns

      -- MergeBranchI (RepoLink src srcPath0) destPath0 -> do
      --   undefined

      --   let srcPath = case (src, srcPath0) of
      --         (Local, p) -> Path.toAbsolutePath currentPath' p
      --         (Github{}, p) -> Path.toAbsolutePath Path.absoluteEmpty p
      --       destPath = Path.toAbsolutePath currentPath' destPath0
      --   srcBranch <- loadBranchAt src srcPath
      --   updateAtM destPath $ (\b -> eval . Eval $ Branch.merge srcBranch b)
      -- SwitchBranchI path' -> do
      --   path <- use $ currentPath . to (`Path.toAbsolutePath` path')
      --   currentPath .= path
      --   branch' <- loadBranchAt Local path
      --   when (Branch.isEmpty . Branch.head $ branch')
      --     (respond $ CreatedNewBranch path)
      AliasTermI srcHQ dest ->
        zeroOneOrMore getSrcRefs srcNotFound srcOk srcConflicted
        where
        srcOk r = zeroOrMore getDestRefs (destOk r) destExists
        destOk = stepAt . BranchUtil.makeAddTermName (resolvePath' dest)
        getSrcRefs = BranchUtil.getTerm (resolvePath' srcHQ) root0
        getDestRefs = BranchUtil.getTerm (fmap HQ.NameOnly (resolvePath' dest)) root0
        srcNotFound = respond $ SourceTermNotFound input srcHQ
        srcConflicted = respond . Output.SourceTermAmbiguous input srcHQ
        destExists = respond . Output.DestTermAlreadyExists input dest

      AliasTypeI srcHQ dest ->
        zeroOneOrMore getSrcRefs srcNotFound srcOk srcConflicted
        where
        srcOk r = zeroOrMore getDestRefs (destOk r) destExists
        destOk = stepAt . BranchUtil.makeAddTypeName (resolvePath' dest)
        getSrcRefs = BranchUtil.getType (resolvePath' srcHQ) root0
        getDestRefs = BranchUtil.getType (fmap HQ.NameOnly (resolvePath' dest)) root0
        srcNotFound = respond $ SourceTypeNotFound input srcHQ
        srcConflicted = respond . Output.SourceTypeAmbiguous input srcHQ
        destExists = respond . Output.DestTypeAlreadyExists input dest

      MoveTermI srcHQ@(fmap HQ'.toHQ -> srcHQ') dest ->
        zeroOneOrMore getSrcRefs srcNotFound srcOk srcConflicted
        where
        srcOk r = zeroOrMore getDestRefs (destOk r) destExists
        destOk r = stepManyAt
          [ BranchUtil.makeDeleteTermName (resolvePath' . fmap HQ'.toName $ srcHQ) r
          , BranchUtil.makeAddTermName (resolvePath' dest) r ]
        getSrcRefs = BranchUtil.getTerm (resolvePath' srcHQ') root0
        getDestRefs = BranchUtil.getNamedTerm (fmap HQ'.NameOnly (resolvePath' dest)) root0
        srcNotFound = respond $ SourceTermNotFound input srcHQ'
        srcConflicted = respond . Output.SourceTermAmbiguous input srcHQ'
        destExists = respond . Output.DestTermAlreadyExists input dest . Set.map snd

      MoveTypeI srcHQ@(fmap HQ'.toHQ -> srcHQ') dest ->
        zeroOneOrMore getSrcRefs srcNotFound srcOk srcConflicted
        where
        srcOk r = zeroOrMore getDestRefs (destOk r) destExists
        destOk r = stepManyAt
          [ BranchUtil.makeDeleteTypeName (resolvePath' . fmap HQ'.toName $ srcHQ) r
          , BranchUtil.makeAddTypeName (resolvePath' dest) r ]
        getSrcRefs = BranchUtil.getType (resolvePath' srcHQ') root0
        getDestRefs = BranchUtil.getType (fmap HQ.NameOnly (resolvePath' dest)) root0
        srcNotFound = respond $ SourceTypeNotFound input srcHQ'
        srcConflicted = respond . Output.SourceTypeAmbiguous input srcHQ'
        destExists = respond . Output.DestTypeAlreadyExists input dest

      -- MoveBranchI src dest ->
      --   maybe (respond $ Output.SourceBranchNotFound input src)
      --         (\b -> maybe
      --           (undefined)
      --           (respond $ Output.DestBranchAlreadyExists input dest)
      --           (BranchUtil.getBranch (resolvePath' dest) root0)
      --         )
      --         (BranchUtil.getBranch (resolvePath' src) root0)


      -- RenameI targets srcHQ destName -> do
      --   (srcBranch, srcNamesSeg, srcNames0, _srcPath, hq) <- loadHqSrc srcHQ
      --   let (_destPath, _destNameSeg) = unsnocPath' destName
      --       -- doTermConditionally =
      --       --   if Set.notMember Editor.TermName targets then id
      --       --   else ifUniqueTermHQ srcNamesSeg srcNames0 hq renameTerm
      --       -- doTypeConditionally =
      --       --   if Set.notMember Editor.TypeName targets then id
      --       --   else ifUniqueTypeHQ srcNamesSeg srcNames0 hq renameType
      --       -- doBranchConditionally = error "todo"
      --       -- renameTerm r =
      --   error "todo"
      ShowDefinitionI outputLoc (fmap HQ.fromString -> hqs) -> do
        results <- eval . LoadSearchResults $ searchBranchExact currentBranch' hqs
        let termTypes :: Map.Map Reference (Type v Ann)
            termTypes =
              Map.fromList
                [ (r, t) | Output.Tm _ (Just t) (Referent.Ref r) _ <- results ]
            (collatedTerms, collatedTypes) = collateReferences
              (catMaybes . map Output.tmReferent $ results)
              (catMaybes . map Output.tpReference $ results)
        loadedTerms <- for collatedTerms $ \r -> case r of
          Reference.DerivedId i -> do
            tm <- eval (LoadTerm i)
            -- We add a type annotation to the term using if it doesn't
            -- already have one that the user provided
            pure . (r, ) $ case liftA2 (,) tm (Map.lookup r termTypes) of
              Nothing        -> MissingThing i
              Just (tm, typ) -> case tm of
                Term.Ann' _ _ -> RegularThing tm
                _ -> RegularThing (Term.ann (ABT.annotation tm) tm typ)
          Reference.Builtin _ -> pure (r, BuiltinThing)
        loadedTypes <- for collatedTypes $ \r -> case r of
          Reference.DerivedId i ->
            (r, ) . maybe (MissingThing i) RegularThing <$> eval (LoadType i)
          Reference.Builtin _ -> pure (r, BuiltinThing)
        -- makes sure that the user search terms get used as the names
        -- in the pretty-printer
        let
          names :: Names
          names = error $ "todo: come up with a Names here that's sufficient\n"
                      ++  "to pretty-print these loadedTerms, loadedTypes"
          -- ppe = -- now Names:
          --   PPE.fromTermNames [ (r, n) | Editor.Tm n _ r _ <- results ]
          --     <> PPE.fromTypeNames [ (r, n) | Editor.Tp n _ r _ <- results ]
          --     <> Branch.prettyPrintEnv (Branch.head currentBranch')
          loc = case outputLoc of
            ConsoleLocation    -> Nothing
            FileLocation path  -> Just path
            LatestFileLocation -> fmap fst latestFile' <|> Just "scratch.u"
        do
          eval . Notify $ DisplayDefinitions loc names loadedTerms loadedTypes
          -- We set latestFile to be programmatically generated, if we
          -- are viewing these definitions to a file - this will skip the
          -- next update for that file (which will happen immediately)
          latestFile .= ((, True) <$> loc)
      -- ls with no arguments
      SearchByNameI [] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' False
      SearchByNameI ["-l"] -> do
        let results = listBranch $ Branch.head currentBranch'
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' True
      -- ls with arguments
      SearchByNameI ("-l" : (fmap HQ.fromString -> qs)) -> do
        let results = searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' True
      SearchByNameI (map HQ.fromString -> qs) -> do
        let results = searchBranchScored currentBranch' fuzzyNameDistance qs
        numberedArgs .= fmap searchResultToHQString results
        eval (LoadSearchResults results)
          >>= respond
          .   ListOfDefinitions names' False
--      RemoveTermNameI r path ->
--        stepAt $ Branch.deleteTermName r name
--       RemoveTypeNameI r path ->
--        stepAt $ Branch.deleteTypeName r name
      -- ResolveTermNameI r name ->
      --   stepAt
      --     $ Branch.addTermName r name
      --     . Branch.unnameAll Names.TermName name
      -- ResolveTypeName r name ->
      --   stepAt
      --     $ Branch.addTypeName r name
      --     . Branch.unnameAll Names.TypeName name
      -- AliasUnconflictedI targets existingName newName ->
      --   aliasUnconflicted targets existingName newName
      -- RenameUnconflictedI targets oldName newName ->
      --   renameUnconflicted targets oldName newName
      -- UnnameAllI hqs -> do
      --   stepAt $ \b ->
      --     let wrangle b hq = doTerms (doTypes b)
      --          where
      --           doTerms b = foldl' doTerm b (Branch.resolveHQNameTerm b hq)
      --           doTypes b = foldl' doType b (Branch.resolveHQNameType b hq)
      --           doTerm b (n, r) = Branch.deleteTermName r n b
      --           doType b (n, r) = Branch.deleteTypeName r n b
      --     in  foldl' wrangle b hqs
      --   respond $ Success input
      -- SlurpFileI allowUpdates -> case uf of
      --   Nothing  -> respond NoUnisonFile
      --   Just uf' -> do
      --     let collisionHandler = if allowUpdates
      --           then Editor.updateCollisionHandler
      --           else Editor.addCollisionHandler
      --     updateo <- eval $ SlurpFile collisionHandler currentBranch' uf'
      --     let branch' = updatedBranch updateo
      --     -- Don't bother doing anything if the branch is unchanged by the slurping
      --     when (branch' /= currentBranch') $ doMerge currentBranchName' branch'
      --     eval . Notify $ SlurpOutput updateo
      --     currentBranch .= branch'
      -- ListBranchesI ->
      --   eval ListBranches >>= respond . ListOfBranches currentBranchName'
      -- DeleteBranchI branchNames -> withBranches branchNames $ \bnbs -> do
      --   uniqueToDelete <- prettyUniqueDefinitions bnbs
      --   let deleteBranches b =
      --         traverse (eval . DeleteBranch) b >> respond (Success input)
      --   if (currentBranchName' `elem` branchNames)
      --     then respond DeletingCurrentBranch
      --     else if null uniqueToDelete
      --       then deleteBranches branchNames
      --       else ifM (confirmedCommand input)
      --                (deleteBranches branchNames)
      --                (respond . DeleteBranchConfirmation $ uniqueToDelete)
      -- TodoI -> checkTodo
      -- PropagateI -> do
      --   b <- eval . Propagate $ currentBranch'
      --   _ <- eval $ SyncBranch currentBranchName' b
      --   _ <- success
      --   currentBranch .= b
      -- ExecuteI input ->
      --   withFile [Type.ref External $ IOSource.ioReference]
      --            "execute command"
      --            ("main_ = " <> Text.pack input) $
      --              \_ unisonFile ->
      --                 eval . Execute (view currentBranch s) $
      --                   UF.discardTypes unisonFile
      -- UpdateBuiltinsI -> do
      --   stepAt updateBuiltins
      --   checkTodo
      -- ListEditsI -> do
      --   (Branch.head -> b) <- use currentBranch
      --   respond $ ListEdits b
      -- QuitI -> quit
      _ -> error $ "todo: " <> show input
     where
      success       = respond $ Success input
  case e of
    Right input -> lastInput .= Just input
    _ -> pure ()
 where
  {-
  doMerge branchName b = do
    updated <- eval $ SyncBranch branchName b
    -- updated is False if `branchName` doesn't exist.
    -- Not sure why you were updating a nonexistent branch, but under the
    -- assumption that it just got deleted somehow, I guess, we'll write
    -- it to disk now.
    unless updated $ do
      written <- eval $ NewBranch b branchName
      unless written (disappearingBranchBomb branchName)
  disappearingBranchBomb branchName =
    error
      $  "The branch named "
      <> Text.unpack branchName
      <> " disappeared from storage. "
      <> "I tried to put it back, but couldn't. Everybody panic!"
  -- todo: when `branch` becomes purely switchBranch and not newBranch, fix this up.
  switchBranch branchName = do
    branch <- eval $ LoadBranch branchName
    case branch of
      Nothing -> do
        let newBranch = Editor.builtinBranch
        _ <- eval $ NewBranch newBranch branchName
        currentBranch .= newBranch
        currentBranchName .= branchName
        respond $ CreatedBranch $ branchName
      Just branch -> do
        currentBranch .= branch
        currentBranchName .= branchName
        respond $ SwitchedBranch $ branchName
    checkForBuiltinsMismatch
  quit = MaybeT $ pure Nothing
  -}

eval :: Command m i v a -> Action m i v a
eval = lift . lift . Free.eval

-- confirmedCommand :: Input -> Action m i v Bool
-- confirmedCommand i = do
--   i0 <- use lastInput
--   pure $ Just i == i0
--
-- loadBranch :: BranchName -> Action m i v (Maybe Branch)
-- loadBranch = eval . LoadBranch


listBranch :: Branch0 m -> [SearchResult]
listBranch (Branch.toNames0 -> b) =
  List.sortOn (\s -> (SR.name s, s)) (Names.asSearchResults b)

-- | restores the full hash to these search results, for _numberedArgs purposes
searchResultToHQString :: SearchResult -> String
searchResultToHQString = \case
  SR.Tm' n r _ -> HQ.toString $ HQ.requalify n r
  SR.Tp' n r _ -> HQ.toString $ HQ.requalify n (Referent.Ref r)
  _ -> error "unpossible match failure"

-- Return a list of definitions whose names fuzzy match the given queries.
fuzzyNameDistance :: Name -> Name -> Maybe _ -- MatchArray
fuzzyNameDistance (Name.toString -> q) (Name.toString -> n) =
  case Find.fuzzyFindMatchArray q [n] id of
    [] -> Nothing
    (m, _) : _ -> Just m

-- return `name` and `name.<everything>...`
searchBranchPrefix :: Branch m -> Name -> [SearchResult]
searchBranchPrefix b n = case Path.unsnoc (Path.fromName n) of
  Nothing -> []
  Just (init, last) -> case Branch.getAt init b of
    Nothing -> []
    Just b -> Names.asSearchResults . Names.prefix0 n $ names0
      where
      lastName = Path.toName (Path.singleton last)
      subnames = Branch.toNames0 . Branch.head $ (Branch.getAt' (Path.singleton last) b)
      rootnames =
        Names.filter (== lastName) .
        Branch.toNames0 . set Branch.children mempty $ Branch.head b
      names0 = rootnames <> Names.prefix0 lastName subnames

searchBranchScored :: forall m score. (Ord score)
              => Branch m
              -> (Name -> Name -> Maybe score)
              -> [HashQualified]
              -> [SearchResult]
searchBranchScored b score queries =
  nubOrd . fmap snd . toList $ searchTermNamespace <> searchTypeNamespace
  where
  names0 = Branch.toNames0 . Branch.head $ b
  searchTermNamespace = foldMap do1query queries
    where
    do1query :: HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.terms $ names0)
    score1hq :: HashQualified -> (Name, Referent) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` (Referent.toShortHash ref) ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` (Referent.toShortHash ref) ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = Names.termSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty
  searchTypeNamespace = foldMap do1query queries
    where
    do1query :: HashQualified -> Set (Maybe score, SearchResult)
    do1query q = foldMap (score1hq q) (R.toList . Names.types $ names0)
    score1hq :: HashQualified -> (Name, Reference) -> Set (Maybe score, SearchResult)
    score1hq query (name, ref) = case query of
      HQ.NameOnly qn ->
        pair qn
      HQ.HashQualified qn h | h `SH.isPrefixOf` (Reference.toShortHash ref) ->
        pair qn
      HQ.HashOnly h | h `SH.isPrefixOf` (Reference.toShortHash ref) ->
        Set.singleton (Nothing, result)
      _ -> mempty
      where
      result = Names.typeSearchResult names0 name ref
      pair qn = case score qn name of
        Just score -> Set.singleton (Just score, result)
        Nothing -> mempty

-- Separates type references from term references and returns terms and types,
-- respectively. For terms that are constructors, turns them into their data
-- types.
collateReferences
  :: [Referent] -- terms requested, including ctors
  -> [Reference] -- types requested
  -> ([Reference], [Reference])
collateReferences terms types =
  let terms' = [ r | Referent.Ref r <- terms ]
      types' = terms >>= \case
        Referent.Con r _ -> [r]
        _                -> []
  in  (terms', nubOrd $ types' <> types)



-- Foo#123
-- Foo#890
-- bar#567
-- blah#abc
-- cat#abc
-- and search for

-- Foo, want Foo#123 and Foo#890
-- Foo#1, want Foo#123
-- #567, want bar -- what goes in the SR.name?
-- blah, cat, want blah (with comment about cat)?

-- #567 :: Int
-- #567 = +3

searchBranchExact :: Branch m -> [HashQualified] -> [SearchResult]
searchBranchExact b queries = let
  names0 = Branch.toNames0 . Branch.head $ b
  matchesHashPrefix :: (r -> SH.ShortHash) -> (Name, r) -> HashQualified -> Bool
  matchesHashPrefix toShortHash (name, r) = \case
    HQ.NameOnly n -> n == name
    HQ.HashOnly q -> q `SH.isPrefixOf` toShortHash r
    HQ.HashQualified n q ->
      n == name && q `SH.isPrefixOf` toShortHash r
  filteredTypes, filteredTerms, deduped :: [SearchResult]
  filteredTypes =
    [ SR.typeResult query r (Names.hqTypeAliases names0 name r)
    | (name, r) <- R.toList $ Names.types names0
    , Just query <- [find (matchesHashPrefix Reference.toShortHash (name, r)) queries ]
    ]
  filteredTerms =
    [ SR.termResult query r (Names.hqTermAliases names0 name r)
    | (name, r) <- R.toList $ Names.terms names0
    , Just query <- [find (matchesHashPrefix Referent.toShortHash (name, r)) queries ]
    ]
  deduped = uniqueBy SR.toReferent (filteredTypes <> filteredTerms)
  in List.sort deduped

-- withBranch :: BranchName -> (Branch -> Action m i v ()) -> Action m i v ()
-- withBranch b f = loadBranch b >>= maybe (respond $ UnknownBranch b) f
--
-- withBranches :: [BranchName] -> ([(BranchName, Branch)] -> Action m i v ()) -> Action m i v ()
-- withBranches branchNames f = do
--   branches :: [Maybe Branch] <- traverse (eval . LoadBranch) branchNames
--   if any null branches
--   then traverse_ (respond . UnknownBranch)
--           [name | (name, Nothing) <- branchNames `zip` branches]
--   else f (branchNames `zip` fmap fromJust branches)

respond :: Output v -> Action m i v ()
respond output = eval $ Notify output

-- -- Collects the definitions that are not named in any branch outside the inputs.
-- -- It collects all the references within the branches that *aren't* specified,
-- -- and then reports if the branches that *are* specified contain any references
-- -- that don't exist anywhere else.
-- prettyUniqueDefinitions :: forall i v.
--   [(BranchName, Branch)] -> Action m i v [(BranchName, (PPE.PrettyPrintEnv, [Editor.SearchResult' v Ann]))]
-- prettyUniqueDefinitions queryBNBs = do
--     let (branchNames, _) = unzip queryBNBs
--     otherBranchNames <- filter (`notElem` branchNames) <$> eval ListBranches
--     otherKnownReferences :: Set Reference <-
--       mconcat
--         . fmap (Branch.allNamedReferences . Branch.head)
--         . ((Editor.builtinBranch):) -- we dont care about saving these
--         . catMaybes
--         <$> traverse loadBranch otherBranchNames
--     raw <- (traverse . traverse) -- traverse over `[]` and `(BranchName,)`
--       (sequence -- traverse over (PPE,)
--         . go otherKnownReferences
--         . Branch.head)
--       queryBNBs
--     -- remove empty entries like this one: ("test4",(PrettyPrintEnv,[]))
--     pure . filter (not . null . snd . snd) $ raw
--   where
--   go :: Set Reference
--      -> Branch0
--      -> (PPE.PrettyPrintEnv, Action m i v [Editor.SearchResult' v Ann])
--   go known =
--     Branch.prettyPrintEnv &&& eval . LoadSearchResults . pickResults known
--   pickResults :: Set Reference -> Branch0 -> [SearchResult]
--   pickResults known = filter (keep known) . Branch.asSearchResults
--   keep :: Set Reference -> SearchResult -> Bool
--   keep known = \case
--     SR.Tp' _ r@(Reference.DerivedId _) _ -> Set.notMember r known
--     SR.Tm' _ (Referent.Ref r@(Reference.DerivedId _)) _ -> Set.notMember r known
--     _ -> False
--
-- updateBuiltins :: Branch0 -> Branch0
-- updateBuiltins b
--   -- This branch should include:
--   --   names for all refs missing from existing branch
--   --   ~~deprecations for the missing references~~
--   --   no, can't just be deprecations for the missing references,
--   --      they would never be able to come back. :-\
--   --   ok, we'll fix the story for neverending edits later;
--   --   todo: reevaluate this after that.
--   -- todo: remove deprecations for newly added terms?
--             -- what if user intentionally removed
--   = over (Branch.namespaceL . Branch.terms) (Relation.||> oldRefts)
--   . over (Branch.namespaceL . Branch.types) (Relation.||> oldRefs)
--   . over (Branch.namespaceL . Branch.terms) (<> newTerms)
--   . over (Branch.namespaceL . Branch.types) (<> newTypes)
--   . over (Branch.editedTermsL) (<> deprecatedTerms)
--   . over (Branch.editedTypesL) (<> deprecatedTypes)
--   $ b
--   where
--   newTerms =
--     (Branch.termNamespace . Branch.head) Editor.builtinBranch
--       Relation.|> (Set.map Referent.Ref newRefs)
--   newTypes =
--     (Branch.typeNamespace . Branch.head) Editor.builtinBranch
--       Relation.|> newRefs
--   deprecatedTerms =
--     Relation.fromList [ (r, TermEdit.Deprecate) | r <- toList oldRefs ]
--   deprecatedTypes =
--     Relation.fromList [ (r, TypeEdit.Deprecate) | r <- toList oldRefs ]
--
--   -- Builtin references in the "empty" branch, but not in the current branch
--   newRefs = refs Editor.builtinBranch0 `Set.difference` refs b
--   -- Builtin references in the current branch, but not in the empty branch
--   -- Todo: filter away the structural types from this list; they don't need
--   -- to be deleted.  For nominal / unique types, let's think about it?
--   oldRefts = Set.map (Referent.Ref) oldRefs
--   oldRefs = Set.fromList [r | r@(Reference.Builtin _) <- toList $ oldRefs']
--   oldRefs' = refs b `Set.difference` refs Editor.builtinBranch0
--   refs = Branch.allNamedReferences
--
--
-- checkForBuiltinsMismatch :: Action m i v ()
-- checkForBuiltinsMismatch = do
--   b <- use currentBranch
--   when (not $ all null [new b, old b]) $
--     respond $ BustedBuiltins (new b) (old b)
--   where
--   -- Builtin references in the "empty" branch, but not in the current branch
--   new b = refs Editor.builtinBranch `Set.difference` refs b
--   -- Builtin references in the current branch, but not in the empty branch
--   -- Todo: filter away the structural types from this list; they don't need
--   -- to be deleted.  For nominal / unique types, let's think about it.
--   old b = Set.fromList [r | r@(Reference.Builtin _) <- toList $ old' b]
--   old' b = refs b `Set.difference` refs Editor.builtinBranch
--   refs = Branch.allNamedReferences . Branch.head
--
-- checkTodo :: Action m i v ()
-- checkTodo = do
--   b <- use currentBranch
--   eval (Todo b) >>= respond . TodoOutput b
--
--
-- aliasUnconflicted
--   :: forall i v . Set NameTarget -> Name -> Name -> Action m i v ()
-- aliasUnconflicted nameTargets oldName newName =
--   modifyCurrentBranchM $ \branch ->
--     let (branch', result) = foldl' go (branch, mempty) nameTargets
--     in do
--       respond $ AliasOutput oldName newName result
--       pure $ branch'
--   where
--   go (branch, result) nameTarget = (result <>) <$> case nameTarget of
--      Names.TermName ->
--        alias nameTarget Branch.termsNamed Branch.addTermName branch
--      Names.TypeName ->
--        alias nameTarget Branch.typesNamed Branch.addTypeName branch
--                        -- the RenameOutput action and setting the loop state
--   alias
--     :: Foldable f
--     => NameTarget
--     -> (Name -> Branch0 -> f a)
--     -> (a -> Name -> Branch0 -> Branch0)
--     -> Branch
--     -> (Branch, NameChangeResult)
--   alias nameTarget named alias' branch =
--     let oldMatches        = toList . named oldName $ Branch.head branch
--         oldNameMatchCount = length oldMatches
--         newNameExists     = (not . null) (named newName $ Branch.head branch)
--     in  case oldMatches of
--           [oldMatch] | not newNameExists ->
--             ( Branch.modify (alias' oldMatch newName) branch
--             , NameChangeResult mempty mempty (Set.singleton nameTarget)
--             )
--           _ -> (branch, NameChangeResult a b mempty)
--            where
--             a =
--               if oldNameMatchCount > 1 then Set.singleton nameTarget else mempty
--             b = if newNameExists then Set.singleton nameTarget else mempty
--
-- renameUnconflicted
--   :: forall i v . Set NameTarget -> Name -> Name -> Action m i v ()
-- renameUnconflicted nameTargets oldName newName =
--   modifyCurrentBranchM $ \branch ->
--     let (branch', result) = foldl' go (branch, mempty) nameTargets
--     in do
--       respond $ RenameOutput oldName newName result
--       pure branch'
--   where
--   go (branch, result) nameTarget = (result <>) <$> case nameTarget of
--     Names.TermName ->
--       rename nameTarget Branch.termsNamed Branch.renameTerm branch
--     Names.TypeName ->
--       rename nameTarget Branch.typesNamed Branch.renameType branch
--   rename
--     :: Foldable f
--     => NameTarget
--     -> (Name -> Branch0 -> f a)
--     -> (Name -> Name -> Branch0 -> Branch0)
--     -> Branch
--     -> (Branch, NameChangeResult)
--   rename nameTarget named rename' branch =
--     let oldNameMatchCount = length . named oldName $ Branch.head branch
--         newNameExists     = (not . null) (named newName $ Branch.head branch)
--     in  if (oldNameMatchCount == 1 && not newNameExists)
--           then
--             ( Branch.modify (rename' oldName newName) branch
--             , NameChangeResult mempty mempty (Set.singleton nameTarget)
--             )
--           else
--             ( branch
--             , NameChangeResult
--               (if oldNameMatchCount > 1
--                 then Set.singleton nameTarget
--                 else mempty
--               )
--               (if newNameExists then Set.singleton nameTarget else mempty)
--               mempty
--             )
--
-- -- todo: should this go away?
-- merging :: BranchName -> Branch -> Action m i v () -> Action m i v ()
-- merging targetBranchName b success =
--   ifM (eval $ SyncBranch targetBranchName b) success . respond $ UnknownBranch
--     targetBranchName

-- loadBranchAt :: RepoRef -> Path.Absolute -> Action m i v (Branch m)
-- loadBranchAt repo (Path.Absolute p) = do
--   root <- eval $ LoadRootBranch repo
--   let b = Branch.getAt' p root
--   let names0 = Branch.toNames0 $ Branch.head b
--   eval $ RetrieveHashes repo (Names.typeReferences names0)
--                              (Names.termReferences names0)
--   pure b


--getAt :: Functor m => Path -> Action m i v (Branch (Action m i v))
--getAt p = go <$> use root where
--  go root = fromMaybe Branch.empty . Branch.getAt p
--          $ Branch.transform liftToAction root

getAt :: Functor m => Path.Absolute -> Action m i v (Branch m)
getAt (Path.Absolute p) =
  use root <&> fromMaybe Branch.empty . Branch.getAt p

----- These are no good because they presume we're only going to be doing one thing
----- at a time

setAt :: Applicative m => Path.Absolute -> Branch m -> Action m i v ()
setAt p b = updateAtM p (const . pure $ b)

-- stepAt :: Applicative m => Path.Absolute -> (Branch0 m -> Branch0 m) -> Action m i v ()
-- stepAt p f = updateAtM p (pure . Branch.step f)

-- -- stepAtMany :: Applicative m => [(Path.Absolute, Branch0 m -> Branch0 m)] -> Action m i v ()
-- -- stepAtMany steps =

-- stepAtM :: Applicative m => Path.Absolute -> (Branch0 m -> Action m i v (Branch0 m)) -> Action m i v ()
-- stepAtM p f = updateAtM p $ (Branch.stepAtM Path.empty f)

updateAtM :: Applicative m
          => Path.Absolute
          -> (Branch m -> Action m i v (Branch m))
          -> Action m i v ()
updateAtM (Path.Absolute p) f = do
  b <- use root
  b' <- Branch.modifyAtM p f b
  root .= b'
  when (b /= b') $ eval $ SyncLocalRootBranch b'

stepAt :: Applicative m => (Path.Path, Branch0 m -> Branch0 m) -> Action m i v ()
stepAt = stepManyAt . pure

stepManyAt :: Applicative m => [(Path.Path, Branch0 m -> Branch0 m)] -> Action m i v ()
stepManyAt actions = do
    b <- use root
    let b' = Branch.stepManyAt actions b
    root .= b'
    when (b /= b') $ eval $ SyncLocalRootBranch b'

-- data Target = TargetType | TargetTerm | TargetBranch
--   deriving (Eq, Ord, Show)
--
-- getTerm :: Path.HQPathSplit' -> Branch0 m -> Set Referent
-- getTerm = undefined
--
-- getType :: Path.HQPathSplit' -> Branch0 m -> Set Reference
-- getType = undefined
--
-- getTerm' :: Set Target -> Path.HQPathSplit' -> Branch0 m -> Set Referent
-- getTerm' t = if Set.member TargetTerm t then getTerm else \_ _ -> mempty
--
-- getType' :: Set Target -> Path.HQPathSplit' -> Branch0 m -> Set Reference
-- getType' t = if Set.member TargetType t then getType else \_ _ -> mempty

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

matchTermRefs :: NamesSeg -> Names0 -> HQSegment -> [Referent]
matchTermRefs =
  matchRefsImpl Names.termsNamed Names.termReferents Referent.toShortHash

matchTypeRefs :: NamesSeg -> Names0 -> HQSegment -> [Reference]
matchTypeRefs =
  matchRefsImpl Names.typesNamed Names.typeReferences Reference.toShortHash

matchRefsImpl :: (byName -> HQ.HashQualified' n -> Set r)
                  -> (all -> Set r)
                  -> (r -> SH.ShortHash)
                  -> byName
                  -> all
                  -> HQ.HashQualified' n
                  -> [r]
matchRefsImpl byName all toSH ns n0 = \case
  HQ.NameOnly n -> Set.toList $ byName ns (HQ.fromName n)
  HQ.HashQualified n sh ->
    Set.toList
    . Set.filter ((sh `SH.isPrefixOf`) . toSH)
    $ byName ns (HQ.fromName n)
  HQ.HashOnly sh ->
    Set.toList
      . Set.filter ((sh `SH.isPrefixOf`) . toSH)
      $ all n0

-- cata for 0, 1, or more elements of a Foldable
-- tries to match as lazily as possible
zeroOneOrMore :: Foldable f => f a -> b -> (a -> b) -> (f a -> b) -> b
zeroOneOrMore f zero one more = case toList f of
  _ : _ : _ -> more f
  a : _ -> one a
  _ -> zero

zeroOrMore :: Foldable f => f a -> b -> (f a -> b) -> b
zeroOrMore f zero more = case toList f of
  a : _ -> more f
  _ -> zero