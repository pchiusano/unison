{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | The Unison language typechecker, based on:
-- "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism",
-- by Dunfield and Krishnaswami
--
-- PDF at: https://www.mpi-sws.org/~neelk/bidir.pdf
module Unison.Typechecker.Context where


-- import           Unison.Term (Term)
--  trace (msg ++ ":\n" ++ show (Var.shortName a, Var.shortName b, Var.shortName c)) t
--watchVar msg a = trace (msg ++ ": " ++ Text.unpack (Var.shortName a)) a
--watchVars msg t@(a,b,c) =
import           Control.Monad
import           Control.Monad.Loops (anyM, allM)
import           Control.Monad.State
import qualified Data.Foldable as Foldable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Debug.Trace
import qualified Unison.ABT as ABT
import           Unison.DataDeclaration (DataDeclaration)
import qualified Unison.DataDeclaration as DataDeclaration
import           Unison.Note (Note,Noted(..))
import qualified Unison.Note as Note
import           Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import           Unison.Reference (Reference)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import           Unison.TypeVar (TypeVar)
import qualified Unison.TypeVar as TypeVar
import           Unison.Var (Var)
import qualified Unison.Var as Var
-- uncomment for debugging
--watch msg a = trace (msg ++ ":\n" ++ show a) a

-- | We deal with type variables annotated with whether they are universal or existential
type Type v = Type.Type (TypeVar v)
type Term v = Term.Term' (TypeVar v) v
type Monotype v = Type.Monotype (TypeVar v)

pattern Universal v <- Var (TypeVar.Universal v) where
  Universal v = Var (TypeVar.Universal v)

pattern Existential v <- Var (TypeVar.Existential v) where
  Existential v = Var (TypeVar.Existential v)

-- | Elements of an ordered algorithmic context
data Element v
  = Var (TypeVar v)        -- A variable declaration
  | Solved v (Monotype v)  -- `v` is solved to some monotype
  | Ann v (Type v)         -- `v` has type `a`, which may be quantified
  | Marker v deriving (Eq) -- used for scoping

instance Var v => Show (Element v) where
  show (Var v) = case v of
    TypeVar.Universal v -> Text.unpack (Var.shortName v)
    TypeVar.Existential v -> "'"++Text.unpack (Var.shortName v)
  show (Solved v t) = "'"++Text.unpack (Var.shortName v)++" = "++show t
  show (Ann v t) = Text.unpack (Var.shortName v)++" : "++show t
  show (Marker v) = "|"++Text.unpack (Var.shortName v)++"|"

(===) :: Eq v => Element v -> Element v -> Bool
Existential v === Existential v2 | v == v2 = True
Universal v   === Universal v2 | v == v2 = True
Marker v      === Marker v2 | v == v2 = True
_ === _ = False

{- An ordered algorithmic context, stored as a snoc list of
   elements (the first element of the list represents the last element
   of the context).

   The `Info` value stored along with each element is a summary of
   all values up to and including that element of the context. With
   this representation, any suffix of the `Context` element list
   is also a valid context, and a fresh name can be obtained just
   by inspecting the first `Info` in the list.
-}
newtype Context v = Context [(Element v, Info v)]

data Info v =
  Info { existentialVars :: Set v -- set of existentials seen so far
       , universalVars :: Set v -- set of universals seen so far
       , allVars :: Set v -- all variables seen so far
       , isWellformed :: Bool -- whether the context so far is well-formed
       }

-- | The empty context
context0 :: Context v
context0 = Context []

env0 :: Env v
env0 = Env 0 context0

instance Var v => Show (Context v) where
  show (Context es) = "Γ\n  " ++ (intercalate "\n  " . map (show . fst)) (reverse es)

debugEnabled :: Bool
debugEnabled = True

logContext :: Var v => String -> M v ()
logContext msg = when debugEnabled $ do
  ctx <- getContext
  let !_ = trace ("\n"++msg ++ ": " ++ show ctx) ()
  setContext ctx

-- ctxOK :: Context -> Context
-- ctxOK ctx = if wellformed ctx then ctx else error $ "not ok: " ++ show ctx

usedVars :: Context v -> Set v
usedVars = allVars . info

-- | Return the `Info` associated with the last element of the context, or the zero `Info`.
info :: Context v -> Info v
info (Context []) = Info Set.empty Set.empty Set.empty True
info (Context ((_,i):_)) = i

-- | Add an element onto the end of this `Context`. Takes `O(log N)` time,
-- including updates to the accumulated `Info` value.
extend :: Var v => Element v -> Context v -> Context v
extend e c@(Context ctx) = Context ((e,i'):ctx) where
  i' = addInfo e (info c)
  -- see figure 7
  addInfo e (Info es us vs ok) = case e of
    Var v -> case v of
      -- UvarCtx - ensure no duplicates
      TypeVar.Universal v -> Info es (Set.insert v us) (Set.insert v vs) (ok && Set.notMember v us)
      -- EvarCtx - ensure no duplicates, and that this existential is not solved earlier in context
      TypeVar.Existential v -> Info (Set.insert v es) us (Set.insert v vs) (ok && Set.notMember v es)
    -- SolvedEvarCtx - ensure `v` is fresh, and the solution is well-formed wrt the context
    Solved v sa -> Info (Set.insert v es) us (Set.insert v vs) (ok && Set.notMember v es
                                                                      && wellformedType c (Type.getPolytype sa))
    -- VarCtx - ensure `v` is fresh, and annotation is well-formed wrt the context
    Ann v t -> Info es us (Set.insert v vs) (ok && Set.notMember v vs && wellformedType c t)
    -- MarkerCtx - note that since a Marker is always the first mention of a variable, suffices to
    -- just check that `v` is not previously mentioned
    Marker v -> Info es us (Set.insert v vs) (ok && Set.notMember v vs)

data Env v = Env { freshId :: Word, ctx :: Context v }

type DataDeclarations v = Map Reference (DataDeclaration v)

-- | Typechecking monad
newtype M v a = M {
  runM :: MEnv v -> Either Note (a, Env v)
}

-- | The typechecking environment
data MEnv v = MEnv {
  env :: Env v,                    -- The typechecking state
  abilities :: [Type v],           -- Allowed ambient abilities
  dataDecls :: DataDeclarations v, -- Data declarations in scope
  abilityChecks :: Bool            -- Whether to perform ability checks.
                                   --   It's here so we can disable it during
                                   --   effect inference.
}

orElse :: M v a -> M v a -> M v a
orElse m1 m2 =
  M (\menv -> either (const $ runM m2 menv) Right $ runM m1 menv)

fromMEnv :: (MEnv v -> a) -> MEnv v -> Either Note (a, Env v)
fromMEnv f m = Right (f m, env m)

getContext :: M v (Context v)
getContext = M . fromMEnv $ ctx . env

getDataDeclarations :: M v (DataDeclarations v)
getDataDeclarations = M $ fromMEnv dataDecls

getAbilities :: M v [Type v]
getAbilities = M $ fromMEnv abilities

abilityCheckEnabled :: M v Bool
abilityCheckEnabled = M $ fromMEnv abilityChecks

withoutAbilityCheck :: M v a -> M v a
withoutAbilityCheck m = M (\menv -> runM m $ menv { abilityChecks = False })

abilityCheck' :: Var v => [Type v] -> [Type v] -> M v ()
abilityCheck' ambient requested = do
  success <- flip allM requested $ \req ->
    flip anyM ambient $ \amb -> (True <$ subtype amb req) `orElse` pure False
  when (not success) $
    fail $ "Ability check failed. Requested abilities " <> show requested <>
           " but ambient abilities only included " <> show ambient <> "."

abilityCheck :: Var v => [Type v] -> M v ()
abilityCheck requested = do
  enabled <- abilityCheckEnabled
  when enabled $ do
    ambient <- getAbilities
    abilityCheck' ambient requested

getFromTypeEnv :: (Ord r, Show r)
               => String -> M v (Map r (f v)) -> r ->  M v (f v)
getFromTypeEnv what get r = get >>= \decls ->
  case Map.lookup r decls of
    Nothing -> fail $ "unknown " ++ what ++ " reference: " ++ show r ++ " " ++
                      show (Map.keys decls)
    Just decl -> pure decl

getDataDeclaration :: Reference -> M v (DataDeclaration v)
getDataDeclaration = getFromTypeEnv "data type" getDataDeclarations

getConstructorType :: Var v => Reference -> Int -> M v (Type v)
getConstructorType = getConstructorType' getDataDeclaration

getConstructorType' :: (Var v, Show r)
                    => (r -> M v (DataDeclaration v))
                    -> r
                    -> Int
                    -> M v (Type v)
getConstructorType' get r cid = do
  decl <- get r
  case drop cid (DataDeclaration.constructors decl) of
    [] -> fail $ "invalid constructor id: " ++ show cid ++ " in " ++ show r
    (_v, typ) : _ -> pure $ ABT.vmap TypeVar.Universal typ

setContext :: Context v -> M v ()
setContext ctx = M (\menv -> let e = env menv in Right ((), e {ctx = ctx}))

modifyContext :: (Context v -> M v (Context v)) -> M v ()
modifyContext f = do c <- getContext; c <- f c; setContext c

modifyContext' :: (Context v -> Context v) -> M v ()
modifyContext' f = modifyContext (pure . f)

appendContext :: Var v => Context v -> M v ()
appendContext tl = modifyContext' (\ctx -> ctx `append` tl)

scope :: String -> M v a -> M v a
scope msg (M m) = M (\menv -> Note.scope msg $ m menv)

freshenVar :: Var v => v -> M v v
freshenVar v =
  M (\menv ->
       let e = env menv
           id = freshId e
       in Right (Var.freshenId id v, e {freshId = id+1}))

freshenTypeVar :: Var v => TypeVar v -> M v v
freshenTypeVar v =
  M (\menv ->
       let e = env menv
           id = freshId e
       in Right (Var.freshenId id (TypeVar.underlying v), e {freshId = id+1}))

freshNamed :: Var v => Text -> M v v
freshNamed = freshenVar . Var.named

freshVar :: Var v => M v v
freshVar = freshNamed "v"


-- then have check, subtype, etc, take a Fresh (Term v), Fresh (Type v)

-- | Build a context from a list of elements.
context :: Var v => [Element v] -> Context v
context xs = foldl' (flip extend) context0 xs

-- | `append c1 c2` adds the elements of `c2` onto the end of `c1`.
append :: Var v => Context v -> Context v -> Context v
append ctxL (Context es) =
  -- since `es` is a snoc list, we add it to `ctxL` in reverse order
  foldl' f ctxL (reverse es) where
    f ctx (e,_) = extend e ctx

-- | Delete from the end of this context up to and including
-- the given `Element`. Returns `Left` if the element is not found.
retract :: (Monad m, Var v) => Element v -> Context v -> m (Context v)
retract m (Context ctx) =
  let maybeTail [] = fail ("unable to retract: " ++ show m)
      maybeTail (_:t) = pure t
  -- note: no need to recompute used variables; any suffix of the
  -- context snoc list is also a valid context
  in Context <$> maybeTail (dropWhile (\(e,_) -> e /= m) ctx)

-- | Like `retract`, but returns the empty context if retracting would remove all elements.
retract' :: Var v => Element v -> Context v -> Context v
retract' e ctx = case retract e ctx of
  Left _ -> context []
  Right ctx -> ctx

universals :: Context v -> Set v
universals = universalVars . info

existentials :: Context v -> Set v
existentials = existentialVars . info

solved :: Context v -> [(v, Monotype v)]
solved (Context ctx) = [(v, sa) | (Solved v sa,_) <- ctx]

unsolved :: Context v -> [v]
unsolved (Context ctx) = [v | (Existential v,_) <- ctx]

-- | Apply the context to the input type, then convert any unsolved existentials
-- to universals.
generalizeExistentials :: Var v => Context v -> Type v -> Type v
generalizeExistentials ctx t = foldr gen (apply ctx t) (unsolved ctx)
  where
    gen e t =
      if TypeVar.Existential e `ABT.isFreeIn` t
      then Type.forall (TypeVar.Universal e) (ABT.subst (TypeVar.Existential e) (Type.universal e) t)
      else t -- don't bother introducing a forall if type variable is unused

replace :: Var v => Element v -> Context v -> Context v -> Context v
replace e focus ctx =
  let (l,r) = breakAt e ctx
  in l `append` focus `append` r

breakAt :: Var v => Element v -> Context v -> (Context v, Context v)
breakAt m (Context xs) =
  let
    (r, l) = break (\(e,_) -> e === m) xs
  -- l is a suffix of xs and is already a valid context;
  -- r needs to be rebuilt
  in (Context (drop 1 l), context . map fst $ reverse r)

-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: Var v => Context v -> v -> v -> Bool
ordered ctx v v2 = Set.member v (existentials (retract' (Existential v2) ctx))

-- | Check that the context is well formed, see Figure 7 of paper
-- Since contexts are 'monotonic', we can compute an cache this efficiently
-- as the context is built up, see implementation of `extend`.
wellformed :: Context v -> Bool
wellformed ctx = isWellformed (info ctx)

-- | Check that the type is well formed wrt the given `Context`, see Figure 7 of paper
wellformedType :: Var v => Context v -> Type v -> Bool
wellformedType c t = wellformed c && case t of
  Type.Existential' v -> Set.member v (existentials c)
  Type.Universal' v -> Set.member v (universals c)
  Type.Ref' _ -> True
  Type.Arrow' i o -> wellformedType c i && wellformedType c o
  Type.Ann' t' _ -> wellformedType c t'
  Type.App' x y -> wellformedType c x && wellformedType c y
  Type.Effect' es a -> all (wellformedType c) es && wellformedType c a
  Type.Forall' t ->
    let (v,ctx2) = extendUniversal c
    in wellformedType ctx2 (ABT.bind t (Type.universal v))
  _ -> error $ "Context.wellformedType - ill formed type - " ++ show t
  where
  -- | Extend this `Context` with a single variable, guaranteed fresh
  extendUniversal ctx = case Var.freshIn (usedVars ctx) (Var.named "var") of
    v -> (v, extend (Universal v) ctx)

bindings :: Context v -> [(v, Type v)]
bindings (Context ctx) = [(v,a) | (Ann v a,_) <- ctx]

lookupType :: Eq v => Context v -> v -> Maybe (Type v)
lookupType ctx v = lookup v (bindings ctx)

-- | Replace any existentials with their solution in the context
apply :: Var v => Context v -> Type v -> Type v
apply ctx t = case t of
  Type.Universal' _ -> t
  Type.Ref' _ -> t
  Type.Existential' v ->
    maybe t (\(Type.Monotype t') -> apply ctx t') (lookup v (solved ctx))
  Type.Arrow' i o -> Type.arrow (apply ctx i) (apply ctx o)
  Type.App' x y -> Type.app (apply ctx x) (apply ctx y)
  Type.Ann' v k -> Type.ann (apply ctx v) k
  Type.Effect' es t -> Type.effect (map (apply ctx) es) (apply ctx t)
  Type.ForallNamed' v t' -> Type.forall v (apply ctx t')
  _ -> error $ "Context.apply ill formed type - " ++ show t

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α^ = τ,ΓR)
-- If the given existential variable exists in the context,
-- we solve it to the given monotype, otherwise return `Nothing`
solve :: Var v => Context v -> v -> Monotype v -> Maybe (Context v)
solve ctx v t
  -- okay to solve something again if it's to an identical type
  | v `elem` (map fst (solved ctx)) = same =<< lookup v (solved ctx)
  where same t2 | apply ctx (Type.getPolytype t) == apply ctx (Type.getPolytype t2) = Just ctx
                | otherwise = Nothing
solve ctx v t
  | wellformedType ctxL (Type.getPolytype t) = Just ctx'
  | otherwise                                = Nothing
  where (ctxL,ctxR) = breakAt (Existential v) ctx
        ctx' = ctxL `append` context [Solved v t] `append` ctxR

extendUniversal :: Var v => v -> M v v
extendUniversal v = do
  v' <- freshenVar v
  modifyContext (pure . extend (Universal v'))
  pure v'

extendMarker :: Var v => v -> M v v
extendMarker v = do
  v' <- freshenVar v
  modifyContext (\ctx -> pure $ ctx `append` (context [Marker v', Existential v']))
  pure v'

notMember :: Var v => v -> Set (TypeVar v) -> Bool
notMember v s = Set.notMember (TypeVar.Universal v) s && Set.notMember (TypeVar.Existential v) s

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: Var v => Type v -> Type v -> M v ()
subtype tx ty = scope (show tx++" <: "++show ty) $
  do ctx <- getContext; go ctx tx ty
  where -- Rules from figure 9
  go _ (Type.Ref' r) (Type.Ref' r2) | r == r2 = pure () -- `Unit`
  go ctx t1@(Type.Universal' v1) t2@(Type.Universal' v2) -- `Var`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go ctx t1@(Type.Existential' v1) t2@(Type.Existential' v2) -- `Exvar`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go _ (Type.Arrow' i1 o1) (Type.Arrow' i2 o2) = do -- `-->`
    subtype i1 i2; ctx' <- getContext
    subtype (apply ctx' o1) (apply ctx' o2)
  go _ (Type.App' x1 y1) (Type.App' x2 y2) = do -- analogue of `-->`
    subtype x1 x2; ctx' <- getContext
    subtype (apply ctx' y1) (apply ctx' y2)
  go _ t (Type.Forall' t2) = scope "forall (R)" $ do
    v' <- extendUniversal =<< ABT.freshen t2 freshenTypeVar
    t2 <- pure $ ABT.bind t2 (Type.universal v')
    subtype t t2
    modifyContext (retract (Universal v'))
  go _ (Type.Forall' t) t2 = scope "forall (L)" $ do
    v <- extendMarker =<< ABT.freshen t freshenTypeVar
    t <- pure $ ABT.bind t (Type.existential v)
    ctx' <- getContext
    subtype (apply ctx' t) t2
    modifyContext (retract (Marker v))
  go ctx (Type.Existential' v) t -- `InstantiateL`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateL v t
  go ctx t (Type.Existential' v) -- `InstantiateR`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateR t v
  go _ (Type.Effect'' es1 a1) (Type.Effect'' es2 a2)
     | not (null es1) || not (null es2) = do
       subtype a1 a2
       ctx <- getContext
       let es1' = map (apply ctx) es1
           es2' = map (apply ctx) es2
       abilityCheck' es2' es1'
  go _ (Type.Effect' [] a1) (Type.Effect' [] a2) = subtype a1 a2
  go _ a1 (Type.Effect' [] a2) = subtype a1 a2

  -- for all e in es1', must exist e2 in es2' s.t. e <: e2
  --{Remote} Int <: {Abort, Remote} Int  ?
  --{} Int <: {es} Int


--{e} a <: {e'} b?
  --Int <: forall e . {e}Int
  --{e}Int <: {e2}Int if e <: e2
  --{e}Int <: {es}Int if exists e' in es | e <: e'
  --go ctx (Type.Effect' es a) t = ...
  --go ctx t (Type.Effect' es a) = ...
  --
  go _ _ _ = fail "not a subtype"

-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: Var v => v -> Type v -> M v ()
instantiateL v t = getContext >>= \ctx -> case Type.monotype t >>= solve ctx v of
  Just ctx -> setContext ctx -- InstLSolve
  Nothing -> case t of
    Type.Existential' v2 | ordered ctx v v2 -> -- InstLReach (both are existential, set v2 = v)
      maybe (fail "InstLReach failed") setContext $
        solve ctx v2 (Type.Monotype (Type.existential v))
    Type.Arrow' i o -> do -- InstLArr
      [i',o'] <- traverse freshenVar [ABT.v' "i", ABT.v' "o"]
      let s = Solved v (Type.Monotype (Type.arrow (Type.existential i') (Type.existential o')))
      modifyContext' $ replace (Existential v) (context [Existential o', Existential i', s])
      instantiateR i i'
      ctx <- getContext
      instantiateL o' (apply ctx o)
    Type.App' x y -> do -- analogue of InstLArr
      [x', y'] <- traverse freshenVar [ABT.v' "x", ABT.v' "y"]
      let s = Solved v (Type.Monotype (Type.app (Type.existential x') (Type.existential y')))
      modifyContext' $ replace (Existential v) (context [Existential y', Existential x', s])
      ctx0 <- getContext
      ctx' <- instantiateL x' (apply ctx0 x) >> getContext
      instantiateL y' (apply ctx' y)
    Type.Forall' body -> do -- InstLIIL
      v <- extendUniversal =<< ABT.freshen body freshenTypeVar
      instantiateL v (ABT.bind body (Type.universal v))
      modifyContext (retract (Universal v))
    _ -> fail ("could not instantiate left: " ++ show t)

-- | Instantiate the given existential such that it is
-- a supertype of the given type, updating the context
-- in the process.
instantiateR :: Var v => Type v -> v -> M v ()
instantiateR t v = getContext >>= \ctx -> case Type.monotype t >>= solve ctx v of
  Just ctx -> setContext ctx -- InstRSolve
  Nothing -> case t of
    Type.Existential' v2 | ordered ctx v v2 -> -- InstRReach (both are existential, set v2 = v)
      maybe (fail "InstRReach failed") setContext $
        solve ctx v2 (Type.Monotype (Type.existential v))
    Type.Arrow' i o -> do -- InstRArrow
      [i', o'] <- traverse freshenVar [ABT.v' "i", ABT.v' "o"]
      let s = Solved v (Type.Monotype (Type.arrow (Type.existential i') (Type.existential o')))
      setContext (replace (Existential v) (context [Existential o', Existential i', s]) ctx)
      ctx <- instantiateL i' i >> getContext
      instantiateR (apply ctx o) o'
    Type.Effect' es vt -> do
      es' <- replicateM (length es) (freshNamed "effect")
      vt' <- freshNamed "vt"
      let s = Solved v (Type.Monotype (Type.effect (Type.existential <$> es')
                                                   (Type.existential vt')))
      setContext $ replace (Existential v)
                           (context $ (Existential <$> es') ++
                                      [Existential vt', s])
                           ctx
      Foldable.for_ (es `zip` es') $ \(e, e') -> do
        ctx <- getContext
        instantiateR (apply ctx e) e'
      ctx <- getContext
      instantiateR (apply ctx vt) vt'
    Type.App' x y -> do -- analogue of InstRArr
      -- example foo a <: v' will
      -- 1. create foo', a', add these to the context
      -- 2. add v' = foo' a' to the context
      -- 3. recurse to refine the types of foo' and a'
      [x', y'] <- traverse freshenVar [ABT.v' "x", ABT.v' "y"]
      let s = Solved v (Type.Monotype (Type.app (Type.existential x') (Type.existential y')))
      setContext $ replace (Existential v) (context [Existential y', Existential x', s]) ctx
      ctx <- getContext
      instantiateR (apply ctx x) x'
      ctx <- getContext
      instantiateR (apply ctx y) y'
    Type.Forall' body -> do -- InstRAIIL
      x' <- ABT.freshen body freshenTypeVar
      setContext $ ctx `append` context [Marker x', Existential x']
      instantiateR (ABT.bind body (Type.existential x')) v
      modifyContext (retract (Marker x'))
    _ -> fail $ "could not instantiate right " ++ show t

withEffects :: [Type v] -> M v a -> M v a
withEffects abilities' m =
  M (\menv -> runM m (menv { abilities = abilities' ++ abilities menv }))

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process.
check :: Var v => Term v -> Type v -> M v ()
check e t = getContext >>= \ctx -> scope ("check: " ++ show e ++ ":   " ++ show t) $
  if wellformedType ctx t then
    let
      go (Term.Int64' _) _ = subtype Type.int64 t -- 1I
      go (Term.UInt64' _) _ = subtype Type.uint64 t -- 1I
      go (Term.Float' _) _ = subtype Type.float t -- 1I
      go (Term.Boolean' _) _ = subtype Type.boolean t -- 1I
      go (Term.Text' _) _ = subtype Type.text t -- 1I
      go Term.Blank' _ = pure () -- somewhat hacky short circuit; blank checks successfully against all types
      go _ (Type.Forall' body) = do -- ForallI
        x <- extendUniversal =<< ABT.freshen body freshenTypeVar
        check e (ABT.bind body (Type.universal x))
        modifyContext $ retract (Universal x)
      go (Term.Lam' body) (Type.Arrow' i o) = do -- =>I
        x <- ABT.freshen body freshenVar
        modifyContext' (extend (Ann x i))
        check (ABT.bind body (Term.var x)) o
        modifyContext (retract (Ann x i))
      go (Term.Let1' binding e) t = do
        v <- ABT.freshen e freshenVar
        tbinding <- synthesize binding
        modifyContext' (extend (Ann v tbinding))
        check (ABT.bind e (Term.var v)) t
        modifyContext (retract (Ann v tbinding))
      go (Term.LetRecNamed' [] e) t = check e t
      go (Term.LetRec' letrec) t = do
        (marker, e) <- annotateLetRecBindings letrec
        check e t
        modifyContext (retract marker)
      go (Term.Handle' h body) t = do
        [e, i] <- sequence [freshNamed "e", freshNamed "i"]
        appendContext $ context [Existential e, Existential i]
        check h $ Type.effectV (Type.existential e) (Type.existential i) `Type.arrow` t
        withEffects [Type.existential e] $ check body (Type.effect [Type.existential e] t)
      go _ _ = do -- Sub
        a <- synthesize e; ctx <- getContext
        subtype (apply ctx a) (apply ctx t)
      (as, t') = Type.stripEffect t
    in withEffects as $ go e t'
  else
    scope ("context: " ++ show ctx) .
    scope ("term: " ++ show e) .
    scope ("type: " ++ show t) .
    scope ("context well formed: " ++ show (wellformed ctx)) .
    scope ("type well formed wrt context: " ++ show (wellformedType ctx t))
    $ fail "check failed"

-- | Synthesize and generalize the type of each binding in a let rec
-- and return the new context in which all bindings are annotated with
-- their type. Also returns the freshened version of `body` and a marker
-- which should be used to retract the context after checking/synthesis
-- of `body` is complete. See usage in `synthesize` and `check` for `LetRec'` case.
annotateLetRecBindings :: Var v => ((v -> M v v) -> M v ([(v, Term v)], Term v)) -> M v (Element v, Term v)
annotateLetRecBindings letrec = do
  (bindings, body) <- letrec freshenVar
  let vs = map fst bindings
  -- generate a fresh existential variable `e1, e2 ...` for each binding
  es <- traverse freshenVar vs
  e1 <- if null vs then fail "impossible" else pure $ head es
  -- Introduce these existentials into the context and
  -- annotate each term variable w/ corresponding existential
  -- [marker e1, 'e1, 'e2, ... v1 : 'e1, v2 : 'e2 ...]
  appendContext $ context (Marker e1 : map Existential es ++ zipWith Ann vs (map Type.existential es))
  -- check each `bi` against `ei`; sequencing resulting contexts
  Foldable.traverse_ (\(e,(_,binding)) -> check binding (Type.existential e)) (zip es bindings)
  -- compute generalized types `gt1, gt2 ...` for each binding `b1, b2...`;
  -- add annotations `v1 : gt1, v2 : gt2 ...` to the context
  (ctx1, ctx2) <- breakAt (Marker e1) <$> getContext
  let gen e = generalizeExistentials ctx2 (Type.existential e)
  let annotations = zipWith Ann vs (map gen es)
  marker <- Marker <$> freshenVar (ABT.v' "let-rec-marker")
  setContext (ctx1 `append` context (marker : annotations))
  pure $ (marker, body)

-- | Synthesize the type of the given term, updating the context in the process.
-- | Figure 11 from the paper
synthesize :: Var v => Term v -> M v (Type v)
synthesize e = scope ("synth: " ++ show e) $ logContext "synthesize" >> go e where
  go :: Var v => Term v -> M v (Type v)
  go (Term.Var' v) = getContext >>= \ctx -> case lookupType ctx v of -- Var
    Nothing -> fail $ "type not known for term var: " ++ show v
    Just t -> pure t
  go Term.Blank' = do
    v <- freshVar
    pure $ Type.forall (TypeVar.Universal v) (Type.universal v)
  go (Term.Ann' (Term.Ref' _) t) = case ABT.freeVars t of
    s | Set.null s ->
      -- innermost Ref annotation assumed to be correctly provided by `synthesizeClosed`
      pure t
    s | otherwise ->
      fail $ "type annotation contains free variables " ++ show (Set.toList s)
  go (Term.Ref' h) = fail $ "unannotated reference: " ++ show h
  go (Term.Constructor' r cid) = getConstructorType r cid
  go (Term.Ann' e' t) = t <$ check e' t
  go (Term.Float' _) = pure Type.float -- 1I=>
  go (Term.Int64' _) = pure Type.int64 -- 1I=>
  go (Term.UInt64' _) = pure Type.uint64 -- 1I=>
  go (Term.Boolean' _) = pure Type.boolean
  go (Term.Text' _) = pure Type.text
  go (Term.App' f arg) = do -- ->E
    ft <- synthesize f
    ctx <- getContext
    synthesizeApp (apply ctx ft) arg
  go (Term.Vector' v) = synthesize (desugarVector (Foldable.toList v))
  go (Term.Let1' binding e) | Set.null (ABT.freeVars binding) = do
    -- special case when it is definitely safe to generalize - binding contains
    -- no free variables, i.e. `let id x = x in ...`
    decls <- getDataDeclarations
    abilities <- getAbilities
    t  <- synthesizeClosed' abilities decls binding
    v' <- ABT.freshen e freshenVar
    e  <- pure $ ABT.bind e (Term.builtin (Var.name v') `Term.ann` t)
    synthesize e
  go (Term.Let1' binding e) = do
    -- literally just convert to a lambda application and call synthesize!
    -- NB: this misses out on let generalization
    -- let x = blah p q in foo y <=> (x -> foo y) (blah p q)
    v' <- ABT.freshen e freshenVar
    e  <- pure $ ABT.bind e (Term.var v')
    synthesize (Term.lam v' e `Term.app` binding)
  --go (Term.Let1' binding e) = do
  --  -- note: no need to freshen binding, it can't refer to v
  --  tbinding <- synthesize binding
  --  v' <- ABT.freshen e freshenVar
  --  appendContext (context [Ann v' tbinding])
  --  t <- synthesize (ABT.bind e (Term.var v'))
  --  pure t
  --  -- TODO: figure out why this retract sometimes generates invalid contexts,
  --  -- (ctx, ctx2) <- breakAt (Ann v' tbinding) <$> getContext
  --  -- as in (f -> let x = (let saved = f in 42) in 1)
  --  -- removing the retract and generalize 'works' for this example
  --  -- generalizeExistentials ctx2 t <$ setContext ctx
  go (Term.Lam' body) = do -- ->I=> (Full Damas Milner rule)
    [arg, i, o] <- sequence [ABT.freshen body freshenVar, freshVar, freshVar]
    appendContext $
      context [Marker i, Existential i, Existential o, Ann arg (Type.existential i)]
    body <- pure $ ABT.bind body (Term.var arg)
    check body (Type.existential o)
    (ctx1, ctx2) <- breakAt (Marker i) <$> getContext
    -- unsolved existentials get generalized to universals
    setContext ctx1
    pure $ generalizeExistentials
             ctx2
             (Type.existential i `Type.arrow` Type.existential o)
  go (Term.LetRecNamed' [] body) = synthesize body
  go (Term.LetRec' letrec) = do
    (marker, e) <- annotateLetRecBindings letrec
    t <- synthesize e
    (ctx, ctx2) <- breakAt marker <$> getContext
    generalizeExistentials ctx2 t <$ setContext ctx
  go (Term.If' cond t f) = foldM synthesizeApp Type.iff [cond, t, f]
  go (Term.And' a b) = foldM synthesizeApp Type.andor [a, b]
  go (Term.Or' a b) = foldM synthesizeApp Type.andor [a, b]
  -- { 42 }
  go (Term.EffectPure' a) = do
    e <- freshenVar (Var.named "e")
    at <- synthesize a
    pure . Type.forall (TypeVar.Universal e) $ Type.effectV (Type.universal e) at
  go (Term.EffectBind' r cid args k) = do
    cType <- getConstructorType r cid
    let arity = Type.arity cType
    -- TODO: error message algebra
    when (length args /= arity) .  fail $
      "Effect constructor wanted " <> show arity <> " arguments " <> "but got "
      <> show (length args)
    ([eType], iType) <-
      Type.stripEffect <$> withoutAbilityCheck (foldM synthesizeApp cType args)
    rType <- synthesizeApp (Type.flipApply iType) k
    pure $ Type.effectV eType rType
  go (Term.Match' scrutinee cases) = do
    scrutineeType <- synthesize scrutinee
    outputTypev <- freshenVar (Var.named "match-output")
    let outputType = Type.existential outputTypev
    appendContext $ context [Existential outputTypev]
    Foldable.traverse_ (checkCase scrutineeType outputType) cases
    ctx <- getContext
    pure $ apply ctx outputType
  go (Term.Handle' h body) = do
    [e, i, o] <- sequence [freshNamed "e", freshNamed "i", freshNamed "o"]
    appendContext $
      context [Existential e, Existential i, Existential o]
    check h $
      Type.effectV (Type.existential e) (Type.existential i)
        `Type.arrow` Type.existential o
    -- output type of the body should {e} i
    -- body : {IO, State Int} Text
    -- body <: {State Int} Text -- don't want this
    -- bodyType <- synthesize the body, obtain `{e,e1,e2...} a`
    -- we strip out `e` from the effect list of body type (if effect list is there)
    -- subtype check
    withEffects [Type.existential e] $ do
      Type.Effect'' es bodyType <- synthesize body
      bodyType `subtype` Type.existential i
      ctx <- getContext
      let normalizedEs = apply ctx <$> es
          normalizedE = apply ctx (Type.existential e)
      -- todo: should we just remove the first one we see?
          es' = filter (\t -> t /= normalizedE) normalizedEs
      case es' of
        [] -> pure $ apply ctx (Type.existential o)
        _ -> pure $ apply ctx (Type.effect es' $ Type.existential o)
  go e = fail $ "unknown case in synthesize " ++ show e

-- data MatchCase a = MatchCase Pattern (Maybe a) a
{-
type Optional b c = None | Some b c
let blah : Optional Int64 Int64
    blah = ...

    case blah of
      Some x (Some y z) | x < 10 -> x + y + z

--becomes--

let x = _
    y = _
    z = _
    pat : Optional Int64 Int64
    pat = Optional.Some x (Some y z)
    -- from here on is rhs'
    guard : Boolean
    guard = x <_Int64 +10
    x +_Int64 y
-}
checkCase :: Var v => Type v -> Type v -> Term.MatchCase (Term v) -> M v ()
checkCase scrutineeType outputType (Term.MatchCase pat guard rhs) =
  -- Get the variables bound in the pattern
  let (vs, body) = case rhs of
        ABT.AbsN' vars bod -> (vars, bod)
        _ -> ([], rhs)
      -- Make up a term that involves the guard if present
      rhs' = case guard of
        Just g -> Term.let1 [(Var.named "_", Term.ann g Type.boolean)] body
        Nothing -> body
      -- Convert pattern to a Term
      patTerm = evalState (patternToTerm pat) vs
      newBody = Term.let1 [(Var.named "_", patTerm `Term.ann` scrutineeType)] rhs'
      entireCase = foldr (\v t -> Term.let1 [(v, Term.blank)] t) newBody vs
  in check entireCase outputType

-- Make up a fake term for the pattern, that we can typecheck
patternToTerm :: Var v => Pattern -> State [v] (Term v)
patternToTerm pat = case pat of
  Pattern.Boolean b -> pure $ Term.boolean b
  Pattern.Int64 n -> pure $ Term.int64 n
  Pattern.UInt64 n -> pure $ Term.uint64 n
  Pattern.Float n -> pure $ Term.float n
  -- similar for other literals
  Pattern.Constructor r cid pats -> do
    outputTerms <- traverse patternToTerm pats
    pure $ Term.apps (Term.constructor r cid) outputTerms
  Pattern.Var -> do
    (h : t) <- get
    put t
    pure $ Term.var h
  Pattern.Unbound -> pure Term.blank
  Pattern.As p -> do
    (h : t) <- get
    put t
    tm <- patternToTerm p
    pure . Term.let1 [(h, tm)] $ Term.var h
  Pattern.EffectPure p -> Term.effectPure <$> patternToTerm p
  Pattern.EffectBind r cid pats kpat -> do
    outputTerms <- traverse patternToTerm pats
    kTerm <- patternToTerm kpat
    pure $ Term.effectBind r cid outputTerms kTerm

-- | Synthesize the type of the given term, `arg` given that a function of
-- the given type `ft` is being applied to `arg`. Update the context in
-- the process.
-- e.g. in `(f:t) x` -- finds the type of (f x) given t and x.
synthesizeApp :: Var v => Type v -> Term v -> M v (Type v)
synthesizeApp ft arg = scope ("synthesizeApp: " ++ show ft ++ " " ++ show arg) $ go ft where
  go (Type.Forall' body) = do -- Forall1App
    v <- ABT.freshen body freshenTypeVar
    appendContext (context [Existential v])
    synthesizeApp (ABT.bind body (Type.existential v)) arg
  go (Type.Arrow' i o) = do -- ->App
    let (es, _) = Type.stripEffect o
    abilityCheck es
    ambientEs <- getAbilities
    o <$ check arg (Type.effect ambientEs i)
  go (Type.Existential' a) = do -- a^App
    [i,o] <- traverse freshenVar [a, ABT.v' "o"]
    let soln = Type.Monotype (Type.existential i `Type.arrow` Type.existential o)
    let ctxMid = context [Existential o, Existential i, Solved a soln]
    modifyContext' $ replace (Existential a) ctxMid
    logContext "synthesizeApp (Existential)"
    Type.existential o <$ check arg (Type.existential i)
  go _ = scope "unable to synthesize type of application" $
         scope ("function type: " ++ show ft) $
         fail  ("arg: " ++ show arg)

-- | For purposes of typechecking, we translate `[x,y,z]` to the term
-- `Vector.prepend x (Vector.prepend y (Vector.prepend z Vector.empty))`,
-- where `Vector.prepend : forall a. a -> Vector a -> a` and
--       `Vector.empty : forall a. Vector a`
-- todo: easiest to desugar as a variadic function forall a . a -> a -> a -> a -> Vector a
-- of the appropriate arity
-- also rename Vector -> Sequence
desugarVector :: Var v => [Term v] -> Term v
desugarVector ts = case ts of
  [] -> Term.builtin "Vector.empty" `Term.ann` Type.forall' ["a"] va
  hd : tl -> (Term.builtin "Vector.prepend" `Term.ann` prependT) `Term.app` hd `Term.app` desugarVector tl
  where prependT = Type.forall' ["a"] (Type.v' "a" `Type.arrow` (va `Type.arrow` va))
        va = Type.vectorOf (Type.v' "a")

annotateRefs :: (Applicative f, Ord v)
             => (Reference -> Noted f (Type.Type v))
             -> Term v
             -> Noted f (Term v)
annotateRefs synth term = ABT.visit f term where
  f (Term.Ref' h) = Just (Term.ann (Term.ref h) <$> (ABT.vmap TypeVar.Universal <$> synth h))
  f _ = Nothing

synthesizeClosed
  :: (Monad f, Var v)
  => [Type v]
  -> Type.Env f v
  -> (Reference -> Noted f (DataDeclaration v))
  -> Term v
  -> Noted f (Type v)
synthesizeClosed abilities synthRef lookupDecl term = do
  let declRefs = Set.toList $ Term.referencedDataDeclarations term
  term <- annotateRefs synthRef term
  decls <- Map.fromList <$> traverse (\r -> (r,) <$> lookupDecl r) declRefs
  synthesizeClosedAnnotated abilities decls term

synthesizeClosed' :: Var v
                  => [Type v]
                  -> DataDeclarations v
                  -> Term v
                  -> M v (Type v)
synthesizeClosed' abilities decls term | Set.null (ABT.freeVars term) =
  verifyDataDeclarations decls *>
  case runM (synthesize term) (MEnv env0 abilities decls True) of
    Left err -> M $ \_ -> Left err
    Right (t,env) -> pure $ generalizeExistentials (ctx env) t
synthesizeClosed' _abilities _decls term =
  fail $ "cannot synthesize term with free variables: " ++ show (Set.toList (ABT.freeVars term))

synthesizeClosedAnnotated :: (Monad f, Var v)
                          => [Type v]
                          -> DataDeclarations v
                          -> Term v
                          -> Noted f (Type v)
synthesizeClosedAnnotated abilities decls term | Set.null (ABT.freeVars term) = do
  Note.fromEither $
    runM (verifyDataDeclarations decls *> synthesize term)
         (MEnv env0 abilities decls True)
      >>= \(t,env) ->
    -- we generalize over any remaining unsolved existentials
        pure $ generalizeExistentials (ctx env) t
synthesizeClosedAnnotated _abilities _decls term =
  fail $ "cannot synthesize term with free variables: " ++ show (map Var.name $ Set.toList (ABT.freeVars term))

verifyDataDeclarations :: Var v => DataDeclarations v -> M v ()
verifyDataDeclarations decls = forM_ (Map.toList decls) $ \(r, decl) -> do
  let ctors = DataDeclaration.constructors decl
  forM_ ctors $ \(ctorName,typ) ->
    if Set.null $ ABT.freeVars typ then pure ()
    else fail $ "encountered free vars " ++
                show (Var.qualifiedName <$> Set.toList (ABT.freeVars typ)) ++ " in " ++
                show r ++ "#" ++ show (Var.qualifiedName ctorName)

-- boring instances
instance Applicative (M v) where
  pure = return
  (<*>) = ap

instance Functor (M v) where
  fmap = liftM

instance Monad (M v) where
  return a = M (\menv -> Right (a, env menv))
  M f >>= g = M (\menv ->
                   f menv >>= (\(a,env') -> runM (g a) (menv {env = env'})))
  fail msg = M (\_ -> Left (Note.note msg))
