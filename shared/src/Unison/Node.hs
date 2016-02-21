{-# LANGUAGE TemplateHaskell #-}

module Unison.Node where

-- import Data.Bytes.Serial (Serial)
import Control.Monad
import Data.Aeson.TH
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import Unison.Eval as Eval
import Unison.Metadata (Metadata)
import Unison.Node.Store (Store)
import Unison.Note (Noted)
import Unison.Paths (Path)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.TermEdit (Action)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Metadata as Metadata
import qualified Unison.Node.Store as Store
import qualified Unison.Paths as Paths
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TermEdit as TermEdit
import qualified Unison.Typechecker as Typechecker

-- | The results of a search.
-- On client, only need to repeat the query if we modify a character
-- at one of the examined positions OR if we add a character to a search
-- that previously returned incomplete results. Appending characters to a
-- search that returned complete results just filters down the set and
-- can be done client-side, assuming the client has the full result set.
data SearchResults v h e =
  SearchResults
    { query :: Metadata.Query
    , references :: [(h, Metadata v h)]
    , matches :: ([e], Int)
    , illTypedMatches :: ([e], Int)
    , positionsExamined :: [Int] }

data LocalInfo e t =
  LocalInfo
    { localTerm :: e
    , localType :: t
    , localAdmissibleType :: t
    , localVariables :: [e]
    , localOverapplications :: [Int]
    , localVariableApplications :: [e] }

deriveJSON defaultOptions ''SearchResults
deriveJSON defaultOptions ''LocalInfo

-- | The Unison Node API:
--   * `m` is the monad
--   * `v` is the type of variables
--   * `h` is the type of hashes
--   * `t` is for type
--   * `e` is for term (pnemonic "expression")
data Node m v h t e = Node {
  -- | Obtain the type of the given subterm, assuming the path is valid
  admissibleTypeAt :: e -> Path -> Noted m t,
  -- | Create a new term and provide its metadata
  createTerm :: e -> Metadata v h -> Noted m h,
  -- | Create a new type and provide its metadata
  createType :: t -> Metadata v h -> Noted m h,
  -- | Lookup the direct dependencies of @k@, optionally limited to the given set
  dependencies :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Lookup the set of terms/types depending directly on the given @k@, optionally limited to the given set
  dependents :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Modify the given subterm, which may fail. First argument is the root path.
  -- Second argument is path relative to the root.
  -- Returns (root path, original e, edited e, new cursor position)
  editTerm :: Path -> Path -> Action v -> e -> Noted m (Maybe (Path,e,e,Path)),
  -- Evaluate all terms, returning a list of (path, original e, evaluated e)
  evaluateTerms :: [(Path, e)] -> Noted m [(Path,e,e)],
  -- | Return information about local types and and variables in scope
  localInfo :: e -> Path -> Noted m (LocalInfo e t),
  -- | Access the metadata for the term and/or types identified by @k@
  metadatas :: [h] -> Noted m (Map h (Metadata v h)),
  -- | Search for a term, optionally constrained to be of the given type
  search :: e -> Path -> Int -> Metadata.Query -> Maybe t -> Noted m (SearchResults v h e),
  -- | Lookup the source of the term identified by @h@
  terms :: [h] -> Noted m (Map h e),
  -- | Lookup the dependencies of @h@, optionally limited to those that intersect the given set
  transitiveDependencies :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Lookup the set of terms or types which depend on the given @k@, optionally limited to those that intersect the given set
  transitiveDependents :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Lookup the source of the type identified by @h@
  types :: [h] -> Noted m (Map h t),
  -- | Obtain the type of the given subterm, assuming the path is valid
  typeAt :: e -> Path -> Noted m t,
  -- | Update the metadata associated with the given term or type
  updateMetadata :: h -> Metadata v h -> Noted m ()
}

node :: (Show v, Monad f, Var v)
     => Eval (Noted f) v
     -> (Term v -> Reference)
     -> Store f v
     -> Node f v Reference.Reference (Type v) (Term v)
node eval hash store =
  let
    readTypeOf = Store.typeOfTerm store

    admissibleTypeAt e loc =
      Typechecker.admissibleTypeAt readTypeOf loc e

    createTerm e md = do
      t <- Typechecker.synthesize readTypeOf e
      let r = hash e
      pure r <* case r of
        Reference.Builtin _ ->
          Store.writeMetadata store r md -- can't change builtin types, just metadata
        Reference.Derived h -> do
          Store.writeTerm store h e
          Store.writeMetadata store r md
          Store.annotateTerm store r t

    createType _ _ = error "todo - createType"

    dependencies _ (Reference.Builtin _) = pure Set.empty
    dependencies limit (Reference.Derived h) = let trim = maybe id Set.intersection limit in do
      e <- Store.readTerm store h
      pure $ trim (Set.map Reference.Derived (Term.dependencies e))

    dependents limit h = do
      hs <- Store.hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (Set.toList hs)
      pure $ Set.fromList [x | (x,deps) <- hs', Set.member h deps]

    edit rootPath path action e = case Paths.atTerm rootPath e of
      Nothing -> pure Nothing
      Just e -> f <$> TermEdit.interpret eval (Store.readTerm store) path action e
        where f Nothing = Nothing
              f (Just (newPath,e')) = Just (rootPath,e,e',newPath)

    evaluateTerms es = traverse go es
      where go (path,e) = (\e' -> (path,e,e')) <$> Eval.whnf eval (Store.readTerm store) e

    metadatas hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> Store.readMetadata store h) hs)

    localInfo e loc = do
      current <- Typechecker.typeAt readTypeOf loc e
      admissible <- Typechecker.admissibleTypeAt readTypeOf loc e
      locals <- Typechecker.locals readTypeOf loc e
      annotatedLocals <- pure $ map (\(v,t) -> Term.var v `Term.ann` t) locals
      let f focus = maybe (pure False)
                          (\e -> Typechecker.wellTyped readTypeOf e)
                          (Paths.modifyTerm (const focus) loc e)
      let fi (e,_) = f e
      let currentApplies = maybe [] (\e -> TermEdit.applications e admissible) (Paths.atTerm loc e) `zip` [0..]
      matchingCurrentApplies <- case Paths.atTerm loc e of
        -- if we're pointing to a Var, matchingCurrentApplies is redundant with `matchingLocals`
        Just (Term.Var' _) -> pure []
        _ -> map snd <$> filterM fi currentApplies
      subterm <- maybe (fail "invalid path") pure (Paths.atTerm loc e)
      matchingLocals <- filterM f (locals >>= (\(v,t) -> TermEdit.applications (Term.var v) t))
      pure $ LocalInfo subterm current admissible annotatedLocals matchingCurrentApplies matchingLocals

    search e loc limit query _ =
      let
        typeOk focus = maybe (pure False)
                             (\e -> Typechecker.wellTyped readTypeOf e)
                             (Paths.modifyTerm (const focus) loc e)
        elaborate h = (\t -> TermEdit.applications (Term.ref h) t) <$> readTypeOf h
        queryOk e = do mds <- traverse (Store.readMetadata store) (Set.toList (Term.dependencies' e))
                       pure $ any (Metadata.matches query) mds
        trim rs =
          let rs' = sortBy (comparing fst) (map (\e -> (negate (Term.countBlanks e), e)) rs)
          in (map snd (take limit rs'), length (drop limit rs'))
      in
      do
        hs <- Store.hashes store Nothing
        tmatches <- do es <- traverse elaborate (Set.toList hs)
                       filterM typeOk (join es)
        qmatches <- filterM queryOk tmatches
        qmatches' <- filterM queryOk (map Term.ref (Set.toList hs))
        illtypedQmatches <-
          -- return type annotated versions of ill-typed terms
          let welltypedRefs = Set.fromList (map hash qmatches)
              terms = filter (\r -> Set.notMember (hash r) welltypedRefs) qmatches'
          in zipWith Term.ann terms <$> traverse (Typechecker.synthesize readTypeOf) terms
        mds <- mapM (\h -> (,) h <$> Store.readMetadata store h)
                    (Set.toList (Set.unions (map Term.dependencies' (illtypedQmatches ++ qmatches))))
        pure $ SearchResults
          query
          mds
          (trim qmatches)
          (trim illtypedQmatches)
          (Metadata.queryPositions query)

    readTermRef (Reference.Derived h) = Store.readTerm store h
    readTermRef r = pure (Term.ref r)

    terms hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> readTermRef h) hs)

    transitiveDependencies = error "todo"

    transitiveDependents = error "todo"

    types hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> readTypeOf h) hs)

    typeAt ctx loc =
      Typechecker.typeAt readTypeOf loc ctx

    updateMetadata = Store.writeMetadata store
  in Node
       admissibleTypeAt
       createTerm
       createType
       dependencies
       dependents
       edit
       evaluateTerms
       localInfo
       metadatas
       search
       terms
       transitiveDependencies
       transitiveDependents
       types
       typeAt
       updateMetadata
