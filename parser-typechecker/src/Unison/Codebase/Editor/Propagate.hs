{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.Propagate where

import Unison.Prelude

import Unison.Codebase.Editor.Command
import Unison.Codebase.Editor.Output

import           Control.Lens
import           Data.Configurator              ()
import qualified Data.Graph as Graph
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Names3                  ( Names0 )
import qualified Unison.Names2                 as Names
import           Unison.Parser                  ( Ann(..) )
import           Unison.Reference               ( Reference(..) )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import           Unison.Util.Free               ( Free, eval )
import qualified Unison.Util.Relation          as R
import           Unison.Util.TransitiveClosure  (transitiveClosure)
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Codebase.TermEdit (TermEdit(..))
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Util.Star3             as Star3
import Unison.Type (Type)

type F m i v = Free (Command m i v)
type Term v a = Term.AnnotatedTerm v a

-- Description:
------------------
-- For any `Reference` in the frontier which has a type edit, do no propagation.
-- (for now, until we have a richer type edit algebra).
--
-- For any `Reference` in the frontier which has an unconflicted, type-preserving
-- term edit, `old -> new`, replace `old` with `new` in dependents of the
-- frontier, and call `propagate'` recursively on the new frontier.
--
-- If the term is `Typing.Same`, the dependents don't need to be typechecked.
-- If the term is `Typing.Subtype`, and the dependent only has inferred type,
-- it should be re-typechecked, and the new inferred type should be used.
--
-- This will create a whole bunch of new terms in the codebase and move the
-- names onto those new terms. Uses `Term.updateDependencies` to perform
-- the substitutions.
--
-- Algorithm:
----------------
-- compute the frontier relation (dependencies of updated terms)
-- for each dirty definition d:
--  for each member c of cycle(d):
--   construct c', an updated c incorporating all type-preserving edits
--   add an edit c -> c'
--   save c' to a `Map Reference Term`

-- propagate only deals with Terms
-- "dirty" means in need of update
-- "frontier" means updated definitions responsible for the "dirty"
-- errorPPE only needs to have external names, since we're using it to report
--   referents that are outside of the
propagate
  :: forall m i v
   . (Monad m, Var v)
  => PPE.PrettyPrintEnv
  -> Patch
  -> Branch m
  -> F m i v (Branch m)
propagate errorPPE patch b = validatePatch patch >>= \case
  Nothing -> do
    eval $ Notify PatchNeedsToBeConflictFree
    pure b
  Just initialEdits -> do
    initialDirty <-
      R.dom
        <$> computeFrontier (eval . GetDependents)
                            (typePreservingTermEdits patch)
                            names0
    missing :: Set Reference <- missingDependents
      initialDirty
      ( Set.fromList
      . mapMaybe Referent.toTermReference
      . toList
      . Branch.deepReferents
      . Branch.head
      $ b
      )
    if not $ Set.null missing
      then do
        eval . Notify $ PatchInvolvesExternalDependents errorPPE missing
        pure b
      else do
        order <- sortDependentsGraph initialDirty
        let
          getOrdered :: Set Reference -> Map Int Reference
          getOrdered rs = Map.fromList
            [ (i, r) | r <- toList rs, Just i <- [Map.lookup r order] ]
          collectEdits
            :: (Monad m, Var v)
            => Map Reference TermEdit
            -> Map Reference Reference
            -> Map Reference (Term v _, Type v _)
            -> Set Reference
            -> Map Int Reference
            -> F m i v
                 ( Map Reference TermEdit
                 , Map Reference Reference
                 , Map Reference (Term v _, Type v _)
                 )
          -- `replacements` contains the TermEdit.Replace elements of `edits`.
          collectEdits edits replacements newTerms seen todo =
            case Map.minView todo of
              Nothing        -> pure (edits, replacements, newTerms)
              Just (r, todo) -> case r of
                Reference.Builtin _ ->
                  collectEdits edits replacements newTerms seen todo
                Reference.DerivedId _ ->
                  if Map.member r edits || Set.member r seen
                    then collectEdits edits replacements newTerms seen todo
                    else unhashComponent r >>= \case
                      Nothing -> collectEdits edits
                                              replacements
                                              newTerms
                                              (Set.insert r seen)
                                              todo
                      Just componentMap -> do
                        let
                          componentMap' =
                            over _2 (Term.updateDependencies replacements)
                              <$> componentMap
                          hashedComponents' =
                            Term.hashComponents (view _2 <$> componentMap')
                          joinedStuff
                            :: [(Reference, Reference, Term v _, Type v _)]
                          joinedStuff =
                            toList
                              (Map.intersectionWith f
                                                    componentMap'
                                                    hashedComponents'
                              )
                          f (oldRef, _, oldType) (newRef, newTerm) =
                            (oldRef, newRef, newTerm, newType)
                            where newType = oldType
                      -- collect the hashedComponents into edits/replacements/newterms/seen
                          edits' =
                            edits <> (Map.fromList . fmap toEdit) joinedStuff
                          toEdit (r, r', _, _) =
                            (r, TermEdit.Replace r' TermEdit.Same)
                          replacements' = replacements
                            <> (Map.fromList . fmap toReplacement) joinedStuff
                          toReplacement (r, r', _, _) = (r, r')
                          newTerms' = newTerms
                            <> (Map.fromList . fmap toNewTerm) joinedStuff
                          toNewTerm (_, r', tm, tp) = (r', (tm, tp))
                          seen' =
                            seen <> Set.fromList (view _1 <$> joinedStuff)
                        -- plan to update the dependents of this component too
                        dependents <-
                          fmap Set.unions
                          . traverse (eval . GetDependents)
                          . toList
                          . Reference.members
                          $ Reference.componentFor r
                        let todo' = todo <> getOrdered dependents
                        collectEdits edits' replacements' newTerms' seen' todo'
        (termEdits, _replacements, newTerms) <- collectEdits
          initialEdits
          (Map.mapMaybe TermEdit.toReference initialEdits)
          mempty -- newTerms
          mempty -- skip
          (getOrdered initialDirty)
        -- todo: can eliminate this filter if collectEdits doesn't leave temporary terms in the map!
        let termEditTargets =
              Set.fromList . mapMaybe TermEdit.toReference $ toList termEdits
        -- write the new terms to the codebase
        (writeTerms . Map.toList) (Map.restrictKeys newTerms termEditTargets)
        let
          deprecatedTerms, deprecatedTypes :: Set Reference
          deprecatedTerms =
            Set.fromList [ r | (r, TermEdit.Deprecate) <- Map.toList termEdits ]
          deprecatedTypes = Set.fromList
            [ r | (r, TypeEdit.Deprecate) <- R.toList (Patch._typeEdits patch) ]
        -- recursively update names and delete deprecated definitions
        pure $ Branch.step
          ( Branch.stepEverywhere
              (updateNames (Map.mapMaybe TermEdit.toReference termEdits))
          . deleteDeprecatedTerms deprecatedTerms
          . deleteDeprecatedTypes deprecatedTypes
          )
          b
 where
  missingDependents :: Set Reference -> Set Reference -> _ (Set Reference)
  missingDependents dirty known = do
    closure <- transitiveClosure (eval . GetDependents) dirty
    pure $ Set.difference closure known
  sortDependentsGraph :: Set Reference -> _ (Map Reference Int)
  sortDependentsGraph rs = do
    closure    <- transitiveClosure (eval . GetDependents) rs
    dependents <- traverse (\r -> (r, ) <$> (eval . GetDependents) r)
                           (toList closure)
    let graphEdges = [ (r, r, toList deps) | (r, deps) <- toList dependents ]
        (graph, getReference, _) = Graph.graphFromEdges graphEdges
    pure $ Map.fromList
      (zip (view _1 . getReference <$> Graph.topSort graph) [0 ..])
    -- vertex i precedes j whenever i has an edge to j and not vice versa.
    -- vertex i precedes j when j is a dependent of i.
  updateNames :: Map Reference Reference -> Branch0 m -> Branch0 m
  updateNames edits Branch0 {..} = Branch.branch0 terms _types _children _edits
   where
    terms = foldl' replace _terms (Map.toList edits)
    replace s (r, r') = Star3.replaceFact (Referent.Ref r) (Referent.Ref r') s
  deleteDeprecatedTerms, deleteDeprecatedTypes
    :: Set Reference -> Branch0 m -> Branch0 m
  deleteDeprecatedTerms rs =
    over Branch.terms (Star3.deleteFact (Set.map Referent.Ref rs))
  deleteDeprecatedTypes rs = over Branch.types (Star3.deleteFact rs)
  typePreservingTermEdits :: Patch -> Patch
  typePreservingTermEdits Patch {..} = Patch termEdits mempty
    where termEdits = R.filterRan TermEdit.isTypePreserving _termEdits
  writeTerms =
    traverse_ (\(Reference.DerivedId id, (tm, tp)) -> eval $ PutTerm id tm tp)
  names0 = (Branch.toNames0 . Branch.head) b
  validatePatch :: Patch -> F m i v (Maybe (Map Reference TermEdit))
  validatePatch p = pure $ R.toMap (Patch._termEdits p)
  -- Turns a cycle of references into a term with free vars that we can edit
  -- and hash again.
  -- todo: Maybe this an others can be moved to HandleCommand, in the
  --  Free (Command m i v) monad, passing in the actions that are needed.
  -- However, if we want this to be parametric in the annotation type, then
  -- Command would have to be made parametric in the annotation type too.
  unhashComponent
    :: forall m v
     . (Monad m, Var v)
    => Reference
    -> F m i v (Maybe (Map v (Reference, Term v _, Type v _)))
  unhashComponent ref = do
    let component = Reference.members $ Reference.componentFor ref
    isTerm <- eval $ IsTerm ref
    isType <- eval $ IsType ref
    if isTerm
      then do
        let
          termInfo
            :: Reference
            -> F m i v (v, (Reference, Term v Ann, Type v Ann))
          termInfo termRef = do
            tpm <- eval $ LoadTypeOfTerm termRef
            tp  <- maybe (fail $ "Missing type for term " <> show termRef)
                         pure
                         tpm
            case termRef of
              Reference.DerivedId id -> do
                mtm <- eval $ LoadTerm id
                tm  <- maybe (fail $ "Missing term with id " <> show id)
                             pure
                             mtm
                pure (Var.typed (Var.RefNamed termRef), (termRef, tm, tp))
              _ ->
                fail
                  $  "Cannot unhashComponent for a builtin: "
                  ++ show termRef
          unhash m =
            let f (ref, _oldTm, oldTyp) (_ref, newTm) = (ref, newTm, oldTyp)
                dropType (r, tm, _tp) = (r, tm)
            in  Map.intersectionWith f
                                     m
                                     (Term.unhashComponent (dropType <$> m))
        Just . unhash . Map.fromList <$> traverse termInfo (toList component)
      else if isType
        then pure Nothing
        else fail $ "Invalid reference: " <> show ref

-- (d, f) when d is "dirty" (needs update),
--             f is in the frontier (an edited dependency of d),
--         and d depends on f
-- a ⋖ b = a depends directly on b
-- dirty(d) ∧ frontier(f) <=> not(edited(d)) ∧ edited(f) ∧ d ⋖ f
--
-- The range of this relation is the frontier, and the domain is
-- the set of dirty references.
computeFrontier
  :: forall m
   . Monad m
  => (Reference -> m (Set Reference)) -- eg Codebase.dependents codebase
  -> Patch
  -> Names0
  -> m (R.Relation Reference Reference)
computeFrontier getDependents patch names = do
      -- (r,r2) ∈ dependsOn if r depends on r2
  dependsOn <- foldM addDependents R.empty edited
  -- Dirty is everything that `dependsOn` Frontier, minus already edited defns
  pure $ R.filterDom (not . flip Set.member edited) dependsOn
 where
  edited :: Set Reference
  edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)
  addDependents
    :: R.Relation Reference Reference
    -> Reference
    -> m (R.Relation Reference Reference)
  addDependents dependents ref =
    (\ds -> R.insertManyDom ds ref dependents)
      .   Set.filter (Names.contains names)
      <$> getDependents ref
