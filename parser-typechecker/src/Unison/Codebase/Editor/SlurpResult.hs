{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.SlurpResult where

import Control.Applicative
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple (swap)
import Unison.Name ( Name )
import Unison.Parser ( Ann )
import Unison.Reference ( Reference )
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.DataDeclaration as DD
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Monoid as Monoid

import Unison.Codebase.Editor.SlurpComponent (SlurpComponent(..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data SlurpResult v = SlurpResult {
  -- The file that we tried to add from
    originalFile :: UF.TypecheckedUnisonFile v Ann
  -- Extra definitions that were added to satisfy transitive closure,
  -- beyond what the user specified.
  , extraDefinitions :: SlurpComponent v
  -- Previously existed only in the file; now added to the codebase.
  , adds :: SlurpComponent v
  -- Exists in the branch and the file, with the same name and contents.
  , duplicates :: SlurpComponent v
  -- Not added to codebase due to the name already existing
  -- in the branch with a different definition.
  , collisions :: SlurpComponent v
  -- Not added to codebase due to the name existing
  -- in the branch with a conflict (two or more definitions).
  , conflicts :: SlurpComponent v
  -- Names that already exist in the branch, but whose definitions
  -- in `originalFile` are treated as updates.
  , updates :: SlurpComponent v
  -- Names of terms in `originalFile` that couldn't be updated because
  -- they refer to existing constructors. (User should instead do a find/replace,
  -- a constructor rename, or refactor the type that the name comes from).
  , termExistingConstructorCollisions :: Set v
  , constructorExistingTermCollisions :: Set v
  -- -- Already defined in the branch, but with a different name.
  , termAlias :: Map v (Set Name)
  , typeAlias :: Map v (Set Name)
  , defsWithBlockedDependencies :: SlurpComponent v
  } deriving (Show)

-- Remove `removed` from the slurp result, and move any defns with transitive
-- dependencies on the removed component into `defsWithBlockedDependencies`.
-- Also removes `removed` from `extraDefinitions`.
subtractComponent :: forall v. Var v => SlurpComponent v -> SlurpResult v -> SlurpResult v
subtractComponent removed sr =
  sr { adds = SC.difference (adds sr) removed
     , updates = SC.difference (updates sr) removed
     , defsWithBlockedDependencies = blocked
     , extraDefinitions = SC.difference (extraDefinitions sr) blocked
     }
  where
  blocked = defsWithBlockedDependencies sr <>
    SC.difference (blockedTerms <> blockedTypes) removed
  -- for each v in adds, move to blocked if transitive dependency in removed
  termDeps :: SlurpComponent v -> v -> SlurpComponent v
  termDeps seen v | Set.member v (SC.terms seen) = seen
  termDeps seen v = fromMaybe seen $ do
    term <- findTerm v
    let -- get the `v`s for the transitive dependency types
        -- (the ones for terms are just the `freeVars below`)
        -- although this isn't how you'd do it for a term that's already in codebase
        tdeps :: [v]
        tdeps = resolveTypes $ Term.dependencies term
        seenTypes :: Set v
        seenTypes = foldl' typeDeps (SC.types seen) tdeps
        seenTerms = Set.insert v (SC.terms seen)
    pure $ foldl' termDeps (seen { types = seenTypes
                                 , terms = seenTerms})
                           (Term.freeVars term)

  typeDeps :: Set v -> v -> Set v
  typeDeps seen v | Set.member v seen = seen
  typeDeps seen v = fromMaybe seen $ do
    dd <- fmap snd (Map.lookup v (UF.dataDeclarations' uf)) <|>
          fmap (DD.toDataDecl . snd) (Map.lookup v (UF.effectDeclarations' uf))
    pure $ foldl' typeDeps (Set.insert v seen) (resolveTypes $ DD.dependencies dd)

  resolveTypes :: Set Reference -> [v]
  resolveTypes rs = [ v | r <- Set.toList rs, Just v <- [Map.lookup r typeNames]]

  uf = originalFile sr
  findTerm :: v -> Maybe (Term v Ann)
  findTerm v = Map.lookup v allTerms
  allTerms = UF.allTerms uf

  invert :: forall k v . Ord k => Ord v => Map k v -> Map v k
  invert m = Map.fromList (swap <$> Map.toList m)

  typeNames :: Map Reference v
  typeNames = invert (fst <$> UF.dataDeclarations' uf) <> invert (fst <$> UF.effectDeclarations' uf)
  blockedTypes = foldMap doType . SC.types $ adds sr <> updates sr where
    doType :: v -> SlurpComponent v
    doType v =
      if null $ Set.intersection (SC.types removed) (typeDeps mempty v)
      then mempty else mempty { types = Set.singleton v }

  blockedTerms = foldMap doTerm . SC.terms $ adds sr <> updates sr where
    doTerm :: v -> SlurpComponent v
    doTerm v =
      if mempty == SC.intersection removed (termDeps mempty v)
      then mempty else mempty { terms = Set.singleton v }

-- Move `updates` to `collisions`, and move any dependents of those updates to `*WithBlockedDependencies`.
-- Subtract stuff from `extraDefinitions` that isn't in `adds` or `updates`
disallowUpdates :: forall v. Var v => SlurpResult v -> SlurpResult v
disallowUpdates sr =
  let sr2 = subtractComponent (updates sr) sr
  in sr2 { collisions = collisions sr2 <> updates sr }

isNonempty :: Ord v => SlurpResult v -> Bool
isNonempty s = Monoid.nonEmpty (adds s) || Monoid.nonEmpty (updates s)
