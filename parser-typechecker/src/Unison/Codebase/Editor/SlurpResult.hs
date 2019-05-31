{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.SlurpResult where

import Data.Map (Map)
import Data.Set (Set)
import Unison.Name ( Name )
import Unison.Parser ( Ann )
import Unison.Var (Var)
import qualified Data.Set as Set
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
  -- for each v in adds, move to blocked if transitive dependency in removed
  blocked = defsWithBlockedDependencies sr <>
    SC.difference (blockedTerms <> blockedTypes) removed

  uf = originalFile sr
  blockedTypes = foldMap doType . SC.types $ adds sr <> updates sr where
    -- include this type if it or any of its dependencies are removed
    doType :: v -> SlurpComponent v
    doType v =
      if null $ Set.intersection (SC.types removed)
                                 (SC.types (SC.closeWithDependencies uf vc))
      then mempty else vc
      where vc = mempty { types = Set.singleton v }

  blockedTerms = foldMap doTerm . SC.terms $ adds sr <> updates sr where
    doTerm :: v -> SlurpComponent v
    doTerm v =
      if mempty == SC.intersection removed (SC.closeWithDependencies uf vc)
      then mempty else vc
      where vc = mempty { terms = Set.singleton v }

-- Move `updates` to `collisions`, and move any dependents of those updates to `*WithBlockedDependencies`.
-- Subtract stuff from `extraDefinitions` that isn't in `adds` or `updates`
disallowUpdates :: forall v. Var v => SlurpResult v -> SlurpResult v
disallowUpdates sr =
  let sr2 = subtractComponent (updates sr) sr
  in sr2 { collisions = collisions sr2 <> updates sr }

isNonempty :: Ord v => SlurpResult v -> Bool
isNonempty s = Monoid.nonEmpty (adds s) || Monoid.nonEmpty (updates s)
