{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.SlurpComponent where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)
import qualified Unison.DataDeclaration as DD
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Term as Term
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF

data SlurpComponent v = SlurpComponent {types :: Set v, terms :: Set v}
  deriving (Eq, Ord, Show)

isEmpty :: SlurpComponent v -> Bool
isEmpty sc = Set.null (types sc) && Set.null (terms sc)

empty :: Ord v => SlurpComponent v
empty = SlurpComponent {types = mempty, terms = mempty}

difference :: Ord v => SlurpComponent v -> SlurpComponent v -> SlurpComponent v
difference c1 c2 = SlurpComponent {types = types', terms = terms'}
  where
    types' = types c1 `Set.difference` types c2
    terms' = terms c1 `Set.difference` terms c2

intersection :: Ord v => SlurpComponent v -> SlurpComponent v -> SlurpComponent v
intersection c1 c2 = SlurpComponent {types = types', terms = terms'}
  where
    types' = types c1 `Set.intersection` types c2
    terms' = terms c1 `Set.intersection` terms c2

instance Ord v => Semigroup (SlurpComponent v) where (<>) = mappend

instance Ord v => Monoid (SlurpComponent v) where
  mempty = SlurpComponent {types = mempty, terms = mempty}
  c1 `mappend` c2 =
    SlurpComponent
      { types = types c1 <> types c2,
        terms = terms c1 <> terms c2
      }

-- I'm calling this `closeWithDependencies` because it doesn't just compute
-- the dependencies of the inputs, it mixes them together.  Make sure this
-- is what you want.
closeWithDependencies ::
  forall v a.
  Ord v =>
  TypecheckedUnisonFile v a ->
  SlurpComponent v ->
  SlurpComponent v
closeWithDependencies uf inputs = seenDefns
  where
    seenDefns = foldl' termDeps (SlurpComponent {types = seenTypes, terms = mempty}) (terms inputs)
    seenTypes = foldl' typeDeps mempty (types inputs)

    termDeps :: SlurpComponent v -> v -> SlurpComponent v
    termDeps seen v | Set.member v (terms seen) = seen
    termDeps seen v = fromMaybe seen $ do
      term <- findTerm v
      let -- get the `v`s for the transitive dependency types
          -- (the ones for terms are just the `freeVars below`)
          -- although this isn't how you'd do it for a term that's already in codebase
          tdeps :: [v]
          tdeps = resolveTypes $ Term.dependencies term
          seenTypes :: Set v
          seenTypes = foldl' typeDeps (types seen) tdeps
          seenTerms = Set.insert v (terms seen)
      pure $
        foldl'
          termDeps
          ( seen
              { types = seenTypes,
                terms = seenTerms
              }
          )
          (Term.freeVars term)

    typeDeps :: Set v -> v -> Set v
    typeDeps seen v | Set.member v seen = seen
    typeDeps seen v = fromMaybe seen $ do
      dd <-
        fmap snd (Map.lookup v (UF.dataDeclarations' uf))
          <|> fmap (DD.toDataDecl . snd) (Map.lookup v (UF.effectDeclarations' uf))
      pure $ foldl' typeDeps (Set.insert v seen) (resolveTypes $ DD.dependencies dd)

    resolveTypes :: Set Reference -> [v]
    resolveTypes rs = [v | r <- Set.toList rs, Just v <- [Map.lookup r typeNames]]

    findTerm :: v -> Maybe (Term.Term v a)
    findTerm v = Map.lookup v allTerms

    allTerms = UF.allTerms uf

    typeNames :: Map Reference v
    typeNames = invert (fst <$> UF.dataDeclarations' uf) <> invert (fst <$> UF.effectDeclarations' uf)

    invert :: forall k v. Ord k => Ord v => Map k v -> Map v k
    invert m = Map.fromList (swap <$> Map.toList m)
