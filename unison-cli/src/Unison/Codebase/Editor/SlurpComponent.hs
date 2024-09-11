module Unison.Codebase.Editor.SlurpComponent
  ( -- * Slurp component
    SlurpComponent (..),

    -- ** Basic constructors
    empty,
    fromTerms,
    fromTypes,
    fromCtors,

    -- ** Predicates
    isEmpty,

    -- ** Closure
    closeWithDependencies,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple (swap)
import Unison.DataDeclaration qualified as DD
import Unison.Prelude hiding (empty)
import Unison.Reference (TypeReference)
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.Util.Defns (Defns (..))

data SlurpComponent = SlurpComponent
  { types :: Set Symbol,
    terms :: Set Symbol,
    ctors :: Set Symbol
  }
  deriving (Eq, Generic, Ord, Show)

isEmpty :: SlurpComponent -> Bool
isEmpty sc = Set.null sc.types && Set.null sc.terms && Set.null sc.ctors

empty :: SlurpComponent
empty = SlurpComponent {types = Set.empty, terms = Set.empty, ctors = Set.empty}

instance Semigroup SlurpComponent where
  c1 <> c2 =
    SlurpComponent
      { types = c1.types <> c2.types,
        terms = c1.terms <> c2.terms,
        ctors = c1.ctors <> c2.ctors
      }

instance Monoid SlurpComponent where
  mempty = empty

-- I'm calling this `closeWithDependencies` because it doesn't just compute
-- the dependencies of the inputs, it mixes them together.  Make sure this
-- is what you want.
closeWithDependencies ::
  forall a.
  TypecheckedUnisonFile Symbol a ->
  SlurpComponent ->
  SlurpComponent
closeWithDependencies uf inputs = seenDefns {ctors = constructorDeps}
  where
    seenDefns = foldl' termDeps (SlurpComponent {terms = mempty, types = seenTypes, ctors = mempty}) inputs.terms
    seenTypes = foldl' typeDeps mempty inputs.types

    constructorDeps :: Set Symbol
    constructorDeps = UF.constructorsForDecls seenTypes uf

    termDeps :: SlurpComponent -> Symbol -> SlurpComponent
    termDeps seen v | Set.member v seen.terms = seen
    termDeps seen v = fromMaybe seen do
      term <- findTerm v
      let -- get the `v`s for the transitive dependency types
          -- (the ones for terms are just the `freeVars below`)
          -- although this isn't how you'd do it for a term that's already in codebase
          tdeps :: [Symbol]
          tdeps = resolveTypes (Term.dependencies term).types
          seenTypes :: Set Symbol
          seenTypes = foldl' typeDeps seen.types tdeps
          seenTerms = Set.insert v seen.terms
      pure $
        foldl'
          termDeps
          ( seen
              & #types .~ seenTypes
              & #terms .~ seenTerms
          )
          (Term.freeVars term)

    typeDeps :: Set Symbol -> Symbol -> Set Symbol
    typeDeps seen v | Set.member v seen = seen
    typeDeps seen v = fromMaybe seen $ do
      dd <-
        fmap snd (Map.lookup v (UF.dataDeclarations' uf))
          <|> fmap (DD.toDataDecl . snd) (Map.lookup v (UF.effectDeclarations' uf))
      pure $ foldl' typeDeps (Set.insert v seen) (resolveTypes $ DD.typeDependencies dd)

    resolveTypes :: Set TypeReference -> [Symbol]
    resolveTypes rs = [v | r <- Set.toList rs, Just v <- [Map.lookup r typeNames]]

    findTerm :: Symbol -> Maybe (Term.Term Symbol a)
    findTerm v = Map.lookup v allTerms

    allTerms = UF.allTerms uf

    typeNames :: Map TypeReference Symbol
    typeNames = invert (fst <$> UF.dataDeclarations' uf) <> invert (fst <$> UF.effectDeclarations' uf)

    invert :: forall k v. (Ord k) => (Ord v) => Map k v -> Map v k
    invert m = Map.fromList (swap <$> Map.toList m)

fromTypes :: Set Symbol -> SlurpComponent
fromTypes vs = SlurpComponent {terms = Set.empty, types = vs, ctors = Set.empty}

fromTerms :: Set Symbol -> SlurpComponent
fromTerms vs = SlurpComponent {terms = vs, types = Set.empty, ctors = Set.empty}

fromCtors :: Set Symbol -> SlurpComponent
fromCtors vs = SlurpComponent {terms = Set.empty, types = Set.empty, ctors = vs}
