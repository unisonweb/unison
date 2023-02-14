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

    -- ** Set operations
    difference,
    intersection,

    -- ** Closure
    closeWithDependencies,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple (swap)
import qualified Unison.DataDeclaration as DD
import Unison.Prelude hiding (empty)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import qualified Unison.Term as Term
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF

data SlurpComponent = SlurpComponent
  { types :: Set Symbol,
    terms :: Set Symbol,
    ctors :: Set Symbol
  }
  deriving (Eq, Ord, Show)

isEmpty :: SlurpComponent -> Bool
isEmpty sc = Set.null (types sc) && Set.null (terms sc) && Set.null (ctors sc)

empty :: SlurpComponent
empty = SlurpComponent {types = Set.empty, terms = Set.empty, ctors = Set.empty}

difference :: SlurpComponent -> SlurpComponent -> SlurpComponent
difference c1 c2 = SlurpComponent {types = types', terms = terms', ctors = ctors'}
  where
    types' = types c1 `Set.difference` types c2
    terms' = terms c1 `Set.difference` terms c2
    ctors' = ctors c1 `Set.difference` ctors c2

intersection :: SlurpComponent -> SlurpComponent -> SlurpComponent
intersection c1 c2 = SlurpComponent {types = types', terms = terms', ctors = ctors'}
  where
    types' = types c1 `Set.intersection` types c2
    terms' = terms c1 `Set.intersection` terms c2
    ctors' = ctors c1 `Set.intersection` ctors c2

instance Semigroup SlurpComponent where
  c1 <> c2 =
    SlurpComponent
      { types = types c1 <> types c2,
        terms = terms c1 <> terms c2,
        ctors = ctors c1 <> ctors c2
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
    seenDefns = foldl' termDeps (SlurpComponent {terms = mempty, types = seenTypes, ctors = mempty}) (terms inputs)
    seenTypes = foldl' typeDeps mempty (types inputs)

    constructorDeps :: Set Symbol
    constructorDeps = UF.constructorsForDecls seenTypes uf

    termDeps :: SlurpComponent -> Symbol -> SlurpComponent
    termDeps seen v | Set.member v (terms seen) = seen
    termDeps seen v = fromMaybe seen $ do
      term <- findTerm v
      let -- get the `v`s for the transitive dependency types
          -- (the ones for terms are just the `freeVars below`)
          -- although this isn't how you'd do it for a term that's already in codebase
          tdeps :: [Symbol]
          tdeps = resolveTypes $ Term.dependencies term
          seenTypes :: Set Symbol
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

    typeDeps :: Set Symbol -> Symbol -> Set Symbol
    typeDeps seen v | Set.member v seen = seen
    typeDeps seen v = fromMaybe seen $ do
      dd <-
        fmap snd (Map.lookup v (UF.dataDeclarations' uf))
          <|> fmap (DD.toDataDecl . snd) (Map.lookup v (UF.effectDeclarations' uf))
      pure $ foldl' typeDeps (Set.insert v seen) (resolveTypes $ DD.dependencies dd)

    resolveTypes :: Set Reference -> [Symbol]
    resolveTypes rs = [v | r <- Set.toList rs, Just v <- [Map.lookup r typeNames]]

    findTerm :: Symbol -> Maybe (Term.Term Symbol a)
    findTerm v = Map.lookup v allTerms

    allTerms = UF.allTerms uf

    typeNames :: Map Reference Symbol
    typeNames = invert (fst <$> UF.dataDeclarations' uf) <> invert (fst <$> UF.effectDeclarations' uf)

    invert :: forall k v. (Ord k) => (Ord v) => Map k v -> Map v k
    invert m = Map.fromList (swap <$> Map.toList m)

fromTypes :: Set Symbol -> SlurpComponent
fromTypes vs = mempty {types = vs}

fromTerms :: Set Symbol -> SlurpComponent
fromTerms vs = mempty {terms = vs}

fromCtors :: Set Symbol -> SlurpComponent
fromCtors vs = mempty {ctors = vs}
