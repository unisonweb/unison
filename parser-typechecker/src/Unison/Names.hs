{-# Language RecordWildCards #-}

module Unison.Names where

import Data.List (foldl')
import           Data.Map                       ( Map )
import qualified Data.Map as Map
import           Data.Text                      ( Text )
import qualified Data.Text as Text
import           Unison.Reference               ( Reference )
import           Unison.Term                    ( AnnotatedTerm )
import qualified Unison.Term as Term
import           Unison.Type                    ( AnnotatedType )
import qualified Unison.Type as Type
import Unison.Var (Var)
import qualified Unison.Var as Var

type Name = Text

data Names v a = Names
  { termNames :: Map Name (AnnotatedTerm v a, AnnotatedType v a)
  , patternNames :: Map Name (Reference, Int)
  , typeNames :: Map Name Reference
  }

instance Semigroup (Names v a) where (<>) = mappend

instance Monoid (Names v a) where
  mempty = Names mempty mempty mempty
  Names e1 p1 t1 `mappend` Names e2 p2 t2 =
    Names (e1 `unionL` e2) (p1 `unionL` p2) (t1 `unionL` t2)
    where
      unionL :: Ord k => Map k v -> Map k v -> Map k v
      unionL = Map.unionWith const

fromTypeNamesV :: Var v => [(v, Reference)] -> Names v x
fromTypeNamesV env = Names mempty mempty (Map.fromList env')
  where
  env' = [(Var.name v, r) | (v, r) <- env ]

filterTypes :: (Name -> Bool) -> Names v a -> Names v a
filterTypes f (Names {..}) = Names termNames patternNames m2
  where
  m2 = Map.fromList $ [(k,v) | (k,v) <- Map.toList typeNames, f k]

patternNameds :: Names v a -> String -> Maybe (Reference, Int)
patternNameds ns s = patternNamed ns (Text.pack s)

patternNamed :: Names v a -> Name -> Maybe (Reference, Int)
patternNamed ns n = Map.lookup n (patternNames ns)

bindBuiltinTypes :: Var v => Names v x -> AnnotatedType v a -> AnnotatedType v a
bindBuiltinTypes ns t = Type.bindBuiltins typeNames' t
  where
  typeNames' = [ (Var.named v, r) | (v, r) <- Map.toList $ typeNames ns ]

bindBuiltinTerms :: Var v => Names v a -> AnnotatedTerm v a -> AnnotatedTerm v a
bindBuiltinTerms ns e = Term.bindBuiltins termBuiltins typeBuiltins e
  where
  termBuiltins = [ (Var.named v, e) | (v, (e,_typ)) <- Map.toList (termNames ns) ]
  typeBuiltins = [ (Var.named v, t) | (v, t) <- Map.toList (typeNames ns) ]

-- Given a mapping from name to qualified name, update a `PEnv`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `PEnv`,
-- the alias `Some` will map to that same constructor
importing :: Var v => [(v,v)] -> Names v a -> Names v a
importing shortToLongName0 (Names {..}) = let
  go :: Ord k => Map k v -> (k, k) -> Map k v
  go m (qname, shortname) = case Map.lookup qname m of
    Nothing -> m
    Just v  -> Map.insert shortname v m
  shortToLongName = [
    (Var.name v, Var.name v2) | (v,v2) <- shortToLongName0 ]
  terms' = foldl' go termNames shortToLongName
  types' = foldl' go typeNames shortToLongName
  patterns' = foldl' go patternNames shortToLongName
  in Names terms' patterns' types'

