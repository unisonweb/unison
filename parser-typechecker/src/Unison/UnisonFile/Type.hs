{-# LANGUAGE RecordWildCards #-}

module Unison.UnisonFile.Type where

import Control.Lens
import Data.Bifunctor (first)
import qualified Unison.ABT as ABT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.Prelude
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import qualified Unison.Reference as Reference
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.WatchKind (WatchKind)

data UnisonFile v a = UnisonFileId
  { dataDeclarationsId :: Map v (TermReferenceId, DataDeclaration v a),
    effectDeclarationsId :: Map v (TermReferenceId, EffectDeclaration v a),
    terms :: [(v, Term v a)],
    watches :: Map WatchKind [(v, Term v a)]
  }
  deriving (Show)

instance Foldable (UnisonFile v) where
  foldMap f (UnisonFile dataDeclarationsId effectDeclarationsId terms watches) =
    (foldMap . foldMap . foldMap) f dataDeclarationsId
      <> (foldMap . foldMap . foldMap) f effectDeclarationsId
      <> (foldMap . foldMap . foldMap) f terms
      <> (foldMap . foldMap . foldMap . foldMap) f watches

pattern UnisonFile ::
  Map v (TypeReference, DataDeclaration v a) ->
  Map v (TypeReference, EffectDeclaration v a) ->
  [(v, Term v a)] ->
  Map WatchKind [(v, Term v a)] ->
  UnisonFile v a
pattern UnisonFile ds es tms ws <-
  UnisonFileId
    (fmap (first Reference.DerivedId) -> ds)
    (fmap (first Reference.DerivedId) -> es)
    tms
    ws

{-# COMPLETE UnisonFile #-}

-- | A UnisonFile after typechecking. Terms are split into groups by
--  cycle and the type of each term is known.
data TypecheckedUnisonFile v a = TypecheckedUnisonFileId
  { dataDeclarationsId' :: Map v (TypeReferenceId, DataDeclaration v a),
    effectDeclarationsId' :: Map v (TypeReferenceId, EffectDeclaration v a),
    topLevelComponents' :: [[(v, Term v a, Type v a)]],
    watchComponents :: [(WatchKind, [(v, Term v a, Type v a)])],
    hashTermsId :: Map v (TermReferenceId, Maybe WatchKind, Term v a, Type v a)
  }
  deriving stock (Generic, Show)

{-# COMPLETE TypecheckedUnisonFile #-}

pattern TypecheckedUnisonFile ::
  Map v (TypeReference, DataDeclaration v a) ->
  Map v (TypeReference, EffectDeclaration v a) ->
  [[(v, Term v a, Type v a)]] ->
  [(WatchKind, [(v, Term v a, Type v a)])] ->
  Map
    v
    ( TermReference,
      Maybe WatchKind,
      ABT.Term (Term.F v a a) v a,
      ABT.Term Type.F v a
    ) ->
  TypecheckedUnisonFile v a
pattern TypecheckedUnisonFile ds es tlcs wcs hts <-
  TypecheckedUnisonFileId
    (fmap (first Reference.DerivedId) -> ds)
    (fmap (first Reference.DerivedId) -> es)
    tlcs
    wcs
    (fmap (over _1 Reference.DerivedId) -> hts)

instance Ord v => Functor (TypecheckedUnisonFile v) where
  fmap f (TypecheckedUnisonFileId ds es tlcs wcs hashTerms) =
    TypecheckedUnisonFileId ds' es' tlcs' wcs' hashTerms'
    where
      ds' = fmap (\(id, dd) -> (id, fmap f dd)) ds
      es' = fmap (\(id, ed) -> (id, fmap f ed)) es
      tlcs' = (fmap . fmap) (\(v, tm, tp) -> (v, Term.amap f tm, fmap f tp)) tlcs
      wcs' = map (\(wk, tms) -> (wk, map (\(v, tm, tp) -> (v, Term.amap f tm, fmap f tp)) tms)) wcs
      hashTerms' = fmap (\(id, wk, tm, tp) -> (id, wk, Term.amap f tm, fmap f tp)) hashTerms
