{-# LANGUAGE RecordWildCards #-}

module Unison.UnisonFile.Type where

import Control.Lens
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.TypeAlias (TypeAlias)
import Unison.TypeAlias qualified as TypeAlias
import Unison.WatchKind (WatchKind)

data UnisonFile v a = UnisonFileId
  { -- Files can have an optional namespace prefix.
    fileNamespace :: Maybe (a, Name),
    dataDeclarationsId :: Map v (TypeReferenceId, DataDeclaration v a),
    effectDeclarationsId :: Map v (TypeReferenceId, EffectDeclaration v a),
    -- | Type aliases declared in the file, after normalisation. Aliases are
    -- not used for hashing terms\/decls (those are expanded against the
    -- alias bodies) — they're retained here only for persistence and
    -- display. See @docs/type-aliases.markdown@.
    typeAliasesId :: Map v (TypeReferenceId, TypeAlias v a),
    terms :: Map v (a {- ann for name of the binding -}, Term v a),
    watches :: Map WatchKind [(v, a {- ann for whole watch -}, Term v a)]
  }
  deriving stock (Generic, Show)

pattern UnisonFile ::
  Maybe (a, Name) ->
  Map v (TypeReference, DataDeclaration v a) ->
  Map v (TypeReference, EffectDeclaration v a) ->
  Map v (TypeReference, TypeAlias v a) ->
  Map v (a, Term v a) ->
  Map WatchKind [(v, a, Term v a)] ->
  UnisonFile v a
pattern UnisonFile fn ds es as tms ws <-
  UnisonFileId
    fn
    (fmap (first Reference.DerivedId) -> ds)
    (fmap (first Reference.DerivedId) -> es)
    (fmap (first Reference.DerivedId) -> as)
    tms
    ws

{-# COMPLETE UnisonFile #-}

-- | A UnisonFile after typechecking. Terms are split into groups by
--  cycle and the type of each term is known.
data TypecheckedUnisonFile v a = TypecheckedUnisonFileId
  { fileNamespace' :: Maybe (a, Name),
    dataDeclarationsId' :: Map v (TypeReferenceId, DataDeclaration v a),
    effectDeclarationsId' :: Map v (TypeReferenceId, EffectDeclaration v a),
    typeAliasesId' :: Map v (TypeReferenceId, TypeAlias v a),
    topLevelComponents' :: [[(v, a {- ann for whole binding -}, Term v a, Type v a)]],
    watchComponents :: [(WatchKind, [(v, a {- ann for whole watch -}, Term v a, Type v a)])],
    hashTermsId :: Map v (a {- ann for whole binding -}, TermReferenceId, Maybe WatchKind, Term v a, Type v a)
  }
  deriving stock (Generic, Show)

{-# COMPLETE TypecheckedUnisonFile #-}

pattern TypecheckedUnisonFile ::
  Maybe (a, Name) ->
  Map v (TypeReference, DataDeclaration v a) ->
  Map v (TypeReference, EffectDeclaration v a) ->
  Map v (TypeReference, TypeAlias v a) ->
  [[(v, a, Term v a, Type v a)]] ->
  [(WatchKind, [(v, a, Term v a, Type v a)])] ->
  Map
    v
    ( a,
      TermReference,
      Maybe WatchKind,
      ABT.Term (Term.F v a a) v a,
      ABT.Term Type.F v a
    ) ->
  TypecheckedUnisonFile v a
pattern TypecheckedUnisonFile fn ds es as tlcs wcs hts <-
  TypecheckedUnisonFileId
    fn
    (fmap (first Reference.DerivedId) -> ds)
    (fmap (first Reference.DerivedId) -> es)
    (fmap (first Reference.DerivedId) -> as)
    tlcs
    wcs
    (fmap (over _2 Reference.DerivedId) -> hts)

instance (Ord v) => Functor (TypecheckedUnisonFile v) where
  fmap f (TypecheckedUnisonFileId fn ds es as tlcs wcs hashTerms) =
    TypecheckedUnisonFileId fn' ds' es' as' tlcs' wcs' hashTerms'
    where
      fn' = (fmap . first) f fn
      ds' = ds <&> \(refId, decl) -> (refId, fmap f decl)
      es' = es <&> \(refId, effect) -> (refId, fmap f effect)
      as' = as <&> \(refId, alias) -> (refId, TypeAlias.amap f alias)
      tlcs' =
        tlcs
          & (fmap . fmap) \(v, a, tm, tp) -> (v, f a, Term.amap f tm, fmap f tp)
      wcs' = map (\(wk, tms) -> (wk, map (\(v, a, tm, tp) -> (v, f a, Term.amap f tm, fmap f tp)) tms)) wcs
      hashTerms' = fmap (\(a, id, wk, tm, tp) -> (f a, id, wk, Term.amap f tm, fmap f tp)) hashTerms
