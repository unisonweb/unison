{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.UnisonFile.Type where

import Unison.Prelude

import Control.Lens
import Data.Bifunctor (first)
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import qualified Unison.Reference as Reference
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import Unison.WatchKind (WatchKind)
import qualified Unison.Util.Relation3 as Rel3

data UnisonFile v a = UnisonFileId {
  dataDeclarationsId   :: Map v (Reference.Id, DataDeclaration v a),
  effectDeclarationsId :: Map v (Reference.Id, EffectDeclaration v a),
  terms :: [(v, Term v a)],
  watches :: Map WatchKind [(v, Term v a)]
} deriving Show

pattern UnisonFile ds es tms ws <-
  UnisonFileId (fmap (first Reference.DerivedId) -> ds)
               (fmap (first Reference.DerivedId) -> es)
               tms
               ws
{-# COMPLETE UnisonFile #-}

-- |A UnisonFile after typechecking. Terms are split into groups by
-- cycle and the type of each term is known.
data TypecheckedUnisonFile v a =
  TypecheckedUnisonFileId {
    dataDeclarationsId'   :: Map v (Reference.Id, DataDeclaration v a),
    effectDeclarationsId' :: Map v (Reference.Id, EffectDeclaration v a),
    topLevelComponents' :: [[(v, Term v a, Type v a)]],
    watchComponents     :: [(WatchKind, [(v, Term v a, Type v a)])],
    hashTermsId           :: Map v (Reference.Id, Maybe WatchKind, Term v a, Type v a)
  } deriving Show

{-# COMPLETE TypecheckedUnisonFile #-}
pattern TypecheckedUnisonFile ds es tlcs wcs hts <-
  TypecheckedUnisonFileId (fmap (first Reference.DerivedId) -> ds)
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
      tlcs' = (fmap.fmap) (\(v, tm, tp) -> (v, Term.amap f tm, fmap f tp)) tlcs
      wcs' = map (\(wk, tms) -> (wk, map (\(v, tm, tp) -> (v, Term.amap f tm, fmap f tp)) tms)) wcs
      hashTerms' = fmap (\(id, wk, tm, tp) -> (id, wk, Term.amap f tm, fmap f tp)) hashTerms
