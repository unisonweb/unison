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
import Unison.Hash (Hash)
import qualified Unison.LabeledDependency as LD

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

-- Produce a mapping which includes all component hashes and the variables contained
-- within them.
-- This includes all kinds of definitions: types, terms, abilities, constructors
componentMap :: 
  TypecheckedUnisonFile v ann 
  -- Left is a variable for a type
  -- Right is a variable for a term or constructor
  -> Map Hash (Set (Either v v))
componentMap _uf = undefined
  -- TODO: watch components?

-- Produce a mapping which includes all variables their reference.
referencesMap :: TypecheckedUnisonFile v ann -> Map v LD.LabeledDependency
referencesMap _uf = undefined

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
