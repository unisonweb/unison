{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.UnisonFile.Type where

import Unison.Prelude

import Control.Lens
import           Data.Bifunctor         (first)
import           Unison.DataDeclaration (DataDeclaration)
import           Unison.DataDeclaration (EffectDeclaration(..))
import qualified Unison.Reference       as Reference
import           Unison.Term            (Term)
import           Unison.Type            (Type)
import Unison.WatchKind (WatchKind)

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
    hashTermsId           :: Map v (Reference.Id, Term v a, Type v a)
  } deriving Show

{-# COMPLETE TypecheckedUnisonFile #-}
pattern TypecheckedUnisonFile ds es tlcs wcs hts <-
  TypecheckedUnisonFileId (fmap (first Reference.DerivedId) -> ds)
                          (fmap (first Reference.DerivedId) -> es)
                          tlcs
                          wcs
                          (fmap (over _1 Reference.DerivedId) -> hts)
