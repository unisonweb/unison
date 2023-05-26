{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.CodeLookup.Util where

import Data.Map qualified as Map
import Unison.Codebase.CodeLookup
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Term qualified as Term
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile)
import Unison.Var (Var)

fromTypecheckedUnisonFile :: forall m v a. (Var v, Monad m) => TypecheckedUnisonFile v a -> CodeLookup v m a
fromTypecheckedUnisonFile tuf = CodeLookup tm ty
  where
    tm :: Reference.Id -> m (Maybe (Term.Term v a))
    tm id = pure $ Map.lookup id termMap
    ty :: Reference.Id -> m (Maybe (DataDeclaration.Decl v a))
    ty id = pure $ Map.lookup id dataDeclMap <|> Map.lookup id effectDeclMap
    dataDeclMap =
      Map.fromList
        [ (id, Right dd)
          | (_, (Reference.DerivedId id, dd)) <-
              Map.toList (UF.dataDeclarations' tuf)
        ]
    effectDeclMap =
      Map.fromList
        [ (id, Left ad)
          | (_, (Reference.DerivedId id, ad)) <-
              Map.toList (UF.effectDeclarations' tuf)
        ]
    termMap :: Map Reference.Id (Term.Term v a)
    termMap = Map.fromList [(id, tm) | (id, _wk, tm, _tp) <- toList $ UF.hashTermsId tuf]
