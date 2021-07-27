module Unison.Codebase.CodeLookup.Util where

import Unison.Prelude

import Control.Monad.Morph
import qualified Data.Map as Map
import Unison.Codebase.CodeLookup
import Unison.DataDeclaration (Decl)
import qualified Unison.Reference as Reference
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
import Unison.UnisonFile.Type (UnisonFile)
import Unison.Var (Var)
import qualified Unison.UnisonFile.Type as UF

fromUnisonFile :: (Var v, Monad m) => UnisonFile v a -> CodeLookup v m a
fromUnisonFile uf = CodeLookup tm ty where
  tm id = pure $ Map.lookup id termMap
  ty id = pure $ Map.lookup id typeMap1 <|> Map.lookup id typeMap2
  typeMap1 = Map.fromList [ (id, Right dd) |
                            (_, (Reference.DerivedId id, dd)) <-
                            Map.toList (UF.dataDeclarations uf) ]
  typeMap2 = Map.fromList [ (id, Left ad) |
                            (_, (Reference.DerivedId id, ad)) <-
                            Map.toList (UF.effectDeclarations uf) ]
  tmm = Map.fromList (UF.terms uf)
  termMap = Map.fromList [ (id, e) |
                            (_, (id, e)) <-
                            Map.toList (Term.hashComponents tmm) ]
