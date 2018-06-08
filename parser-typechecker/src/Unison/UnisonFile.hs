{-# Language OverloadedStrings #-}

module Unison.UnisonFile where

import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Unison.Reference (Reference)
import Unison.DataDeclaration (DataDeclaration(..), EffectDeclaration(..))
import Unison.DataDeclaration (hashDecls, toDataDecl)
import qualified Data.Text as Text
import Unison.Term (Term)
import Unison.Var (Var)
import qualified Unison.Var as Var

data UnisonFile v = UnisonFile {
  dataDeclarations :: Map v (Reference, DataDeclaration v),
  effectDeclarations :: Map v (Reference, EffectDeclaration v),
  term :: Term v
} deriving (Show)

type CtorLookup = Map String (Reference, Int)

environmentFor :: Var v
               => Map v (DataDeclaration v)
               -> Map v (EffectDeclaration v)
               -> (Map v (Reference, DataDeclaration v),
                   Map v (Reference, EffectDeclaration v),
                   CtorLookup)
environmentFor dataDecls effectDecls =
  let hashDecls' = hashDecls (Map.union dataDecls (toDataDecl <$> effectDecls))
      allDecls = Map.fromList [ (v, (r,de)) | (v,r,de) <- hashDecls' ]
      dataDecls' = Map.difference allDecls effectDecls
      effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
  in (dataDecls', effectDecls', Map.fromList (constructors' =<< hashDecls'))

constructors' :: Var v => (v, Reference, DataDeclaration v) -> [(String, (Reference, Int))]
constructors' (typeSymbol, r, (DataDeclaration _ constructors)) =
  let qualCtorName ((ctor,_), i) =
       (Text.unpack $ mconcat [Var.qualifiedName typeSymbol, ".", Var.qualifiedName ctor], (r, i))
  in qualCtorName <$> constructors `zip` [0..]
