{-# Language OverloadedStrings, TupleSections, ScopedTypeVariables #-}

module Unison.FileParser2 where

import           Control.Applicative
import           Control.Monad.Reader (local)
import           Data.Either (partitionEithers)
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (readFile)
import qualified Unison.Lexer as L
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
import           Unison.Parser2
import qualified Unison.TermParser2 as TermParser
import qualified Unison.Type as Type
import           Unison.Type (AnnotatedType)
import qualified Unison.TypeParser2 as TypeParser
import           Unison.UnisonFile (UnisonFile(..), environmentFor)
import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)
import Unison.Reference (Reference)

file :: Var v => [(v, Reference)] -> [(v, Reference)] -> P v (UnisonFile v Ann)
file builtinTerms builtinTypes = do
  (dataDecls, effectDecls) <- declarations
  let env = environmentFor builtinTerms builtinTypes dataDecls effectDecls
  local (`Map.union` UF.constructorLookup env) $ do
    term <- TermParser.topBlock
    pure $ UnisonFile (UF.datas env) (UF.effects env) (UF.resolveTerm env term)

declarations :: Var v => P v
                         (Map v (DataDeclaration' v Ann),
                          Map v (EffectDeclaration' v Ann))
declarations = do
  declarations <- many ((Left <$> dataDeclaration) <|> Right <$> effectDeclaration)
  let (dataDecls, effectDecls) = partitionEithers declarations
  pure (Map.fromList dataDecls, Map.fromList effectDecls)

-- type Optional a = Some a | None
--                   a -> Optional a
--                   Optional a

dataDeclaration :: forall v . Var v => P v (v, DataDeclaration' v Ann)
dataDeclaration = do
  start <- reserved "type"
  (name, typeArgs) <- (,) <$> prefixVar <*> many prefixVar
  let typeArgVs = L.payload <$> typeArgs
  eq <- openBlockWith "="
  let
      -- go gives the type of the constructor, given the types of
      -- the constructor arguments, e.g. Cons becomes forall a . a -> List a -> List a
      go :: L.Token v -> [AnnotatedType v Ann] -> (Ann, v, AnnotatedType v Ann)
      go ctorName [] =
        (ann ctorName,
         L.payload ctorName,
         Type.foralls (ann ctorName) typeArgVs (tok Type.var name))
      go ctorName ctorArgs = let
        arrowAcc i o = Type.arrow (ann i <> ann o) i o
        appAcc f arg = Type.app (ann f <> ann arg) f arg
        -- ctorReturnType e.g. `Optional a`
        ctorReturnType = foldl' appAcc (tok Type.var name) (tok Type.var <$> typeArgs)
        -- ctorType e.g. `a -> Optional a`
        --    or just `Optional a` in the case of `None`
        ctorType = (foldr arrowAcc ctorReturnType ctorArgs)
        ctorAnn = (ann ctorName <> ann (last ctorArgs))
        in (ann ctorName, L.payload ctorName, Type.foralls ctorAnn typeArgVs ctorType)
      dataConstructor = go <$> prefixVar <*> many TypeParser.valueTypeLeaf
  constructors <- sepBy (reserved "|") dataConstructor
  _ <- closeBlock
  let -- the annotation of the last constructor if present,
      -- otherwise ann of name
      closingAnn :: Ann
      closingAnn = last (ann eq : ((\(_,_,t) -> ann t) <$> constructors))
  pure $ (L.payload name, DD.mkDataDecl' (ann start <> closingAnn) typeArgVs constructors)

effectDeclaration :: Var v => P v (v, EffectDeclaration' v Ann)
effectDeclaration = do
  effectStart <- reserved "effect"
  name <- prefixVar
  typeArgs <- many prefixVar
  let typeArgVs = L.payload <$> typeArgs
  blockStart <- openBlockWith "where"
  constructors <- sepBy semi constructor
  _ <- closeBlock
  let closingAnn = last $ ann blockStart : ((\(_,_,t) -> ann t) <$> constructors)
  pure $ (L.payload name, DD.mkEffectDecl' (ann effectStart <> closingAnn) typeArgVs constructors)
  where
    constructor :: Var v => P v (Ann, v, AnnotatedType v Ann)
    constructor = explodeToken <$>
      prefixVar <* openBlockWith ":" <*> TypeParser.computationType <* closeBlock
      where explodeToken v t = (ann v, L.payload v, t)
