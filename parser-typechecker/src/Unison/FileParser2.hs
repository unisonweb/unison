{-# Language OverloadedStrings, TupleSections, ScopedTypeVariables #-}

module Unison.FileParser2 where

import           Control.Applicative
-- import           Control.Monad.Reader
-- import           Data.Either (partitionEithers)
import           Data.List (foldl')
-- import           Data.Map (Map)
-- import qualified Data.Map as Map
import           Prelude hiding (readFile)
-- import qualified Text.Parsec.Layout as L
import qualified Unison.Lexer as L
import           Unison.DataDeclaration (DataDeclaration')--, EffectDeclaration')
import qualified Unison.DataDeclaration as DD
-- import           Unison.Parser (Parser, traced, token_, sepBy, string)
import           Unison.Parser2
-- import qualified Unison.TermParser2 as TermParser
import qualified Unison.Type as Type
import           Unison.Type (AnnotatedType)
-- import           Unison.TypeParser (S)
import qualified Unison.TypeParser2 as TypeParser
-- import           Unison.UnisonFile (UnisonFile(..), environmentFor)
-- import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)
-- import Unison.Reference (Reference)
--
-- file :: Var v => [(v, Reference)] -> [(v, Reference)] -> Parser (S v) (UnisonFile v)
-- file builtinTerms builtinTypes = traced "file" $ do
--   (dataDecls, effectDecls) <- traced "declarations" declarations
--   let env = environmentFor builtinTerms builtinTypes dataDecls effectDecls
--   local (`Map.union` UF.constructorLookup env) $ do
--     term <- TermParser.block
--     pure $ UnisonFile (UF.datas env) (UF.effects env) (UF.resolveTerm env term)
--
-- declarations :: Var v => Parser (S v)
--                          (Map v (DataDeclaration v),
--                           Map v (EffectDeclaration v))
-- declarations = do
--   declarations <- many ((Left <$> dataDeclaration) <|> Right <$> effectDeclaration)
--   let (dataDecls, effectDecls) = partitionEithers declarations
--   pure (Map.fromList dataDecls, Map.fromList effectDecls)

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
  let -- the annotation of the last constructor if present,
      -- otherwise ann of name
      closingAnn :: Ann
      closingAnn = last (ann eq : ((\(_,_,t) -> ann t) <$> constructors))
  pure $ (L.payload name, DD.mkDataDecl' (ann start <> closingAnn) typeArgVs constructors)

-- effectDeclaration :: Var v => Parser (S v) (v, EffectDeclaration v)
-- effectDeclaration = traced "effect declaration" $ do
--   token_ $ string "effect"
--   name <- TermParser.prefixVar
--   typeArgs <- many TermParser.prefixVar
--   token_ $ string "where"
--   L.vblockNextToken $ do
--     constructors <- sepBy L.vsemi constructor
--     pure $ (name, DD.mkEffectDecl typeArgs constructors)
--   where
--     constructor = (,) <$> (TermParser.prefixVar <* token_ (string ":")) <*> traced "computation type" TypeParser.computationType
