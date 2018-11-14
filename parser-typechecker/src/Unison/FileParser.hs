{-# Language ScopedTypeVariables, TupleSections #-}

module Unison.FileParser where

import qualified Unison.ABT as ABT
import qualified Data.Set as Set
import           Control.Applicative
import           Control.Monad (void)
import           Control.Monad.Reader (local, ask)
import           Data.Either (partitionEithers)
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (readFile)
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
import qualified Unison.Lexer as L
import           Unison.Parser
import           Unison.Term (AnnotatedTerm)
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import           Unison.UnisonFile (UnisonFile(..), environmentFor)
import qualified Unison.UnisonFile as UF
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.PrettyPrintEnv as PPE
-- import Debug.Trace

file :: forall v . Var v => P v (PPE.PrettyPrintEnv, UnisonFile v Ann)
file = do
  _ <- openBlock
  names <- ask
  (dataDecls, effectDecls) <- declarations
  let env = environmentFor names dataDecls effectDecls
  -- push names onto the stack ahead of existing names
  local (UF.names env `mappend`) $ do
    names <- ask
    term <- terminateTerm <$> TermParser.topLevelBlock "top-level block"
              (void <$> peekAny) -- we actually opened before the declarations
              closeBlock
    let uf = UnisonFile (UF.datas env) (UF.effects env) term
    pure (PPE.fromNames names, uf)

terminateTerm :: Var v => AnnotatedTerm v Ann -> AnnotatedTerm v Ann
terminateTerm e@(Term.LetRecNamedAnnotatedTop' top a bs body@(Term.Var' v))
  | Set.member v (ABT.freeVars e) = Term.letRec top a bs (Term.unit (ABT.annotation body))
  | otherwise = e
terminateTerm e = e

declarations :: Var v => P v
                         (Map v (DataDeclaration' v Ann),
                          Map v (EffectDeclaration' v Ann))
declarations = do
  declarations <- many $
    ((Left <$> dataDeclaration) <|> Right <$> effectDeclaration) <* semi
  let (dataDecls, effectDecls) = partitionEithers declarations
  pure (Map.fromList dataDecls, Map.fromList effectDecls)

-- type Optional a = Some a | None
--                   a -> Optional a
--                   Optional a

dataDeclaration :: forall v . Var v => P v (v, DataDeclaration' v Ann)
dataDeclaration = do
  start <- openBlockWith "type"
  (name, typeArgs) <- (,) <$> prefixVar <*> many prefixVar
  let typeArgVs = L.payload <$> typeArgs
  eq <- reserved "="
  let
      -- go gives the type of the constructor, given the types of
      -- the constructor arguments, e.g. Cons becomes forall a . a -> List a -> List a
      go :: L.Token v -> [AnnotatedType v Ann] -> (Ann, v, AnnotatedType v Ann)
      go ctorName ctorArgs = let
        arrow i o = Type.arrow (ann i <> ann o) i o
        app f arg = Type.app (ann f <> ann arg) f arg
        -- ctorReturnType e.g. `Optional a`
        ctorReturnType = foldl' app (tok Type.var name) (tok Type.var <$> typeArgs)
        -- ctorType e.g. `a -> Optional a`
        --    or just `Optional a` in the case of `None`
        ctorType = foldr arrow ctorReturnType ctorArgs
        ctorAnn = ann ctorName <> ann (last ctorArgs)
        in (ann ctorName, Var.namespaced [L.payload name, L.payload ctorName],
            Type.foralls ctorAnn typeArgVs ctorType)
      dataConstructor = go <$> prefixVar <*> many TypeParser.valueTypeLeaf
  constructors <- sepBy (reserved "|") dataConstructor
  _ <- closeBlock
  let -- the annotation of the last constructor if present,
      -- otherwise ann of name
      closingAnn :: Ann
      closingAnn = last (ann eq : ((\(_,_,t) -> ann t) <$> constructors))
  pure (L.payload name, DD.mkDataDecl' (ann start <> closingAnn) typeArgVs constructors)

effectDeclaration :: Var v => P v (v, EffectDeclaration' v Ann)
effectDeclaration = do
  effectStart <- reserved "effect" <|> reserved "ability"
  name <- prefixVar
  typeArgs <- many prefixVar
  let typeArgVs = L.payload <$> typeArgs
  blockStart <- openBlockWith "where"
  constructors <- sepBy semi (constructor name)
  _ <- closeBlock
  let closingAnn = last $ ann blockStart : ((\(_,_,t) -> ann t) <$> constructors)
  pure (L.payload name, DD.mkEffectDecl' (ann effectStart <> closingAnn) typeArgVs constructors)
  where
    constructor :: Var v => L.Token v -> P v (Ann, v, AnnotatedType v Ann)
    constructor name = explodeToken <$>
      prefixVar <* reserved ":" <*> (Type.generalizeLowercase <$> TypeParser.computationType)
      where explodeToken v t = (ann v, Var.namespaced [L.payload name, L.payload v], t)
