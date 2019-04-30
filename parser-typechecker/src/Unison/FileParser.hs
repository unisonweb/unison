{-# Language DoAndIfThenElse #-}
{-# Language DeriveFunctor #-}
{-# Language ScopedTypeVariables #-}

module Unison.FileParser where

import qualified Unison.ABT as ABT
import qualified Data.Set as Set
import           Control.Applicative
import           Control.Monad (guard, msum)
import           Control.Monad.Reader (local, ask)
import           Data.Functor
import           Data.Either (partitionEithers)
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (readFile)
import qualified Text.Megaparsec as P
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
import qualified Unison.Util.List as List
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.PrettyPrintEnv as PPE

file :: forall v . Var v => P v (PPE.PrettyPrintEnv, UnisonFile v Ann)
file = do
  _ <- openBlock
  names <- ask
  (dataDecls, effectDecls) <- declarations
  env <- case environmentFor names dataDecls effectDecls of
    Right env -> pure env
    Left es -> P.customFailure $ TypeDeclarationErrors es
  -- push names onto the stack ahead of existing names
  local (UF.names env `mappend`) $ do
    names <- ask
    -- The file may optionally contain top-level imports,
    -- which are parsed and applied to each stanza
    (names', substImports) <- TermParser.imports <* optional semi
    stanzas0 <- local (names' `mappend`) $ sepBy semi stanza
    let stanzas = fmap substImports <$> stanzas0
    _ <- closeBlock
    let (termsr, watchesr, _) = foldl' go ([], [], 0 :: Int) stanzas
        go (terms, watches, n) s = case s of
          WatchBinding kind _ ((_, v), at) ->
            (terms, (kind,(v,at)) : watches, n)
          WatchExpression kind _ at ->
            (terms, (kind, (Var.nameds ("_" <> show n), at)) : watches, n + 1)
          Binding ((_, v), at) -> ((v,at) : terms, watches, n)
          Bindings bs -> ([(v,at) | ((_,v), at) <- bs ] ++ terms, watches, n)
        (terms, watches) = (reverse termsr, List.multimap $ reverse watchesr)
        uf = UnisonFile (UF.datas env) (UF.effects env) terms watches
    pure (PPE.fromNames names, uf)

-- A stanza is either a watch expression like:
--   > 1 + x
--   > z = x + 1
-- Or it is a binding like:
--   foo : Nat -> Nat
--   foo x = x + 42
-- Or it is a namespace like:
--   namespace Woot where
--     x = 42
--     y = 17
-- which parses as [(Woot.x, 42), (Woot.y, 17)]

data Stanza v term
  = WatchBinding UF.WatchKind Ann ((Ann, v), term)
  | WatchExpression UF.WatchKind Ann term
  | Binding ((Ann, v), term)
  | Bindings [((Ann, v), term)] deriving Functor

stanza :: Var v => P v (Stanza v (AnnotatedTerm v Ann))
stanza = watchExpression <|> unexpectedAction <|> binding <|> namespace
  where
  unexpectedAction = failureIf (TermParser.blockTerm $> getErr) binding
  getErr = do
    t <- anyToken
    t2 <- optional anyToken
    P.customFailure $ DidntExpectExpression t t2
  watchExpression = do
    (kind, ann) <- watched
    _ <- closed
    msum [
      WatchBinding kind ann <$> TermParser.binding,
      WatchExpression kind ann <$> TermParser.blockTerm ]
  binding = Binding <$> TermParser.binding
  namespace = tweak <$> TermParser.namespaceBlock where
    tweak ns = Bindings (TermParser.toBindings [ns])

watched :: Var v => P v (UF.WatchKind, Ann)
watched = P.try $ do
  kind <- optional wordyId
  op <- optional (L.payload <$> P.lookAhead symbolyId)
  guard (op == Just ">")
  tok <- anyToken
  guard $ maybe True (`L.touches` tok) kind
  pure (maybe UF.RegularWatch L.payload kind, maybe mempty ann kind <> ann tok)

closed :: Var v => P v ()
closed = P.try $ do
  op <- optional (L.payload <$> P.lookAhead closeBlock)
  case op of Just () -> P.customFailure EmptyWatch
             _ -> pure ()

terminateTerm :: Var v => AnnotatedTerm v Ann -> AnnotatedTerm v Ann
terminateTerm e@(Term.LetRecNamedAnnotatedTop' top a bs body@(Term.Var' v))
  | Set.member v (ABT.freeVars e) = Term.letRec top a bs (DD.unitTerm (ABT.annotation body))
  | otherwise = e
terminateTerm e = e

declarations :: Var v => P v
                         (Map v (DataDeclaration' v Ann),
                          Map v (EffectDeclaration' v Ann))
declarations = do
  declarations <- many $
    ((Left <$> dataDeclaration) <|> Right <$> effectDeclaration) <* optional semi
  let (dataDecls, effectDecls) = partitionEithers declarations
      multimap :: Ord k => [(k,v)] -> Map k [v]
      multimap kvs = foldl' mi Map.empty kvs
      mi m (k,v) = Map.insertWith (++) k [v] m
      mds = multimap dataDecls
      mes = multimap effectDecls
      mdsBad = Map.filter (\xs -> length xs /= 1) mds
      mesBad = Map.filter (\xs -> length xs /= 1) mes
  if Map.null mdsBad && Map.null mesBad then
    pure (Map.fromList dataDecls, Map.fromList effectDecls)
  else
    P.customFailure . DuplicateTypeNames $
      [ (v, DD.annotation <$> ds) | (v, ds) <- Map.toList mdsBad ] <>
      [ (v, DD.annotation . DD.toDataDecl <$> es) | (v, es) <- Map.toList mesBad ]

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
        ctorAnn = ann ctorName <>
                  (if null ctorArgs then mempty else ann (last ctorArgs))
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
