{-# Language DoAndIfThenElse #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language ScopedTypeVariables #-}
{-# Language TupleSections #-}
{-# Language OverloadedStrings #-}

module Unison.FileParser where

import Unison.Prelude

import qualified Unison.ABT as ABT
import Control.Lens
import           Control.Monad.Reader (local, asks)
import qualified Data.Map as Map
import           Prelude hiding (readFile)
import qualified Text.Megaparsec as P
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
import qualified Unison.Lexer as L
import           Unison.Parser
import           Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import           Unison.UnisonFile (UnisonFile(..), environmentFor)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.List as List
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Names3 as Names
import qualified Unison.Name as Name

resolutionFailures :: Ord v => [Names.ResolutionFailure v Ann] -> P v x
resolutionFailures es = P.customFailure (ResolutionFailures es)

file :: forall v . Var v => P v (UnisonFile v Ann)
file = do
  _ <- openBlock
  -- The file may optionally contain top-level imports,
  -- which are parsed and applied to the type decls and term stanzas
  (namesStart, imports) <- TermParser.imports <* optional semi
  (dataDecls, effectDecls, parsedAccessors) <- declarations
  env <- case environmentFor (Names.currentNames namesStart) dataDecls effectDecls of
    Right (Right env) -> pure env
    Right (Left es) -> P.customFailure $ TypeDeclarationErrors es
    Left es -> resolutionFailures (toList es)
  let importNames = [(Name.fromVar v, Name.fromVar v2) | (v,v2) <- imports ]
  let locals = Names.importing0 importNames (UF.names env)
  local (\e -> e { names = Names.push locals namesStart }) $ do
    names <- asks names
    stanzas0 <- local (\e -> e { names = names }) $ sepBy semi stanza
    let stanzas = fmap (TermParser.substImports names imports) <$> stanzas0
    _ <- closeBlock
    let (termsr, watchesr) = foldl' go ([], []) stanzas
        go (terms, watches) s = case s of
          WatchBinding kind _ ((_, v), at) ->
            (terms, (kind,(v,Term.generalizeTypeSignatures at)) : watches)
          WatchExpression kind guid _ at ->
            (terms, (kind, (Var.unnamedTest guid, Term.generalizeTypeSignatures at)) : watches)
          Binding ((_, v), at) -> ((v,Term.generalizeTypeSignatures at) : terms, watches)
          Bindings bs -> ([(v,Term.generalizeTypeSignatures at) | ((_,v), at) <- bs ] ++ terms, watches)
    let (terms, watches) = (reverse termsr, reverse watchesr)
    -- local term bindings shadow any same-named thing from the outer codebase scope
    let locals = stanzas0 >>= getVars
    let curNames = Names.deleteTerms0 (Name.fromVar <$> locals) (Names.currentNames names)
    terms <- case List.validate (traverse $ Term.bindSomeNames curNames) terms of
      Left es -> resolutionFailures (toList es)
      Right terms -> pure terms
    watches <- case List.validate (traverse . traverse $ Term.bindSomeNames curNames) watches of
      Left es -> resolutionFailures (toList es)
      Right ws -> pure ws
    let toPair (tok, _) = (L.payload tok, ann tok)
        accessors =
          [ DD.generateRecordAccessors (toPair <$> fields) (L.payload typ) r
          | (typ, fields) <- parsedAccessors
          , Just (r,_) <- [Map.lookup (L.payload typ) (UF.datas env)]
          ]
        uf = UnisonFileId (UF.datasId env) (UF.effectsId env) (terms <> join accessors)
                        (List.multimap watches)
    pure uf

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
  | WatchExpression UF.WatchKind Text Ann term
  | Binding ((Ann, v), term)
  | Bindings [((Ann, v), term)] deriving (Foldable, Traversable, Functor)

getVars :: Var v => Stanza v term -> [v]
getVars = \case
  WatchBinding _ _ ((_,v), _) -> [v]
  WatchExpression _ guid _ _ -> [Var.unnamedTest guid]
  Binding ((_,v), _) -> [v]
  Bindings bs -> [ v | ((_,v), _) <- bs ]

stanza :: Var v => P v (Stanza v (Term v Ann))
stanza = watchExpression <|> unexpectedAction <|> binding <|> namespace
  where
  unexpectedAction = failureIf (TermParser.blockTerm $> getErr) binding
  getErr = do
    t <- anyToken
    t2 <- optional anyToken
    P.customFailure $ DidntExpectExpression t t2
  watchExpression = do
    (kind, guid, ann) <- watched
    _ <- closed
    msum [
      WatchBinding kind ann <$> TermParser.binding,
      WatchExpression kind guid ann <$> TermParser.blockTerm ]
  binding = Binding <$> TermParser.binding
  namespace = tweak <$> TermParser.namespaceBlock where
    tweak ns = Bindings (TermParser.toBindings [ns])

watched :: Var v => P v (UF.WatchKind, Text, Ann)
watched = P.try $ do
  kind <- optional wordyIdString
  guid <- uniqueName 10
  op <- optional (L.payload <$> P.lookAhead symbolyIdString)
  guard (op == Just ">")
  tok <- anyToken
  guard $ maybe True (`L.touches` tok) kind
  pure (maybe UF.RegularWatch L.payload kind, guid, maybe mempty ann kind <> ann tok)

closed :: Var v => P v ()
closed = P.try $ do
  op <- optional (L.payload <$> P.lookAhead closeBlock)
  case op of Just () -> P.customFailure EmptyWatch
             _ -> pure ()

-- The parsed form of record accessors, as in:
--
-- type Additive a = { zero : a, (+) : a -> a -> a }
--
-- The `Token v` is the variable name and location (here `zero` and `(+)`) of
-- each field, and the type is the type of that field
type Accessors v = [(L.Token v, [(L.Token v, Type v Ann)])]

declarations :: Var v => P v
                         (Map v (DataDeclaration' v Ann),
                          Map v (EffectDeclaration' v Ann),
                          Accessors v)
declarations = do
  declarations <- many $ declaration <* optional semi
  let (dataDecls0, effectDecls) = partitionEithers declarations
      dataDecls = [(a,b) | (a,b,_) <- dataDecls0 ]
      multimap :: Ord k => [(k,v)] -> Map k [v]
      multimap = foldl' mi Map.empty
      mi m (k,v) = Map.insertWith (++) k [v] m
      mds = multimap dataDecls
      mes = multimap effectDecls
      mdsBad = Map.filter (\xs -> length xs /= 1) mds
      mesBad = Map.filter (\xs -> length xs /= 1) mes
  if Map.null mdsBad && Map.null mesBad then
    pure (Map.fromList dataDecls,
          Map.fromList effectDecls,
          join . map (view _3) $ dataDecls0)
  else
    P.customFailure . DuplicateTypeNames $
      [ (v, DD.annotation <$> ds) | (v, ds) <- Map.toList mdsBad ] <>
      [ (v, DD.annotation . DD.toDataDecl <$> es) | (v, es) <- Map.toList mesBad ]

modifier :: Var v => P v (L.Token DD.Modifier)
modifier = do
  o <- optional (openBlockWith "unique")
  case o of
    Nothing -> fmap (const DD.Structural) <$> P.lookAhead anyToken
    Just tok -> do
      uid <- do
        o <- optional (reserved "[" *> wordyIdString <* reserved "]")
        case o of
          Nothing -> uniqueName 32
          Just uid -> pure (fromString . L.payload $ uid)
      pure (DD.Unique uid <$ tok)

declaration :: Var v
            => P v (Either (v, DataDeclaration' v Ann, Accessors v)
                           (v, EffectDeclaration' v Ann))
declaration = do
  mod <- modifier
  fmap Right (effectDeclaration mod) <|> fmap Left (dataDeclaration mod)

dataDeclaration
  :: forall v
   . Var v
  => L.Token DD.Modifier
  -> P v (v, DataDeclaration' v Ann, Accessors v)
dataDeclaration mod = do
  _                <- fmap void (reserved "type") <|> openBlockWith "type"
  (name, typeArgs) <-
    (,) <$> TermParser.verifyRelativeVarName prefixDefinitionName
        <*> many (TermParser.verifyRelativeVarName prefixDefinitionName)
  let typeArgVs = L.payload <$> typeArgs
  eq <- reserved "="
  let
      -- go gives the type of the constructor, given the types of
      -- the constructor arguments, e.g. Cons becomes forall a . a -> List a -> List a
      go :: L.Token v -> [Type v Ann] -> (Ann, v, Type v Ann)
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
      prefixVar = TermParser.verifyRelativeVarName prefixDefinitionName
      dataConstructor = go <$> prefixVar <*> many TypeParser.valueTypeLeaf
      record = do
        _ <- openBlockWith "{"
        fields <- sepBy1 (reserved "," <* optional semi) $
          liftA2 (,) (prefixVar <* reserved ":") TypeParser.valueType
        _ <- closeBlock
        pure ([go name (snd <$> fields)], [(name, fields)])
  (constructors, accessors) <-
    msum [record, (,[]) <$> sepBy (reserved "|") dataConstructor]
  _ <- closeBlock
  let -- the annotation of the last constructor if present,
      -- otherwise ann of name
      closingAnn :: Ann
      closingAnn = last (ann eq : ((\(_,_,t) -> ann t) <$> constructors))
  pure (L.payload name,
        DD.mkDataDecl' (L.payload mod) (ann mod <> closingAnn) typeArgVs constructors,
        accessors)

effectDeclaration
  :: Var v => L.Token DD.Modifier -> P v (v, EffectDeclaration' v Ann)
effectDeclaration mod = do
  _        <- fmap void (reserved "ability") <|> openBlockWith "ability"
  name     <- TermParser.verifyRelativeVarName prefixDefinitionName
  typeArgs <- many (TermParser.verifyRelativeVarName prefixDefinitionName)
  let typeArgVs = L.payload <$> typeArgs
  blockStart   <- openBlockWith "where"
  constructors <- sepBy semi (constructor typeArgs name)
               -- `ability` opens a block, as does `where`
  _            <- closeBlock <* closeBlock
  let closingAnn =
        last $ ann blockStart : ((\(_, _, t) -> ann t) <$> constructors)
  pure
    ( L.payload name
    , DD.mkEffectDecl' (L.payload mod)
                       (ann mod <> closingAnn)
                       typeArgVs
                       constructors
    )
 where
  constructor
    :: Var v => [L.Token v] -> L.Token v -> P v (Ann, v, Type v Ann)
  constructor typeArgs name =
    explodeToken
      <$> TermParser.verifyRelativeVarName prefixDefinitionName
      <*  reserved ":"
      <*> (   Type.generalizeLowercase mempty
          .   ensureEffect
          <$> TypeParser.computationType
          )
   where
    explodeToken v t = (ann v, Var.namespaced [L.payload name, L.payload v], t)
    -- If the effect is not syntactically present in the constructor types,
    -- add them after parsing.
    ensureEffect t = case t of
      Type.Effect' _ _ -> modEffect t
      x -> Type.editFunctionResult modEffect x
    modEffect t = case t of
      Type.Effect' es t -> go es t
      t                 -> go [] t
    toTypeVar t = Type.av' (ann t) (Var.name $ L.payload t)
    headIs t v = case t of
      Type.Apps' (Type.Var' x) _ -> x == v
      Type.Var' x                -> x == v
      _                          -> False
    go es t =
      let es' = if any (`headIs` L.payload name) es
            then es
            else Type.apps' (toTypeVar name) (toTypeVar <$> typeArgs) : es
      in  Type.cleanupAbilityLists $ Type.effect (ABT.annotation t) es' t
