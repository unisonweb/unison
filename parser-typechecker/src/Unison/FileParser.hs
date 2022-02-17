{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# Language DeriveTraversable #-}
{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}

module Unison.FileParser where

import Unison.Prelude

import qualified Unison.ABT as ABT
import Control.Lens
import           Control.Monad.Reader (local, asks)
import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Prelude hiding (readFile)
import qualified Text.Megaparsec as P
import           Unison.DataDeclaration (DataDeclaration, EffectDeclaration)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Lexer as L
import           Unison.Parser
import Unison.Parser.Ann (Ann)
import           Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import Unison.UnisonFile (UnisonFile(..))
import qualified Unison.UnisonFile.Env as UF
import Unison.UnisonFile.Names (environmentFor)
import qualified Unison.Util.List as List
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.WatchKind as UF
import qualified Unison.NamesWithHistory as NamesWithHistory
import qualified Unison.Names as Names
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Name as Name
import qualified Unison.UnisonFile as UF

resolutionFailures :: Ord v => [Names.ResolutionFailure v Ann] -> P v x
resolutionFailures es = P.customFailure (ResolutionFailures es)

file :: forall v . Var v => P v (UnisonFile v Ann)
file = do
  _ <- openBlock
  -- The file may optionally contain top-level imports,
  -- which are parsed and applied to the type decls and term stanzas
  (namesStart, imports) <- TermParser.imports <* optional semi
  (dataDecls, effectDecls, parsedAccessors) <- declarations
  env <- case environmentFor (NamesWithHistory.currentNames namesStart) dataDecls effectDecls of
    Right (Right env) -> pure env
    Right (Left es) -> P.customFailure $ TypeDeclarationErrors es
    Left es -> resolutionFailures (toList es)
  let importNames = [(Name.unsafeFromVar v, Name.unsafeFromVar v2) | (v,v2) <- imports ]
  let locals = Names.importing importNames (UF.names env)
  -- At this stage of the file parser, we've parsed all the type and ability
  -- declarations. The `Names.push (Names.suffixify0 locals)` here has the effect
  -- of making suffix-based name resolution prefer type and constructor names coming
  -- from the local file.
  --
  -- There's some more complicated logic below to have suffix-based name resolution
  -- make use of _terms_ from the local file.
  local (\e -> e { names = NamesWithHistory.push locals namesStart }) $ do
    names <- asks names
    stanzas0 <- sepBy semi stanza
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
    -- suffixified local term bindings shadow any same-named thing from the outer codebase scope
    -- example: `foo.bar` in local file scope will shadow `foo.bar` and `bar` in codebase scope
    let (curNames, resolveLocals) =
          ( Names.shadowTerms locals (NamesWithHistory.currentNames names)
          , resolveLocals )
          where
          -- All locally declared term variables, running example:
          --   [foo.alice, bar.alice, zonk.bob]
          locals0 :: [v]
          locals0 = stanzas0 >>= getVars
          -- Groups variables by their suffixes:
          --   [ (foo.alice, [foo.alice]),
          --     (bar.alice, [bar.alice])
          --     (alice, [foo.alice, bar.alice]),
          --     (zonk.bob, [zonk.bob]),
          --     (bob, [zonk.bob]) ]
          varsBySuffix :: Map Name.Name [v]
          varsBySuffix = List.multimap [ (n, v) | v <- locals0, n <- Name.suffixes (Name.unsafeFromVar v) ]
          -- Any unique suffix maps to the corresponding variable. Above, `alice` is not a unique
          -- suffix, but `bob` is. `foo.alice` and `bob.alice` are both unique suffixes but
          -- they map to themselves, so we ignore them. In our example, we'll just be left with
          --   [(bob, Term.var() zonk.bob)]
          replacements = [ (Name.toVar n, Term.var() v')
                         | (n, nubOrd -> [v']) <- Map.toList varsBySuffix
                         , Name.toVar n /= v' ]
          locals = Map.keys varsBySuffix
          -- This will perform the actual variable replacements for suffixes
          -- that uniquely identify definitions in the file. It will avoid
          -- variable capture and respect local shadowing. For example, inside
          -- `bob -> bob * 42`, `bob` will correctly refer to the lambda parameter.
          -- and not the `zonk.bob` declared in the file.
          resolveLocals = ABT.substsInheritAnnotation replacements
    let bindNames = Term.bindSomeNames avoid curNames . resolveLocals
                    where avoid = Set.fromList (stanzas0 >>= getVars)
    terms <- case List.validate (traverse bindNames) terms of
      Left es -> resolutionFailures (toList es)
      Right terms -> pure terms
    watches <- case List.validate (traverse . traverse $ bindNames) watches of
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
    validateUnisonFile uf
    pure uf

-- | Final validations and sanity checks to perform before finishing parsing.
validateUnisonFile :: forall v . Var v => UnisonFile v Ann -> P v ()
validateUnisonFile uf =
  checkForDuplicateTermsAndConstructors uf

-- | Because types and abilities can introduce their own constructors and fields it's difficult
-- to detect all duplicate terms during parsing itself. Here we collect all terms and
-- constructors and verify that no duplicates exist in the file, triggering an error if needed.
checkForDuplicateTermsAndConstructors ::
  forall v.
  (Ord v) =>
  UnisonFile v Ann ->
  P v ()
checkForDuplicateTermsAndConstructors uf = do
  when (not . null $ duplicates) $ do
    let dupeList :: [(v, [Ann])]
        dupeList = duplicates
                 & fmap Set.toList
                 & Map.toList
    P.customFailure (DuplicateTermNames dupeList)
  where
    effectDecls :: [DataDeclaration v Ann]
    effectDecls = (Map.elems . fmap (DD.toDataDecl . snd) $ (effectDeclarationsId uf))
    dataDecls :: [DataDeclaration v Ann]
    dataDecls = fmap snd $ Map.elems (dataDeclarationsId uf)
    allConstructors :: [(v, Ann)]
    allConstructors =
        (dataDecls <> effectDecls)
        & foldMap DD.constructors'
        & fmap (\(ann, v, _typ) -> (v, ann))
    allTerms :: [(v, Ann)]
    allTerms =
      UF.terms uf
        <&> (\(v, t) -> (v, ABT.annotation t))
    mergedTerms :: Map v (Set Ann)
    mergedTerms = (allConstructors <> allTerms)
           & (fmap . fmap) Set.singleton
           & Map.fromListWith (<>)
    duplicates :: Map v (Set Ann)
    duplicates =
      -- Any vars with multiple annotations are duplicates.
      Map.filter ((> 1) . Set.size) mergedTerms

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
stanza = watchExpression <|> unexpectedAction <|> binding
  where
  unexpectedAction = failureIf (TermParser.blockTerm $> getErr) binding
  getErr = do
    t <- anyToken
    t2 <- optional anyToken
    P.customFailure $ DidntExpectExpression t t2
  watchExpression = do
    (kind, guid, ann) <- watched
    _ <- guardEmptyWatch ann
    msum [
      WatchBinding kind ann <$> TermParser.binding,
      WatchExpression kind guid ann <$> TermParser.blockTerm ]
  guardEmptyWatch ann =
    P.try $ do
      op <- optional (L.payload <$> P.lookAhead closeBlock)
      case op of Just () -> P.customFailure (EmptyWatch ann)
                 _ -> pure ()


  -- binding :: forall v. Var v => P v ((Ann, v), Term v Ann)
  binding = do
    -- this logic converts
    --   {{ A doc }}  to   foo.doc = {{ A doc }}
    --   foo = 42          foo = 42
    doc <- P.optional (TermParser.doc2Block <* semi)
    binding@((_,v), _) <- TermParser.binding
    pure $ case doc of
      Nothing -> Binding binding
      Just doc -> Bindings [((ann doc, Var.joinDot v (Var.named "doc")), doc), binding]

watched :: Var v => P v (UF.WatchKind, Text, Ann)
watched = P.try $ do
  kind <- optional wordyIdString
  guid <- uniqueName 10
  op <- optional (L.payload <$> P.lookAhead symbolyIdString)
  guard (op == Just ">")
  tok <- anyToken
  guard $ maybe True (`L.touches` tok) kind
  pure (maybe UF.RegularWatch L.payload kind, guid, maybe mempty ann kind <> ann tok)

-- The parsed form of record accessors, as in:
--
-- type Additive a = { zero : a, (+) : a -> a -> a }
--
-- The `Token v` is the variable name and location (here `zero` and `(+)`) of
-- each field, and the type is the type of that field
type Accessors v = [(L.Token v, [(L.Token v, Type v Ann)])]

declarations :: Var v => P v
                         (Map v (DataDeclaration v Ann),
                          Map v (EffectDeclaration v Ann),
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

-- unique[someguid] type Blah = ...
modifier :: Var v => P v (Maybe (L.Token DD.Modifier))
modifier = do
  optional (unique <|> structural)
  where
    unique = do
      tok <- openBlockWith "unique"
      uid <- do
        o <- optional (openBlockWith "[" *> wordyIdString <* closeBlock)
        case o of
          Nothing -> uniqueName 32
          Just uid -> pure (fromString . L.payload $ uid)
      pure (DD.Unique uid <$ tok)
    structural = do
      tok <- openBlockWith "structural"
      pure (DD.Structural <$ tok)

declaration :: Var v
            => P v (Either (v, DataDeclaration v Ann, Accessors v)
                           (v, EffectDeclaration v Ann))
declaration = do
  mod <- modifier
  fmap Right (effectDeclaration mod) <|> fmap Left (dataDeclaration mod)

dataDeclaration
  :: forall v
   . Var v
  => Maybe (L.Token DD.Modifier)
  -> P v (v, DataDeclaration v Ann, Accessors v)
dataDeclaration mod = do
  keywordTok       <- fmap void (reserved "type") <|> openBlockWith "type"
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
        ctorAnn = ann ctorName <> maybe (ann ctorName) ann (lastMay ctorArgs)
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
  case mod of
    Nothing -> P.customFailure $ MissingTypeModifier ("type" <$ keywordTok) name
    Just mod' ->
      pure (L.payload name,
            DD.mkDataDecl' (L.payload mod') (ann mod' <> closingAnn) typeArgVs constructors,
            accessors)

effectDeclaration
  :: Var v => Maybe (L.Token DD.Modifier) -> P v (v, EffectDeclaration v Ann)
effectDeclaration mod = do
  keywordTok  <- fmap void (reserved "ability") <|> openBlockWith "ability"
  name        <- TermParser.verifyRelativeVarName prefixDefinitionName
  typeArgs    <- many (TermParser.verifyRelativeVarName prefixDefinitionName)
  let typeArgVs = L.payload <$> typeArgs
  blockStart   <- openBlockWith "where"
  constructors <- sepBy semi (constructor typeArgs name)
               -- `ability` opens a block, as does `where`
  _            <- closeBlock <* closeBlock
  let closingAnn =
        last $ ann blockStart : ((\(_, _, t) -> ann t) <$> constructors)

  case mod of
    Nothing -> P.customFailure $ MissingTypeModifier ("ability" <$ keywordTok) name
    Just mod' ->
      pure
        ( L.payload name
        , DD.mkEffectDecl' (L.payload mod')
                          (ann mod' <> closingAnn)
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
