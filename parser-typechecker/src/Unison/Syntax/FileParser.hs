module Unison.Syntax.FileParser where

import Control.Lens
import Control.Monad.Reader (asks, local)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Megaparsec qualified as P
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration)
import Unison.DataDeclaration qualified as DD
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Syntax.DeclParser (declarations)
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name (unsafeFromVar)
import Unison.Syntax.Parser
import Unison.Syntax.TermParser qualified as TermParser
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.UnisonFile (UnisonFile (..))
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Env qualified as UF
import Unison.UnisonFile.Names qualified as UFN
import Unison.Util.List qualified as List
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as UF
import Prelude hiding (readFile)

resolutionFailures :: (Ord v) => [Names.ResolutionFailure v Ann] -> P v m x
resolutionFailures es = P.customFailure (ResolutionFailures es)

file :: forall m v. (Monad m, Var v) => P v m (UnisonFile v Ann)
file = do
  _ <- openBlock
  -- The file may optionally contain top-level imports,
  -- which are parsed and applied to the type decls and term stanzas
  (namesStart, imports) <- TermParser.imports <* optional semi
  (dataDecls, effectDecls, parsedAccessors) <- declarations
  env <- case UFN.environmentFor (NamesWithHistory.currentNames namesStart) dataDecls effectDecls of
    Right (Right env) -> pure env
    Right (Left es) -> P.customFailure $ TypeDeclarationErrors es
    Left es -> resolutionFailures (toList es)
  let accessors :: [[(v, Ann, Term v Ann)]]
      accessors =
          [ DD.generateRecordAccessors (toPair <$> fields) (L.payload typ) r
            | (typ, fields) <- parsedAccessors,
              Just (r, _) <- [Map.lookup (L.payload typ) (UF.datas env)]
          ]
      toPair (tok, typ) = (L.payload tok, ann tok <> ann typ)
  let importNames = [(Name.unsafeFromVar v, Name.unsafeFromVar v2) | (v, v2) <- imports]
  let locals = Names.importing importNames (UF.names env)
  -- At this stage of the file parser, we've parsed all the type and ability
  -- declarations. The `push locals` here has the effect
  -- of making suffix-based name resolution prefer type and constructor names coming
  -- from the local file.
  --
  -- There's some more complicated logic below to have suffix-based name resolution
  -- make use of _terms_ from the local file.
  local (\e -> e {names = NamesWithHistory.push locals namesStart}) $ do
    names <- asks names
    stanzas0 <- sepBy semi stanza
    let stanzas = fmap (TermParser.substImports names imports) <$> stanzas0
    _ <- closeBlock
    let (termsr, watchesr) = foldl' go ([], []) stanzas
        go (terms, watches) s = case s of
          WatchBinding kind spanningAnn ((_, v), at) ->
            (terms, (kind, (v, spanningAnn, Term.generalizeTypeSignatures at)) : watches)
          WatchExpression kind guid spanningAnn at ->
            (terms, (kind, (Var.unnamedTest guid, spanningAnn, Term.generalizeTypeSignatures at)) : watches)
          Binding ((spanningAnn, v), at) -> ((v, spanningAnn, Term.generalizeTypeSignatures at) : terms, watches)
          Bindings bs -> ([(v, spanningAnn, Term.generalizeTypeSignatures at) | ((spanningAnn, v), at) <- bs] ++ terms, watches)
    let (terms, watches) = (reverse termsr, reverse watchesr)
        -- All locally declared term variables, running example:
        --   [foo.alice, bar.alice, zonk.bob]
        fqLocalTerms :: [v]
        fqLocalTerms = (stanzas0 >>= getVars) <> (view _1 <$> join accessors) 
    -- suffixified local term bindings shadow any same-named thing from the outer codebase scope
    -- example: `foo.bar` in local file scope will shadow `foo.bar` and `bar` in codebase scope
    let (curNames, resolveLocals) =
          ( Names.shadowTerms locals (NamesWithHistory.currentNames names),
            resolveLocals
          )
          where
            -- Each unique suffix mapped to its fully qualified name
            canonicalVars :: Map v v
            canonicalVars = UFN.variableCanonicalizer fqLocalTerms
            
            -- All unique local term name suffixes - these we want to
            -- avoid resolving to a term that's in the codebase
            locals :: [Name.Name]
            locals = (Name.unsafeFromVar <$> Map.keys canonicalVars)
            
            -- A function to replace unique local term suffixes with their
            -- fully qualified name
            replacements = [ (v, Term.var () v2) | (v,v2) <- Map.toList canonicalVars, v /= v2 ]
            resolveLocals = ABT.substsInheritAnnotation replacements
    let bindNames = Term.bindSomeNames Name.unsafeFromVar (Set.fromList fqLocalTerms) curNames . resolveLocals
    terms <- case List.validate (traverseOf _3 bindNames) terms of
      Left es -> resolutionFailures (toList es)
      Right terms -> pure terms
    watches <- case List.validate (traverseOf (traversed . _3) bindNames) watches of
      Left es -> resolutionFailures (toList es)
      Right ws -> pure ws
    let uf =
          UnisonFileId
            (UF.datasId env)
            (UF.effectsId env)
            (terms <> join accessors)
            (List.multimap watches)
    validateUnisonFile uf
    pure uf

-- | Final validations and sanity checks to perform before finishing parsing.
validateUnisonFile :: (Var v) => UnisonFile v Ann -> P v m ()
validateUnisonFile uf =
  checkForDuplicateTermsAndConstructors uf

-- | Because types and abilities can introduce their own constructors and fields it's difficult
-- to detect all duplicate terms during parsing itself. Here we collect all terms and
-- constructors and verify that no duplicates exist in the file, triggering an error if needed.
checkForDuplicateTermsAndConstructors ::
  forall m v.
  (Ord v) =>
  UnisonFile v Ann ->
  P v m ()
checkForDuplicateTermsAndConstructors uf = do
  when (not . null $ duplicates) $ do
    let dupeList :: [(v, [Ann])]
        dupeList =
          duplicates
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
        <&> (\(v, bindingAnn, _t) -> (v, bindingAnn))
    mergedTerms :: Map v (Set Ann)
    mergedTerms =
      (allConstructors <> allTerms)
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

data Stanza v term
  = WatchBinding UF.WatchKind Ann ((Ann, v), term)
  | WatchExpression UF.WatchKind Text Ann term
  | Binding ((Ann, v), term)
  | Bindings [((Ann, v), term)]
  deriving (Foldable, Traversable, Functor)

getVars :: (Var v) => Stanza v term -> [v]
getVars = \case
  WatchBinding _ _ ((_, v), _) -> [v]
  WatchExpression _ guid _ _ -> [Var.unnamedTest guid]
  Binding ((_, v), _) -> [v]
  Bindings bs -> [v | ((_, v), _) <- bs]

stanza :: (Monad m, Var v) => P v m (Stanza v (Term v Ann))
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
      msum
        [ WatchBinding kind ann <$> TermParser.binding,
          WatchExpression kind guid ann <$> TermParser.blockTerm
        ]
    guardEmptyWatch ann =
      P.try $ do
        op <- optional (L.payload <$> P.lookAhead closeBlock)
        case op of
          Just () -> P.customFailure (EmptyWatch ann)
          _ -> pure ()

    -- binding :: forall v. Var v => P v ((Ann, v), Term v Ann)
    binding = do
      -- this logic converts
      --   {{ A doc }}  to   foo.doc = {{ A doc }}
      --   foo = 42          foo = 42
      doc <- P.optional (TermParser.doc2Block <* semi)
      binding@((_, v), _) <- TermParser.binding
      pure $ case doc of
        Nothing -> Binding binding
        Just doc -> Bindings [((ann doc, Var.joinDot v (Var.named "doc")), doc), binding]

watched :: (Monad m, Var v) => P v m (UF.WatchKind, Text, Ann)
watched = P.try do
  kind <- optional wordyIdString
  guid <- uniqueName 10
  op <- optional (L.payload <$> P.lookAhead symbolyIdString)
  guard (op == Just ">")
  tok <- anyToken
  guard $ maybe True (`L.touches` tok) kind
  pure (maybe UF.RegularWatch L.payload kind, guid, maybe mempty ann kind <> ann tok)
