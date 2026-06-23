{-# LANGUAGE OverloadedStrings #-}

-- | The S-expression (Clojure-like) /parser/: text -> 'SFile'.
--
-- This is a pure front end: it produces the 'Surface' IR, and 'Unison.Syntax.Surface.Elaborate.elaborateFile' does the
-- name resolution and AST construction (shared with every other dialect). So this module knows nothing about
-- 'Reference's, the typechecker, or pattern-variable scoping — it just reads s-expressions.
--
-- v1 scope: @(def …)@ / @(defn …)@ bindings and @(: name type)@ signatures; the full term grammar including @match@;
-- and types. Declaring new types, watches, and docs are deferred (handled — i.e. rejected with an error — by
-- 'Unison.Syntax.Surface.Elaborate').
module Unison.Syntax.Dialect.SExpr.Parser
  ( parseFile,
  )
where

import Data.Char (isSpace)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read qualified as Read
import Unison.HashQualified qualified as HQ
import Unison.Lexer.Pos qualified as Pos
import Unison.Name (Name)
import Unison.Name qualified as Name (snoc)
import Unison.NameSegment qualified as NameSegment (docSegment)
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.Syntax.Name qualified as Name (unsafeParseText)
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.Surface
import Unison.Syntax.Surface.Elaborate (elaborateFile, parseDocs, resolveDeclGuids)
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

type SP = P.Parsec Void String

-- | A parsed top-level form before merging signatures into bindings.
data RawForm = RSig Name SType | RBind SBinding | RDecl SDecl | RWatch SWatch

parseFile ::
  forall m v.
  (Monad m, Var v) =>
  FilePath ->
  String ->
  Parser.ParsingEnv m ->
  m (Either (Parser.Err v) (UnisonFile v Ann))
parseFile fp src env =
  case P.runParser pSFile fp src of
    Left bundle -> pure (Left (errFail (P.errorBundlePretty bundle)))
    Right sfile0 -> do
      sfile <- resolveDeclGuids env sfile0
      edocs <- parseDocs env pTerm sfile
      pure (edocs >>= \docMap -> elaborateFile (Parser.names env) docMap sfile)

errFail :: String -> Parser.Err v
errFail msg = P.FancyError 0 (Set.singleton (P.ErrorFail msg))

-- Lexing -------------------------------------------------------------------------------------------------------------

sc :: SP ()
sc = L.space C.space1 (L.skipLineComment ";") P.empty

lexeme :: SP a -> SP a
lexeme = L.lexeme sc

symbol :: String -> SP String
symbol = L.symbol sc

isDelim :: Char -> Bool
isDelim c = c `elem` ("()[]{}\";" :: String) || isSpace c

atom :: SP (Ann, String)
atom = lexeme (withAnn (P.takeWhile1P (Just "atom") (not . isDelim)))

keyword :: String -> SP ()
keyword s = void (P.try (lexeme (C.string s <* P.notFollowedBy (P.satisfy (not . isDelim)))))

parens :: SP a -> SP a
parens p = symbol "(" *> p <* symbol ")"

brackets :: SP a -> SP a
brackets p = symbol "[" *> p <* symbol "]"

braces :: SP a -> SP a
braces p = symbol "{" *> p <* symbol "}"

withAnn :: SP a -> SP (Ann, a)
withAnn p = do
  start <- P.getSourcePos
  x <- p
  end <- P.getSourcePos
  pure (spanAnn start end, x)

spanAnn :: P.SourcePos -> P.SourcePos -> Ann
spanAnn s e = Ann.Ann (toPos s) (toPos e)
  where
    toPos sp = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

sname :: String -> SName
sname = HQ.NameOnly . Name.unsafeParseText . Text.pack

pname :: String -> Name
pname = Name.unsafeParseText . Text.pack

-- Top level ----------------------------------------------------------------------------------------------------------

pSFile :: SP SFile
pSFile = do
  sc
  forms <- concat <$> P.many pTopForm
  P.eof
  let sigs = [(n, t) | RSig n t <- forms]
      lookupSig n = lookup n sigs
      binds = [b {bType = lookupSig (bName b)} | RBind b <- forms]
      decls = [d | RDecl d <- forms]
      watches = [w | RWatch w <- forms]
  pure (SFile Nothing decls binds watches)

-- | A top-level form, optionally preceded by a @{{ }}@ doc block. A leading doc becomes a separate @<name>.doc@
-- binding (the same desugaring Unison's own file parser does), so a documented definition round-trips.
pTopForm :: SP [RawForm]
pTopForm = do
  mdoc <- P.optional (withAnn pDocRaw)
  form <- pForm
  pure case mdoc of
    Just (a, txt) | Just nm <- formName form -> [docBinding a nm txt, form]
    _ -> [form]

-- | The name a form defines, if any (for attaching a preceding doc).
formName :: RawForm -> Maybe Name
formName = \case
  RBind b -> Just (bName b)
  RSig n _ -> Just n
  RDecl d -> Just (dName d)
  RWatch _ -> Nothing

-- | A @<name>.doc = {{ … }}@ binding synthesized from a doc block preceding a definition.
docBinding :: Ann -> Name -> String -> RawForm
docBinding a nm txt = RBind (SBinding a (Name.snoc nm NameSegment.docSegment) Nothing (STerm a (SDocLit (Text.pack txt))))

pForm :: SP RawForm
pForm = P.choice [pWatch, parens (P.choice [pSig, pDefn, pDef, pRecord, pDecl])]

pWatch :: SP RawForm
pWatch = do
  (a, kind) <- withAnn (P.choice [WK.TestWatch <$ symbol "test>", WK.RegularWatch <$ symbol ">"])
  RWatch . SWatch a kind <$> pTerm

pDecl :: SP RawForm
pDecl = do
  (a, isAb) <- withAnn ((False <$ keyword "type") P.<|> (True <$ keyword "ability"))
  modi <- pModifier
  (_, nm) <- atom
  tvs <- parens (P.many atom)
  let tyvarNames = [pname n | (_, n) <- tvs]
      selfTy =
        SType a $
          if null tyvarNames
            then STyVar (pname nm)
            else STyApp (SType a (STyVar (pname nm))) [SType a (STyVar t) | t <- tyvarNames]
  ctors <- P.many (pCtor isAb selfTy)
  pure (RDecl (SDecl a modi isAb (pname nm) tyvarNames ctors Nothing))

-- | A record declaration: @(record <modifier> Name (params) (field Type) …)@. The single constructor is named after
-- the type; the field names drive accessor regeneration in 'Unison.Syntax.Surface.Elaborate'.
pRecord :: SP RawForm
pRecord = do
  (a, _) <- withAnn (keyword "record")
  modi <- pModifier
  (_, nm) <- atom
  tvs <- parens (P.many atom)
  fields <- P.many (parens ((,) <$> (snd <$> atom) <*> pType))
  let tyvarNames = [pname n | (_, n) <- tvs]
      selfTy =
        SType a $
          if null tyvarNames
            then STyVar (pname nm)
            else STyApp (SType a (STyVar (pname nm))) [SType a (STyVar t) | t <- tyvarNames]
      fieldNames = [pname fn | (fn, _) <- fields]
      ctorType = foldr (\(_, t) acc -> SType a (STyArrow t Nothing acc)) selfTy fields
  pure (RDecl (SDecl a modi False (pname nm) tyvarNames [SConstructor a (pname nm) ctorType] (Just fieldNames)))

-- | An optional declaration modifier. @structural@ is explicit; its absence means @unique@ (the default), whose GUID
-- is recovered by name in 'parseFile' (the empty 'SUnique' sentinel is filled there).
pModifier :: SP SModifier
pModifier = P.option (SUnique "") (SStructural <$ keyword "structural")

pCtor :: Bool -> SType -> SP SConstructor
pCtor isAb selfTy = parens do
  (ca, cn) <- atom
  if isAb
    then SConstructor ca (pname cn) <$> pType
    else do
      args <- P.many pType
      pure (SConstructor ca (pname cn) (foldr (\arg acc -> SType ca (STyArrow arg Nothing acc)) selfTy args))

pSig :: SP RawForm
pSig = do
  keyword ":"
  (_, name) <- atom
  RSig (pname name) <$> pType

pDef :: SP RawForm
pDef = do
  (a, _) <- withAnn (keyword "def")
  (_, name) <- atom
  body <- pTerm
  pure (RBind (SBinding a (pname name) Nothing body))

pDefn :: SP RawForm
pDefn = do
  (a, _) <- withAnn (keyword "defn")
  (_, name) <- atom
  args <- parens (P.many atom)
  body <- pTerm
  let params = [SParam pa (pname n) | (pa, n) <- args]
  pure (RBind (SBinding a (pname name) Nothing (STerm a (SLam params body))))

-- Terms --------------------------------------------------------------------------------------------------------------

pTerm :: SP STerm
pTerm = P.choice [pDoc, pListTerm, pParenTerm, pAtomTerm]

-- | Capture a @{{ … }}@ doc literal verbatim (balanced), to be re-parsed by the real doc parser in Elaborate.
pDoc :: SP STerm
pDoc = do
  (a, txt) <- withAnn pDocRaw
  pure (STerm a (SDocLit (Text.pack txt)))

-- | The raw text of a @{{ … }}@ doc block (delimiters included, nested @{{ }}@ balanced), as 'SDocLit' stores it.
pDocRaw :: SP String
pDocRaw = lexeme (C.string "{{" *> scanDoc (1 :: Int) "{{")
  where
    scanDoc depth acc =
      P.choice
        [ P.try (C.string "}}") *> (let d = depth - 1 in if d == 0 then pure (acc <> "}}") else scanDoc d (acc <> "}}")),
          P.try (C.string "{{") *> scanDoc (depth + 1) (acc <> "{{"),
          P.anySingle >>= \c -> scanDoc depth (acc <> [c])
        ]

pAtomTerm :: SP STerm
pAtomTerm = P.choice [pStringTerm, pCharTerm, mkAtom <$> atom]
  where
    mkAtom (a, s) = STerm a (classifyAtom s)

pStringTerm :: SP STerm
pStringTerm = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (STerm a (SLit (SText (Text.pack s))))

pCharTerm :: SP STerm
pCharTerm = do
  (a, c) <- withAnn (lexeme (C.char '\\' *> L.charLiteral))
  pure (STerm a (SLit (SChar c)))

classifyAtom :: String -> STermF
classifyAtom s = case s of
  "true" -> SLit (SBool True)
  "false" -> SLit (SBool False)
  _
    | Just n <- readNat s -> SLit (SNat n)
    | Just i <- readInt s -> SLit (SInt i)
    | Just f <- readFloat s -> SLit (SFloat f)
    | otherwise -> SName (sname s)

pListTerm :: SP STerm
pListTerm = do
  (a, xs) <- withAnn (brackets (P.many pTerm))
  pure (STerm a (SList xs))

pParenTerm :: SP STerm
pParenTerm = do
  (a, mk) <- withAnn (parens form)
  pure (STerm a (mk a))
  where
    form = P.choice [pIf, pAnd, pOr, pFn, pLet, pLetrec, pAnnotate, pDelay, pHandle, pTuple, pMatch, pApp]

-- Each special form returns @Ann -> STermF@.
pIf, pAnd, pOr, pFn, pLet, pLetrec, pAnnotate, pDelay, pHandle, pTuple, pMatch, pApp :: SP (Ann -> STermF)
pIf = keyword "if" >> (\c t e _ -> SIf c t e) <$> pTerm <*> pTerm <*> pTerm
pAnd = keyword "and" >> (\x y _ -> SAnd x y) <$> pTerm <*> pTerm
pOr = keyword "or" >> (\x y _ -> SOr x y) <$> pTerm <*> pTerm
pDelay = keyword "delay" >> (\x _ -> SDelay x) <$> pTerm
pHandle = keyword "handle" >> (\h e _ -> SHandle h e) <$> pTerm <*> pTerm
pAnnotate = keyword "ann" >> (\e t _ -> SAnn e t) <$> pTerm <*> pType
pTuple = keyword "tuple" >> (\xs _ -> STuple xs) <$> P.many pTerm
pFn = do
  keyword "fn"
  args <- parens (P.many atom)
  body <- pTerm
  pure \_ -> SLam [SParam pa (pname n) | (pa, n) <- args] body
pLet = do
  keyword "let"
  bs <- parens (P.many pBinding)
  body <- pTerm
  pure \_ -> SLet bs body
pLetrec = do
  keyword "letrec"
  bs <- parens (P.many pBinding)
  body <- pTerm
  pure \_ -> SLetRec bs body
pApp = do
  f <- pTerm
  args <- P.many pTerm
  pure \_ -> case args of [] -> tOut f; _ -> SApp f args
pMatch = do
  keyword "match"
  scrutinee <- pTerm
  cases <- P.many pCase
  pure \_ -> SMatch scrutinee cases

pBinding :: SP SBinding
pBinding = parens do
  (a, name) <- atom
  SBinding a (pname name) Nothing <$> pTerm

pCase :: SP SCase
pCase = parens do
  keyword "case"
  pat <- pPattern
  guard_ <- P.optional (P.try (parens (keyword "when" >> pTerm)))
  SCase pat guard_ <$> pTerm

-- Patterns -----------------------------------------------------------------------------------------------------------

pPattern :: SP SPattern
pPattern = P.choice [pListPattern, pCtorPattern, pStringPattern, pCharPattern, pAtomPattern]

-- | A list pattern: @[]@, @[a b]@.
pListPattern :: SP SPattern
pListPattern = do
  (a, subs) <- withAnn (brackets (P.many pPattern))
  pure (SPattern a (SPList subs))

pStringPattern :: SP SPattern
pStringPattern = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (SPattern a (SPLit (SText (Text.pack s))))

pCharPattern :: SP SPattern
pCharPattern = do
  (a, c) <- withAnn (lexeme (C.char '\\' *> L.charLiteral))
  pure (SPattern a (SPLit (SChar c)))

pAtomPattern :: SP SPattern
pAtomPattern = do
  (a, s) <- atom
  pure $ SPattern a case s of
    "_" -> SPWild
    "true" -> SPLit (SBool True)
    "false" -> SPLit (SBool False)
    _
      | Just n <- readNat s -> SPLit (SNat n)
      | Just i <- readInt s -> SPLit (SInt i)
      | Just f <- readFloat s -> SPLit (SFloat f)
      | otherwise -> SPVar (pname s)

-- | A parenthesized pattern, dispatched on its head: the seq operators @cons@\/@snoc@\/@concat@, an as-pattern @as@, an
-- ability @request@\/@pure@, a @tuple@, or otherwise a constructor application. (Inverse of 'SExpr.renderSPattern'.)
pCtorPattern :: SP SPattern
pCtorPattern = do
  (a, (name, subs)) <- withAnn (parens ((,) <$> atom <*> P.many pPattern))
  let mk = case (snd name, subs) of
        ("tuple", _) -> SPTuple subs
        ("cons", [l, r]) -> SPSeqOp l SCons r
        ("snoc", [l, r]) -> SPSeqOp l SSnoc r
        ("concat", [l, r]) -> SPSeqOp l SConcat r
        ("as", [SPattern _ (SPVar n), sub]) -> SPAs n sub
        ("pure", [sub]) -> SPEffectPure sub
        ("request", SPattern _ (SPVar n) : rest@(_ : _)) -> SPEffect (HQ.NameOnly n) (init rest) (last rest)
        _ -> SPCtor (sname (snd name)) subs
  pure (SPattern a mk)

-- Types --------------------------------------------------------------------------------------------------------------

pType :: SP SType
pType = P.choice [pEffectsType, pParenType, pAtomType]

pAtomType :: SP SType
pAtomType = do
  (a, s) <- atom
  pure (SType a (STyVar (pname s)))

pEffectsType :: SP SType
pEffectsType = do
  (a, es) <- withAnn (braces (P.many pType))
  pure (SType a (STyEffects es))

pParenType :: SP SType
pParenType = do
  (a, mk) <- withAnn (parens (P.choice [pArrow, pForall, pTupleType, P.try pEffectfulType, pTypeApp]))
  pure (SType a (mk a))

-- | An ability-annotated (non-arrow) type @({e} t)@, e.g. an ability request type @({Abort} a)@.
pEffectfulType :: SP (Ann -> STypeF)
pEffectfulType = do
  es <- braces (P.many pType)
  t <- pType
  pure \_ -> STyEffectful es t

pTupleType :: SP (Ann -> STypeF)
pTupleType = do
  keyword "tuple"
  ts <- P.many pType
  pure \_ -> STyTuple ts

pArrow, pForall, pTypeApp :: SP (Ann -> STypeF)
pArrow = do
  keyword "->"
  ts <- P.some pType
  pure \_ -> arrows ts
  where
    -- A bare ability row @{e}@ (parsed as 'STyEffects') annotates the arrow leading to the type that follows it, i.e.
    -- @(-> a {e} b)@ is @a ->{e} b@ — it is the arrow's effect, not an argument of kind Ability.
    arrows = \case
      [t] -> tyOut t
      (i : SType _ (STyEffects es) : rest@(_ : _)) -> STyArrow i (Just es) (SType (tyAnn i) (arrows rest))
      (i : rest@(_ : _)) -> STyArrow i Nothing (SType (tyAnn i) (arrows rest))
      [] -> STyVar (pname "_")
pForall = do
  keyword "forall"
  vs <- parens (P.many atom)
  t <- pType
  pure \_ -> STyForall [pname n | (_, n) <- vs] t
pTypeApp = do
  f <- pType
  args <- P.many pType
  pure \_ -> case args of [] -> tyOut f; _ -> STyApp f args

-- Literal classification ---------------------------------------------------------------------------------------------

readNat :: String -> Maybe Word64
readNat s
  | not (null s) && all isDig s = Read.readMaybe s
  | otherwise = Nothing

readInt :: String -> Maybe Int64
readInt = \case
  ('+' : rest) | not (null rest) && all isDig rest -> Read.readMaybe rest
  ('-' : rest) | not (null rest) && all isDig rest -> negate <$> Read.readMaybe rest
  _ -> Nothing

readFloat :: String -> Maybe Double
readFloat s
  | elem '.' s && all (`elem` ("0123456789.+-eE" :: String)) s = Read.readMaybe s
  | otherwise = Nothing

isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'
