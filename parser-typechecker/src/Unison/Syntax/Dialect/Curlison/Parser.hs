{-# LANGUAGE OverloadedStrings #-}

-- | The Curlison /parser/: text -> 'SFile'.
--
-- Reads the curly-brace dialect that 'Unison.Syntax.Dialect.Curlison' prints, producing the 'Surface' IR;
-- 'Unison.Syntax.Surface.Elaborate' does the rest. v1 grammar (matching the renderer):
--
-- > name : Type
-- > name = (x, y) => x + y
-- > f(a, b)              -- application
-- > a + b * c            -- infix operators, precedence-climbing (minimal parens)
-- > (c ? t : e)          -- if
-- > { x = e; body }      -- let block
-- > match (s) { p => body; p if g => body }
-- > structural type Color { Red; Green; Blue }     -- data declaration (semicolon-separated)
-- > unique record Point { x : Nat; y : Nat }       -- record declaration
module Unison.Syntax.Dialect.Curlison.Parser
  ( parseFile,
  )
where

import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read qualified as Read
import Unison.HashQualified qualified as HQ
import Unison.Lexer.Pos qualified as Pos
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.Syntax.Name qualified as Name (isSymboly, unsafeParseText)
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.Precedence (InfixPrecedence (Lowest), Precedence (Bottom, InfixOp), increment, operatorPrecedence)
import Unison.Syntax.Surface
import Unison.Syntax.Surface.Elaborate (elaborateFile, parseDocs, resolveDeclGuids)
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

type CP = P.Parsec Void String

data RawForm = RSig Name SType | RBind SBinding | RDecl SDecl | RWatch SWatch

parseFile :: forall m v. (Monad m, Var v) => FilePath -> String -> Parser.ParsingEnv m -> m (Either (Parser.Err v) (UnisonFile v Ann))
parseFile fp src env =
  case P.runParser pSFile fp src of
    Left bundle -> pure (Left (P.FancyError 0 (Set.singleton (P.ErrorFail (P.errorBundlePretty bundle)))))
    Right sfile0 -> do
      sfile <- resolveDeclGuids env sfile0
      edocs <- parseDocs env pTerm sfile
      pure (edocs >>= \docMap -> elaborateFile (Parser.names env) docMap sfile)

-- Lexing -------------------------------------------------------------------------------------------------------------

sc :: CP ()
sc = L.space C.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: CP a -> CP a
lexeme = L.lexeme sc

symbol :: String -> CP String
symbol = L.symbol sc

parens, brackets, braces, angles :: CP a -> CP a
parens p = symbol "(" *> p <* symbol ")"
brackets p = symbol "[" *> p <* symbol "]"
braces p = symbol "{" *> p <* symbol "}"
angles p = symbol "<" *> p <* symbol ">"

commaSep :: CP a -> CP [a]
commaSep p = P.sepBy p (symbol ",")

withAnn :: CP a -> CP (Ann, a)
withAnn p = do
  s <- P.getSourcePos
  x <- p
  e <- P.getSourcePos
  pure (Ann.Ann (toPos s) (toPos e), x)
  where
    toPos sp = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

isWordyChar :: Char -> Bool
isWordyChar c = isAlphaNum c || c == '_'

isSymChar :: Char -> Bool
isSymChar c = c `elem` ("+-*/<>=!&|^%~$:" :: String)

-- | A Unison name: dotted segments where the last may be symbolic, e.g. @x@, @List.map@, @Nat.+@, @+@.
nameRaw :: CP String
nameRaw = lexeme do
  first <- seg
  rest <- P.many (P.try (C.char '.' *> seg))
  pure (intercalate "." (first : rest))
  where
    seg = P.takeWhile1P (Just "wordy") isWordyChar P.<|> P.takeWhile1P (Just "operator") isSymChar

pname :: String -> Name
pname = Name.unsafeParseText . Text.pack

sname :: String -> SName
sname = HQ.NameOnly . pname

isOpName :: String -> Bool
isOpName s = not (null s) && Name.isSymboly (pname s)

-- Top level ----------------------------------------------------------------------------------------------------------

pSFile :: CP SFile
pSFile = do
  sc
  forms <- P.many (P.choice [P.try pWatch, P.try pDecl, P.try pCFunction, P.try pCValue, pStmt])
  P.eof
  let sigs = [(n, t) | RSig n t <- forms]
      -- a binding may already carry its type (C function/value forms inline it); only fall back to a standalone sig.
      binds = [b {bType = lookup (bName b) sigs <|> bType b} | RBind b <- forms]
      decls = [d | RDecl d <- forms]
      watches = [w | RWatch w <- forms]
  pure (SFile Nothing decls binds watches)

pWatch :: CP RawForm
pWatch = do
  (a, kind) <- withAnn (P.choice [WK.TestWatch <$ symbol "test>", WK.RegularWatch <$ symbol ">"])
  RWatch . SWatch a kind <$> pTerm

srcAnn :: P.SourcePos -> Ann
srcAnn sp = Ann.Ann p p where p = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

-- | An optional declaration modifier. @structural@ is explicit; its absence means @unique@ (the default), whose GUID
-- is recovered by name in 'parseFile' (the empty 'SUnique' sentinel is filled there).
pModifier :: CP SModifier
pModifier = P.option (SUnique "") (SStructural <$ symbol "structural")

data DeclKind = DKData | DKAbility | DKRecord deriving (Eq)

pDecl :: CP RawForm
pDecl = do
  start <- P.getSourcePos
  modi <- pModifier
  kind <- P.choice [DKData <$ symbol "type", DKAbility <$ symbol "ability", DKRecord <$ symbol "record"]
  nm <- nameRaw
  tvs <- P.option [] (angles (commaSep nameRaw))
  let a = srcAnn start
      tyvarNames = map pname tvs
      selfTy =
        SType a $
          if null tyvarNames
            then STyVar (pname nm)
            else STyApp (SType a (STyVar (pname nm))) [SType a (STyVar t) | t <- tyvarNames]
  case kind of
    DKRecord -> do
      fields <- braces (P.sepEndBy pField (symbol ";"))
      let fieldNames = map fst fields
          ctorType = foldr (\(_, t) acc -> SType a (STyArrow t Nothing acc)) selfTy fields
      pure (RDecl (SDecl a modi False (pname nm) tyvarNames [SConstructor a (pname nm) ctorType] (Just fieldNames)))
    _ -> do
      let isAb = kind == DKAbility
      ctors <- braces (P.sepEndBy (pCtor isAb selfTy) (symbol ";"))
      pure (RDecl (SDecl a modi isAb (pname nm) tyvarNames ctors Nothing))

-- | A record field: @fieldName : Type@.
pField :: CP (Name, SType)
pField = (\nm t -> (pname nm, t)) <$> nameRaw <* symbol ":" <*> pType

pCtor :: Bool -> SType -> CP SConstructor
pCtor isAb selfTy = do
  (ca, cn) <- withAnn nameRaw
  if isAb
    then symbol ":" *> (SConstructor ca (pname cn) <$> pType)
    else do
      margs <- P.optional (parens (commaSep pType))
      let args = fromMaybe [] margs
      pure (SConstructor ca (pname cn) (foldr (\arg acc -> SType ca (STyArrow arg Nothing acc)) selfTy args))

pStmt :: CP RawForm
pStmt = do
  (a, nm) <- withAnn nameRaw
  P.choice
    [ symbol ":" *> (RSig (pname nm) <$> pType),
      symbol "=" *> (RBind . SBinding a (pname nm) Nothing <$> pTerm) <* P.optional (symbol ";")
    ]

-- | A Curlison (curly-brace) function definition: @RetType name(ArgType a, …) { …; return e; }@. Reconstructs the binding with
-- its full @arg -> … -> ret@ type inlined and a lambda value, so it round-trips with what 'Unison.Syntax.Dialect.Curlison'
-- prints.
pCFunction :: CP RawForm
pCFunction = do
  tvs <- P.option [] (angles (commaSep nameRaw))
  retTy <- pType
  (a, nm) <- withAnn nameRaw
  ps <- parens (commaSep pCParam)
  effs <- P.optional (symbol "throws" *> braces (commaSep pType))
  body <- braces pFuncBody
  let arrows = case reverse ps of
        [] -> retTy
        ((lt, _) : restRev) ->
          let lastArrow = SType a (STyArrow lt effs retTy)
           in foldl (\acc (t, _) -> SType a (STyArrow t Nothing acc)) lastArrow restRev
      fullTy = if null tvs then arrows else SType a (STyForall (map pname tvs) arrows)
      lam = STerm a (SLam [SParam a p | (_, p) <- ps] body)
  pure (RBind (SBinding a (pname nm) (Just fullTy) lam))

-- | A typed parameter, @Type name@.
pCParam :: CP (SType, Name)
pCParam = do
  t <- pType
  nm <- nameRaw
  pure (t, pname nm)

-- | A function body: zero or more @name = e;@ statements followed by @return e [;]@.
pFuncBody :: CP STerm
pFuncBody = do
  stmts <- P.many (P.try pFuncStmt)
  _ <- symbol "return"
  e <- pTerm
  _ <- P.optional (symbol ";")
  pure (if null stmts then e else STerm (tAnn e) (SLet stmts e))

pFuncStmt :: CP SBinding
pFuncStmt = do
  (a, nm) <- withAnn nameRaw
  _ <- symbol "="
  v <- pTerm
  _ <- symbol ";"
  pure (SBinding a (pname nm) Nothing v)

-- | A C-style typed value definition: @Type name = e;@.
pCValue :: CP RawForm
pCValue = do
  ty <- pType
  (a, nm) <- withAnn nameRaw
  _ <- symbol "="
  v <- pTerm
  _ <- symbol ";"
  pure (RBind (SBinding a (pname nm) (Just ty) v))

-- Terms --------------------------------------------------------------------------------------------------------------

pTerm :: CP STerm
pTerm = P.choice [P.try pLambda, pInfix]

-- | An application, optionally followed by a chain of infix operators, resolved by precedence climbing so the result
-- matches what the renderer's minimal-paren printing assumed.
pInfix :: CP STerm
pInfix = do
  lhs <- pApp
  rest <- P.many ((,) <$> pOp <*> pApp)
  pure (resolveInfix lhs rest)

-- | A (possibly qualified) symbolic infix operator token, e.g. @+@ or @Nat.+@. Excludes the punctuation that has
-- dedicated grammar (@=@, @=>@, @->@, @:@, @|@, @<-@) so those keep their special meanings.
pOp :: CP String
pOp = P.try (nameRaw >>= check)
  where
    check s
      | s `elem` reserved = fail "reserved operator"
      -- @&&@ and @||@ are the boolean operators; they are not Unison names, so don't run them through 'isOpName'
      -- ('resolveInfix' turns them into 'SAnd'\/'SOr').
      | s `elem` ["&&", "||"] = pure s
      | isOpName s = pure s
      | otherwise = fail "operator"
    reserved = ["=", "=>", "->", ":", "|", "<-"]

-- | The precedence the renderer and parser agree to use for an operator, keyed on its last segment (so @Nat.+@ and @+@
-- agree); loosest infix level if it has no entry.
precFor :: String -> Precedence
precFor s = fromMaybe (InfixOp Lowest) (operatorPrecedence (Text.pack (lastSeg s)))
  where
    lastSeg = reverse . takeWhile (/= '.') . reverse

-- | Precedence-climbing resolution of a left operand plus a flat list of @(operator, operand)@ pairs. All operators
-- are left-associative; equal precedence associates left. @&&@ and @||@ become 'SAnd'\/'SOr'.
resolveInfix :: STerm -> [(String, STerm)] -> STerm
resolveInfix lhs0 toks0 = fst (parseExpr lhs0 toks0 Bottom)
  where
    parseExpr lhs toks minPrec = case toks of
      ((op, rhs) : rest)
        | precFor op >= minPrec ->
            let (rhs', rest') = climbRhs rhs rest (precFor op)
             in parseExpr (mkBin op lhs rhs') rest' minPrec
      _ -> (lhs, toks)
    climbRhs rhs toks opPrec = case toks of
      ((op2, _) : _)
        | precFor op2 > opPrec ->
            let (rhs', rest') = parseExpr rhs toks (increment opPrec)
             in climbRhs rhs' rest' opPrec
      _ -> (rhs, toks)
    mkBin op l r = STerm (tAnn l) (mkOpF op l r)
    mkOpF "&&" l r = SAnd l r
    mkOpF "||" l r = SOr l r
    mkOpF s l r = SBinOp (sname s) (precFor s) l r

pLambda :: CP STerm
pLambda = do
  (a, (ps, body)) <- withAnn do
    ps <- parens (commaSep (snd <$> withAnn nameRaw))
    _ <- symbol "=>"
    body <- pTerm
    pure (ps, body)
  pure (STerm a (SLam [SParam a (pname p) | p <- ps] body))

pApp :: CP STerm
pApp = do
  (a, h) <- withAnn pAtom
  calls <- P.many (parens (commaSep pTerm))
  pure (foldl (\f args -> STerm a (SApp f args)) h calls)

pAtom :: CP STerm
pAtom = P.choice [pDoc, pStringTerm, pCharTerm, pList, pBlock, pMatch, pParen, pNameAtom]

pDoc :: CP STerm
pDoc = do
  (a, txt) <- withAnn (lexeme (C.string "{{" *> scanDoc (1 :: Int) "{{"))
  pure (STerm a (SDocLit (Text.pack txt)))
  where
    scanDoc depth acc =
      P.choice
        [ P.try (C.string "}}") *> (let d = depth - 1 in if d == 0 then pure (acc <> "}}") else scanDoc d (acc <> "}}")),
          P.try (C.string "{{") *> scanDoc (depth + 1) (acc <> "{{"),
          P.anySingle >>= \c -> scanDoc depth (acc <> [c])
        ]

pNameAtom :: CP STerm
pNameAtom = do
  (a, nm) <- withAnn nameRaw
  pure $ STerm a case nm of
    "true" -> SLit (SBool True)
    "false" -> SLit (SBool False)
    _
      | Just n <- readNat nm -> SLit (SNat n)
      | Just i <- readInt nm -> SLit (SInt i)
      | Just f <- readFloat nm -> SLit (SFloat f)
      | otherwise -> SName (sname nm)

pStringTerm :: CP STerm
pStringTerm = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (STerm a (SLit (SText (Text.pack s))))

pCharTerm :: CP STerm
pCharTerm = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (STerm a (SLit (SChar c)))

pList :: CP STerm
pList = do
  (a, xs) <- withAnn (brackets (commaSep pTerm))
  pure (STerm a (SList xs))

pBlock :: CP STerm
pBlock = do
  (a, (bs, body)) <- withAnn (braces blockBody)
  pure (STerm a (SLet bs body))
  where
    blockBody = do
      bs <- P.many (P.try pBlockBinding)
      body <- pTerm
      _ <- P.optional (symbol ";")
      pure (bs, body)
    pBlockBinding = do
      (a, nm) <- withAnn nameRaw
      _ <- symbol "="
      v <- pTerm
      _ <- symbol ";"
      pure (SBinding a (pname nm) Nothing v)

pMatch :: CP STerm
pMatch = do
  (a, (s, cs)) <- withAnn do
    _ <- symbol "match"
    s <- parens pTerm
    cs <- braces (P.sepEndBy pCase (symbol ";"))
    pure (s, cs)
  pure (STerm a (SMatch s cs))

pCase :: CP SCase
pCase = do
  pat <- pPattern
  guard_ <- P.optional (symbol "if" *> pTerm)
  _ <- symbol "=>"
  SCase pat guard_ <$> pTerm

pParen :: CP STerm
pParen = do
  (a, f) <- withAnn (parens pParenBody)
  pure (STerm a f)
  where
    pParenBody = do
      e <- pTerm
      P.choice
        [ symbol "?" *> ((\t el -> SIf e t el) <$> pTerm <* symbol ":" <*> pTerm),
          symbol ":" *> (SAnn e <$> pType),
          symbol "," *> ((\rest -> STuple (e : rest)) <$> commaSep pTerm),
          pure (tOut e)
        ]

-- Patterns -----------------------------------------------------------------------------------------------------------

pPattern :: CP SPattern
pPattern = P.choice [pListPat, pParenPat, pEffectPat, P.try pAsPat, P.try pCtorPattern, pStringPat, pCharPat, pAtomPat]

-- | A list pattern: @[]@, @[a, b]@.
pListPat :: CP SPattern
pListPat = do
  (a, subs) <- withAnn (brackets (commaSep pPattern))
  pure (SPattern a (SPList subs))

-- | A parenthesized pattern, optionally an infix sequence op: @(p)@, @(l +: r)@, @(l :+ r)@, @(l ++ r)@.
pParenPat :: CP SPattern
pParenPat = parens pSeqPat

pSeqPat :: CP SPattern
pSeqPat = do
  l <- pPattern
  P.optional pSeqOp >>= \case
    Nothing -> pure l
    Just op -> do
      r <- pPattern
      pure (SPattern (patAnn l) (SPSeqOp l op r))

pSeqOp :: CP SSeqOp
pSeqOp = P.choice [SCons <$ P.try (symbol "+:"), SSnoc <$ P.try (symbol ":+"), SConcat <$ P.try (symbol "++")]

-- | An as-pattern: @name\@pat@.
pAsPat :: CP SPattern
pAsPat = do
  (a, nm) <- withAnn nameRaw
  _ <- symbol "@"
  SPattern a . SPAs (pname nm) <$> pPattern

-- | An ability pattern: @{ E.op(a, …) -> k }@ (a request) or @{ p }@ (a pure result).
pEffectPat :: CP SPattern
pEffectPat = braces (P.choice [P.try pRequest, pPure])
  where
    pRequest = do
      (a, nm) <- withAnn nameRaw
      subs <- parens (commaSep pPattern)
      _ <- symbol "->"
      SPattern a . SPEffect (sname nm) subs <$> pPattern
    pPure = do
      p <- pPattern
      pure (SPattern (patAnn p) (SPEffectPure p))

pStringPat :: CP SPattern
pStringPat = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (SPattern a (SPLit (SText (Text.pack s))))

pCharPat :: CP SPattern
pCharPat = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (SPattern a (SPLit (SChar c)))

pAtomPat :: CP SPattern
pAtomPat = do
  (a, nm) <- withAnn nameRaw
  pure $ SPattern a case nm of
    "_" -> SPWild
    "true" -> SPLit (SBool True)
    "false" -> SPLit (SBool False)
    _
      | Just n <- readNat nm -> SPLit (SNat n)
      | Just i <- readInt nm -> SPLit (SInt i)
      | Just f <- readFloat nm -> SPLit (SFloat f)
      | otherwise -> SPVar (pname nm)

pCtorPattern :: CP SPattern
pCtorPattern = do
  (a, (nm, subs)) <- withAnn ((,) <$> nameRaw <*> parens (commaSep pPattern))
  pure (SPattern a (SPCtor (sname nm) subs))

-- Types --------------------------------------------------------------------------------------------------------------

pType :: CP SType
pType = P.choice [pForallTy, pAppTy]

pForallTy :: CP SType
pForallTy = do
  (a, (vs, body)) <- withAnn do
    _ <- symbol "forall"
    vs <- angles (commaSep nameRaw)
    body <- pType
    pure (vs, body)
  pure (SType a (STyForall (map pname vs) body))

pAppTy :: CP SType
pAppTy = do
  (a, t) <- withAnn pTypeAtom
  margs <- P.optional (angles (commaSep pType))
  pure case margs of
    Nothing -> t
    Just args -> SType a (STyApp t args)

pTypeAtom :: CP SType
pTypeAtom = P.choice [pEffectsTy, pParenTy, pNameTy]

pNameTy :: CP SType
pNameTy = do
  (a, nm) <- withAnn nameRaw
  pure (SType a (STyVar (pname nm)))

pEffectsTy :: CP SType
pEffectsTy = do
  (a, es) <- withAnn (braces (commaSep pType))
  pure (SType a (STyEffects es))

pParenTy :: CP SType
pParenTy = parens pArrow

-- | A right-associative arrow chain, each arrow optionally carrying an ability set: @a ->{e} b -> c@.
pArrow :: CP SType
pArrow = do
  i <- pAppTy
  P.optional pArrowTail >>= \case
    Nothing -> pure i
    Just (es, o) -> pure (SType (tyAnn i) (STyArrow i es o))
  where
    pArrowTail = do
      _ <- symbol "->"
      es <- P.optional (braces (commaSep pType))
      o <- pArrow
      pure (es, o)

-- Literal classification ---------------------------------------------------------------------------------------------

readNat :: String -> Maybe Word64
readNat s = if not (null s) && all isDig s then Read.readMaybe s else Nothing

readInt :: String -> Maybe Int64
readInt = \case
  ('+' : r) | not (null r) && all isDig r -> Read.readMaybe r
  ('-' : r) | not (null r) && all isDig r -> negate <$> Read.readMaybe r
  _ -> Nothing

readFloat :: String -> Maybe Double
readFloat s = if elem '.' s && all (`elem` ("0123456789.+-eE" :: String)) s then Read.readMaybe s else Nothing

isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'
