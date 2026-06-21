{-# LANGUAGE OverloadedStrings #-}

-- | The Pyson-like /parser/: text -> 'SFile'. Indentation-significant (via megaparsec's 'L.indentBlock').
--
-- v1 grammar (matching 'Unison.Syntax.Dialect.Pyson'):
--
-- > name : Type
-- > x = value
-- > def name(a, b):
-- >   body
-- > match s:
-- >   case p: body
-- > let:
-- >   x = e
-- >   body
-- > lambda x, y: body        (a + b)   (t if c else e)   f(a, b)   [a, b]
module Unison.Syntax.Dialect.Pyson.Parser
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

type PP = P.Parsec Void String

data RawForm = RSig Name SType | RBind SBinding | RDecl SDecl | RWatch SWatch

parseFile :: forall m v. (Monad m, Var v) => FilePath -> String -> Parser.ParsingEnv m -> m (Either (Parser.Err v) (UnisonFile v Ann))
parseFile fp src env =
  case P.runParser pSFile fp src of
    Left bundle -> pure (Left (P.FancyError 0 (Set.singleton (P.ErrorFail (P.errorBundlePretty bundle)))))
    Right sfile0 -> do
      sfile <- resolveDeclGuids env sfile0
      edocs <- parseDocs env sfile
      pure (edocs >>= \docMap -> elaborateFile (Parser.names env) docMap sfile)

-- Lexing: two space consumers — `sc` stays on the line, `scn` crosses newlines (for indentation). ------------------

lineComment :: PP ()
lineComment = L.skipLineComment "#"

scn :: PP ()
scn = L.space C.space1 lineComment P.empty

sc :: PP ()
sc = L.space (void (P.takeWhile1P (Just "space") (\c -> c == ' ' || c == '\t'))) lineComment P.empty

lexeme :: PP a -> PP a
lexeme = L.lexeme sc

symbol :: String -> PP String
symbol = L.symbol sc

parens, brackets, braces :: PP a -> PP a
parens p = symbol "(" *> p <* symbol ")"
brackets p = symbol "[" *> p <* symbol "]"
braces p = symbol "{" *> p <* symbol "}"

commaSep :: PP a -> PP [a]
commaSep p = P.sepBy p (symbol ",")

withAnn :: PP a -> PP (Ann, a)
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

nameRaw :: PP String
nameRaw = lexeme do
  first <- seg
  rest <- P.many (P.try (C.char '.' *> seg))
  pure (intercalate "." (first : rest))
  where
    seg = P.takeWhile1P (Just "wordy") isWordyChar P.<|> P.takeWhile1P (Just "operator") (\c -> isSymChar c && c /= ':')

pname :: String -> Name
pname = Name.unsafeParseText . Text.pack

sname :: String -> SName
sname = HQ.NameOnly . pname

isOpName :: String -> Bool
isOpName s = not (null s) && Name.isSymboly (pname s)

reserved :: [String]
reserved = ["if", "else", "and", "or", "lambda", "match", "case", "let", "letrec", "def"]

-- Top level ----------------------------------------------------------------------------------------------------------

pSFile :: PP SFile
pSFile = do
  scn
  forms <- P.many (L.nonIndented scn (P.choice [P.try pWatch, P.try pDecl, pStmt]) <* scn)
  P.eof
  let sigs = [(n, t) | RSig n t <- forms]
      binds = [b {bType = lookup (bName b) sigs} | RBind b <- forms]
      decls = [d | RDecl d <- forms]
      watches = [w | RWatch w <- forms]
  pure (SFile Nothing decls binds watches)

pWatch :: PP RawForm
pWatch = do
  (a, kind) <- withAnn (P.choice [WK.TestWatch <$ symbol "test>", WK.RegularWatch <$ symbol ">"])
  RWatch . SWatch a kind <$> pInlineExpr

srcAnn :: P.SourcePos -> Ann
srcAnn sp = Ann.Ann p p where p = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

-- | An optional declaration modifier. @structural@ is explicit; its absence means @unique@ (the default), whose GUID
-- is recovered by name in 'parseFile' (the empty 'SUnique' sentinel is filled there).
pModifier :: PP SModifier
pModifier = P.option (SUnique "") (SStructural <$ symbol "structural")

data DeclKind = DKData | DKAbility | DKRecord deriving (Eq)

-- | An indented declaration item: a constructor (for @type@\/@ability@) or a @field : Type@ (for @record@).
data DeclItem = ItemCtor SConstructor | ItemField Name SType

pDecl :: PP RawForm
pDecl = L.indentBlock scn do
  start <- P.getSourcePos
  modi <- pModifier
  kind <- P.choice [DKData <$ symbol "type", DKAbility <$ symbol "ability", DKRecord <$ symbol "record"]
  nm <- nameRaw
  tvs <- P.option [] (brackets (commaSep nameRaw))
  _ <- symbol ":"
  let a = srcAnn start
      tyvarNames = map pname tvs
      selfTy =
        SType a $
          if null tyvarNames
            then STyVar (pname nm)
            else STyApp (SType a (STyVar (pname nm))) [SType a (STyVar t) | t <- tyvarNames]
      mk items = RDecl case kind of
        DKRecord ->
          let fields = [(n, t) | ItemField n t <- items]
              ctorType = foldr (\(_, t) acc -> SType a (STyArrow t Nothing acc)) selfTy fields
           in SDecl a modi False (pname nm) tyvarNames [SConstructor a (pname nm) ctorType] (Just (map fst fields))
        _ -> SDecl a modi (kind == DKAbility) (pname nm) tyvarNames [c | ItemCtor c <- items] Nothing
  pure (L.IndentSome Nothing (pure . mk) (pDeclItem kind selfTy))

-- | Parse one indented declaration item, dispatched on the declaration kind.
pDeclItem :: DeclKind -> SType -> PP DeclItem
pDeclItem kind selfTy = case kind of
  DKRecord -> (\(_, cn) t -> ItemField (pname cn) t) <$> withAnn nameRaw <* symbol ":" <*> pType
  DKAbility -> ItemCtor <$> pCtor True selfTy
  DKData -> ItemCtor <$> pCtor False selfTy

pCtor :: Bool -> SType -> PP SConstructor
pCtor isAb selfTy = do
  (ca, cn) <- withAnn nameRaw
  if isAb
    then symbol ":" *> (SConstructor ca (pname cn) <$> pType)
    else do
      margs <- P.optional (parens (commaSep pType))
      let args = fromMaybe [] margs
      pure (SConstructor ca (pname cn) (foldr (\arg acc -> SType ca (STyArrow arg Nothing acc)) selfTy args))

pStmt :: PP RawForm
pStmt = P.choice [pDefStmt, pAssignOrSig]

pDefStmt :: PP RawForm
pDefStmt = L.indentBlock scn do
  (a, _) <- withAnn (P.try (symbol "def"))
  nm <- nameRaw
  ps <- parens (commaSep nameRaw)
  _ <- symbol ":"
  pure (L.IndentSome Nothing (\items -> pure (RBind (SBinding a (pname nm) Nothing (STerm a (SLam [SParam a (pname p) | p <- ps] (lastItem a items)))))) pTerm)

lastItem :: Ann -> [STerm] -> STerm
lastItem a = \case
  [] -> STerm a SHole
  xs -> last xs

pAssignOrSig :: PP RawForm
pAssignOrSig = do
  (a, nm) <- withAnn nameRaw
  P.choice
    [ symbol ":" *> (RSig (pname nm) <$> pType),
      symbol "=" *> (RBind . SBinding a (pname nm) Nothing <$> pTerm)
    ]

-- Terms --------------------------------------------------------------------------------------------------------------

pTerm :: PP STerm
pTerm = P.choice [pLet, pLetrec, pMatch, P.try pLambda, pInfix]

-- | An application optionally followed by a chain of symbolic infix operators, resolved by precedence climbing so the
-- result matches the renderer's minimal-paren printing. Keyword operators (@and@\/@or@\/conditional) are not handled
-- here — they live in 'pParen'.
pInfix :: PP STerm
pInfix = do
  lhs <- pApp
  rest <- P.many ((,) <$> pOp <*> pApp)
  pure (resolveInfix lhs rest)

-- | A (possibly qualified) symbolic infix operator token, e.g. @+@ or @Nat.+@. Excludes punctuation with dedicated
-- grammar (@=@, @=>@, @->@, @:@, @|@, @<-@).
pOp :: PP String
pOp = P.try (nameRaw >>= check)
  where
    check s
      | s `elem` reserved = fail "reserved operator"
      | isOpName s = pure s
      | otherwise = fail "operator"
    reserved = ["=", "=>", "->", ":", "|", "<-"]

-- | The precedence the renderer and parser agree to use for an operator, keyed on its last segment (so @Nat.+@ and @+@
-- agree); loosest infix level if it has no entry.
precFor :: String -> Precedence
precFor s = fromMaybe (InfixOp Lowest) (operatorPrecedence (Text.pack (lastSeg s)))
  where
    lastSeg = reverse . takeWhile (/= '.') . reverse

-- | Precedence-climbing resolution of a left operand plus a flat list of @(operator, operand)@ pairs (all
-- left-associative).
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
    mkBin op l r = STerm (tAnn l) (SBinOp (sname op) (precFor op) l r)

pLet :: PP STerm
pLet = pLetLike "let" SLet

pLetrec :: PP STerm
pLetrec = pLetLike "letrec" SLetRec

pLetLike :: String -> ([SBinding] -> STerm -> STermF) -> PP STerm
pLetLike kw mk = L.indentBlock scn do
  (a, _) <- withAnn (P.try (symbol kw <* symbol ":"))
  pure (L.IndentSome Nothing (\items -> pure (assemble a items)) pBlockItem)
  where
    assemble a items =
      let bs = [b | Left b <- items]
          body = lastBody a items
       in STerm a (mk bs body)
    lastBody a items = case [t | Right t <- items] of
      [] -> STerm a SHole
      ts -> last ts

-- | A block item is either a @name = term@ binding (Left) or the body term (Right).
pBlockItem :: PP (Either SBinding STerm)
pBlockItem =
  P.choice
    [ Left <$> P.try pBindingItem,
      Right <$> pTerm
    ]
  where
    pBindingItem = do
      (a, nm) <- withAnn nameRaw
      _ <- symbol "="
      SBinding a (pname nm) Nothing <$> pTerm

pMatch :: PP STerm
pMatch = L.indentBlock scn do
  (a, s) <- withAnn (P.try (symbol "match") *> pInlineExpr)
  _ <- symbol ":"
  pure (L.IndentSome Nothing (\cs -> pure (STerm a (SMatch s cs))) pCase)

pCase :: PP SCase
pCase = L.indentBlock scn do
  _ <- P.try (symbol "case")
  pat <- pPattern
  guard_ <- P.optional (symbol "if" *> pInlineExpr)
  _ <- symbol ":"
  pure (L.IndentSome Nothing (\items -> pure (SCase pat guard_ (lastItem (patAnn pat) items))) pTerm)

pLambda :: PP STerm
pLambda = do
  (a, (ps, body)) <- withAnn do
    _ <- symbol "lambda"
    ps <- commaSep nameRaw
    _ <- symbol ":"
    body <- pInlineExpr
    pure (ps, body)
  pure (STerm a (SLam [SParam a (pname p) | p <- ps] body))

-- | An inline (single-line) expression: no block forms.
pInlineExpr :: PP STerm
pInlineExpr = P.choice [P.try pLambda, pInfix]

pApp :: PP STerm
pApp = do
  (a, h) <- withAnn pAtom
  calls <- P.many (parens (commaSep pTermArg))
  pure (foldl (\f args -> STerm a (SApp f args)) h calls)
  where
    pTermArg = pInlineExpr

pAtom :: PP STerm
pAtom = P.choice [pDoc, pStringTerm, pCharTerm, pList, pParen, pNameAtom]

pDoc :: PP STerm
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

pNameAtom :: PP STerm
pNameAtom = do
  (a, nm) <- withAnn (P.notFollowedBy (P.choice (map (P.try . kw) reserved)) *> nameRaw)
  pure $ STerm a case nm of
    "True" -> SLit (SBool True)
    "False" -> SLit (SBool False)
    _
      | Just n <- readNat nm -> SLit (SNat n)
      | Just i <- readInt nm -> SLit (SInt i)
      | Just f <- readFloat nm -> SLit (SFloat f)
      | otherwise -> SName (sname nm)
  where
    kw s = lexeme (C.string s <* P.notFollowedBy (P.satisfy isWordyChar))

pStringTerm :: PP STerm
pStringTerm = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (STerm a (SLit (SText (Text.pack s))))

pCharTerm :: PP STerm
pCharTerm = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (STerm a (SLit (SChar c)))

pList :: PP STerm
pList = do
  (a, xs) <- withAnn (brackets (commaSep pInlineExpr))
  pure (STerm a (SList xs))

pParen :: PP STerm
pParen = do
  (a, f) <- withAnn (parens pParenBody)
  pure (STerm a f)
  where
    pParenBody = do
      e <- pInlineExpr
      P.choice
        [ symbol "if" *> ((\c el -> SIf c e el) <$> pInlineExpr <* symbol "else" <*> pInlineExpr),
          symbol "and" *> (SAnd e <$> pInlineExpr),
          symbol "or" *> (SOr e <$> pInlineExpr),
          symbol ":" *> (SAnn e <$> pType),
          symbol "," *> ((\rest -> STuple (e : rest)) <$> commaSep pInlineExpr),
          pure (tOut e)
        ]

-- Patterns -----------------------------------------------------------------------------------------------------------

pPattern :: PP SPattern
pPattern = P.choice [P.try pCtorPattern, pStringPat, pCharPat, pAtomPat]

pStringPat :: PP SPattern
pStringPat = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (SPattern a (SPLit (SText (Text.pack s))))

pCharPat :: PP SPattern
pCharPat = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (SPattern a (SPLit (SChar c)))

pAtomPat :: PP SPattern
pAtomPat = do
  (a, nm) <- withAnn nameRaw
  pure $ SPattern a case nm of
    "_" -> SPWild
    "True" -> SPLit (SBool True)
    "False" -> SPLit (SBool False)
    _
      | Just n <- readNat nm -> SPLit (SNat n)
      | Just i <- readInt nm -> SPLit (SInt i)
      | Just f <- readFloat nm -> SPLit (SFloat f)
      | otherwise -> SPVar (pname nm)

pCtorPattern :: PP SPattern
pCtorPattern = do
  (a, (nm, subs)) <- withAnn ((,) <$> nameRaw <*> parens (commaSep pPattern))
  pure (SPattern a (SPCtor (sname nm) subs))

-- Types --------------------------------------------------------------------------------------------------------------

pType :: PP SType
pType = P.choice [pForallTy, pArrowTy]

pForallTy :: PP SType
pForallTy = do
  (a, (vs, body)) <- withAnn do
    _ <- symbol "forall"
    vs <- commaSep nameRaw
    _ <- symbol "."
    body <- pType
    pure (vs, body)
  pure (SType a (STyForall (map pname vs) body))

pArrowTy :: PP SType
pArrowTy = do
  (a, ts) <- withAnn (P.sepBy1 pAppTy (symbol "->"))
  pure case ts of
    [t] -> t
    _ -> SType a (arrows ts)
  where
    arrows = \case
      [t] -> tyOut t
      (i : rest@(_ : _)) -> STyArrow i Nothing (SType (tyAnn i) (arrows rest))
      [] -> STyVar (pname "_")

pAppTy :: PP SType
pAppTy = do
  (a, t) <- withAnn pTypeAtom
  margs <- P.optional (brackets (commaSep pType))
  pure case margs of
    Nothing -> t
    Just args -> SType a (STyApp t args)

pTypeAtom :: PP SType
pTypeAtom = P.choice [pEffectsTy, parens pType, pNameTy]

pNameTy :: PP SType
pNameTy = do
  (a, nm) <- withAnn nameRaw
  pure (SType a (STyVar (pname nm)))

pEffectsTy :: PP SType
pEffectsTy = do
  (a, es) <- withAnn (braces (commaSep pType))
  pure (SType a (STyEffects es))

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
