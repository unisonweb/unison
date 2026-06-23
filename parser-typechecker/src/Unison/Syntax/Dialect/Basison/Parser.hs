{-# LANGUAGE OverloadedStrings #-}

-- | The Basison /parser/: text -> 'SFile'.
--
-- Reads the BASIC-flavored dialect that 'Unison.Syntax.Dialect.Basison' prints, producing the 'Surface' IR;
-- 'Unison.Syntax.Surface.Elaborate' does the rest. Keywords are UPPERCASE; statements within a block are separated by
-- @:@ (BASIC's statement separator). Grammar sketch (matching the renderer):
--
-- > name AS Type
-- > FUNCTION name(x, y) … RETURN e END FUNCTION
-- > LET name = e
-- > f(a, b)                              -- application
-- > a + b * c                            -- infix operators, precedence-climbing
-- > IF c THEN t ELSE e END IF
-- > SELECT CASE s … CASE p: body … END SELECT
-- > BLOCK … e END BLOCK                  -- let block
-- > TYPE Color … Red … Blue … END TYPE
module Unison.Syntax.Dialect.Basison.Parser
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
import Unison.Name qualified as Name (snoc)
import Unison.NameSegment qualified as NameSegment (docSegment)
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.Syntax.Name qualified as Name (isSymboly, parseTextEither, unsafeParseText)
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.Precedence (InfixPrecedence (Lowest), Precedence (Bottom, InfixOp), increment, operatorPrecedence)
import Unison.Syntax.Surface
import Unison.Syntax.Surface.Elaborate (elaborateFile, parseDocs, resolveDeclGuids)
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

type BP = P.Parsec Void String

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

sc :: BP ()
sc = L.space C.space1 P.empty P.empty

lexeme :: BP a -> BP a
lexeme = L.lexeme sc

symbol :: String -> BP String
symbol = L.symbol sc

-- | A whole-word keyword (so @AS@ matches the keyword but @ascii@ stays an identifier).
kw :: String -> BP ()
kw s = void (lexeme (P.try (C.string s <* P.notFollowedBy (P.satisfy isWordyChar))))

parens, brackets, braces, angles :: BP a -> BP a
parens p = symbol "(" *> p <* symbol ")"
brackets p = symbol "[" *> p <* symbol "]"
braces p = symbol "{" *> p <* symbol "}"
angles p = symbol "<" *> p <* symbol ">"

commaSep :: BP a -> BP [a]
commaSep p = P.sepBy p (symbol ",")

withAnn :: BP a -> BP (Ann, a)
withAnn p = do
  s <- P.getSourcePos
  x <- p
  e <- P.getSourcePos
  pure (Ann.Ann (toPos s) (toPos e), x)
  where
    toPos sp = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

isWordyStart :: Char -> Bool
isWordyStart c = isAlphaNum c || c == '_'

isWordyChar :: Char -> Bool
isWordyChar c = isAlphaNum c || c == '_' || c == '!' || c == '\''

isSymChar :: Char -> Bool
isSymChar c = c `elem` ("+-*/<>=!&|^%~$:" :: String)

nameRaw :: BP String
nameRaw = lexeme do
  first <- seg
  rest <- P.many (P.try (C.char '.' *> seg))
  pure (intercalate "." (first : rest))
  where
    seg = wordy P.<|> P.takeWhile1P (Just "operator") isSymChar
    wordy = (:) <$> P.satisfy isWordyStart <*> P.takeWhileP (Just "wordy") isWordyChar

pname :: String -> Name
pname = Name.unsafeParseText . Text.pack

sname :: String -> SName
sname = HQ.NameOnly . pname

isOpName :: String -> Bool
isOpName s = not (null s) && either (const False) Name.isSymboly (Name.parseTextEither (Text.pack s))

-- | The UPPERCASE keywords (excluding @TRUE@\/@FALSE@, which classify as boolean literals).
reservedWords :: [String]
reservedWords =
  [ "FUNCTION",
    "END",
    "BLOCK",
    "RETURN",
    "LET",
    "LAMBDA",
    "IF",
    "THEN",
    "ELSE",
    "SELECT",
    "CASE",
    "HANDLE",
    "WITH",
    "DELAY",
    "AND",
    "OR",
    "AS",
    "FORALL",
    "TYPE",
    "ABILITY",
    "RECORD",
    "STRUCTURAL",
    "TERMLINK",
    "TYPELINK"
  ]

-- | A name that is not a reserved keyword (so block\/application parsing stops at a keyword like @END@ or @RETURN@).
pPlainName :: BP String
pPlainName = P.try do
  nm <- nameRaw
  if nm `elem` reservedWords then fail ("keyword " <> nm) else pure nm

-- Top level ----------------------------------------------------------------------------------------------------------

pSFile :: BP SFile
pSFile = do
  sc
  forms <- concat <$> P.many pTopForm
  P.eof
  let sigs = [(n, t) | RSig n t <- forms]
      binds = [b {bType = lookup (bName b) sigs <|> bType b} | RBind b <- forms]
      decls = [d | RDecl d <- forms]
      watches = [w | RWatch w <- forms]
  pure (SFile Nothing decls binds watches)

pTopForm :: BP [RawForm]
pTopForm =
  P.choice
    [ P.try ((: []) <$> pWatch),
      pDocumented
    ]
  where
    pDocumented = do
      mdoc <- P.optional (withAnn pDocRaw)
      form <- P.choice [pDecl, pFunction, P.try pSigForm, pLetForm]
      pure case mdoc of
        Just (a, txt) | Just nm <- formName form -> [docBinding a nm txt, form]
        _ -> [form]

formName :: RawForm -> Maybe Name
formName = \case
  RBind b -> Just (bName b)
  RSig n _ -> Just n
  RDecl d -> Just (dName d)
  RWatch _ -> Nothing

docBinding :: Ann -> Name -> String -> RawForm
docBinding a nm txt = RBind (SBinding a (Name.snoc nm NameSegment.docSegment) Nothing (STerm a (SDocLit (Text.pack txt))))

pWatch :: BP RawForm
pWatch = do
  (a, kind) <- withAnn (P.choice [WK.TestWatch <$ symbol "test>", WK.RegularWatch <$ symbol ">"])
  RWatch . SWatch a kind <$> pTerm

srcAnn :: P.SourcePos -> Ann
srcAnn sp = Ann.Ann p p where p = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

-- | A standalone type signature: @name AS Type@.
pSigForm :: BP RawForm
pSigForm = do
  nm <- pPlainName
  _ <- kw "AS"
  RSig (pname nm) <$> pType

-- | A top-level value binding: @LET name = e@.
pLetForm :: BP RawForm
pLetForm = do
  _ <- kw "LET"
  (a, nm) <- withAnn pPlainName
  _ <- symbol "="
  RBind . SBinding a (pname nm) Nothing <$> pTerm

-- Declarations -------------------------------------------------------------------------------------------------------

pModifier :: BP SModifier
pModifier = P.option (SUnique "") (SStructural <$ kw "STRUCTURAL")

data DeclKind = DKData | DKAbility | DKRecord deriving (Eq)

pDecl :: BP RawForm
pDecl = do
  start <- P.getSourcePos
  modi <- pModifier
  kind <- P.choice [DKData <$ kw "TYPE", DKAbility <$ kw "ABILITY", DKRecord <$ kw "RECORD"]
  nm <- pPlainName
  tvs <- P.option [] (angles (commaSep pPlainName))
  let a = srcAnn start
      tyvarNames = map pname tvs
      selfTy =
        SType a $
          if null tyvarNames
            then STyVar (pname nm)
            else STyApp (SType a (STyVar (pname nm))) [SType a (STyVar t) | t <- tyvarNames]
  case kind of
    DKRecord -> do
      fields <- P.many pField
      _ <- kw "END" *> kw "RECORD"
      let fieldNames = map fst fields
          ctorType = foldr (\(_, t) acc -> SType a (STyArrow t Nothing acc)) selfTy fields
      pure (RDecl (SDecl a modi False (pname nm) tyvarNames [SConstructor a (pname nm) ctorType] (Just fieldNames)))
    _ -> do
      let isAb = kind == DKAbility
      ctors <- P.many (pCtor isAb selfTy)
      _ <- kw "END" *> (if isAb then kw "ABILITY" else kw "TYPE")
      pure (RDecl (SDecl a modi isAb (pname nm) tyvarNames ctors Nothing))

pField :: BP (Name, SType)
pField = (\nm t -> (pname nm, t)) <$> pPlainName <* kw "AS" <*> pType

pCtor :: Bool -> SType -> BP SConstructor
pCtor isAb selfTy = do
  (ca, cn) <- withAnn pPlainName
  if isAb
    then kw "AS" *> (SConstructor ca (pname cn) <$> pType)
    else do
      margs <- P.optional (parens (commaSep pType))
      let args = fromMaybe [] margs
      pure (SConstructor ca (pname cn) (foldr (\arg acc -> SType ca (STyArrow arg Nothing acc)) selfTy args))

-- Functions and blocks -----------------------------------------------------------------------------------------------

-- | A block item: a @name AS Type@ signature, a nested @FUNCTION@\/@LET@ binding, or a bare term.
data BItem = BISig Name SType | BIBind SBinding | BITerm STerm

pBItem :: BP BItem
pBItem =
  P.choice
    [ P.try pSig,
      BIBind <$> pNestedFunc,
      BIBind <$> pLetBind,
      BITerm <$> pTerm
    ]
  where
    pSig = do
      nm <- pPlainName
      _ <- kw "AS"
      BISig (pname nm) <$> pType
    pLetBind = do
      _ <- kw "LET"
      (a, nm) <- withAnn pPlainName
      _ <- symbol "="
      SBinding a (pname nm) Nothing <$> pTerm

-- | A top-level @FUNCTION@ definition.
pFunction :: BP RawForm
pFunction = do
  (a, nm, ps, body) <- pFunctionParts
  pure (RBind (SBinding a (pname nm) Nothing (STerm a (SLam ps body))))

-- | A nested @FUNCTION@ definition (inside a block), as a binding.
pNestedFunc :: BP SBinding
pNestedFunc = do
  (a, nm, ps, body) <- pFunctionParts
  pure (SBinding a (pname nm) Nothing (STerm a (SLam ps body)))

-- | The shared @FUNCTION name(p, …) … RETURN e END FUNCTION@ form. Body statements precede an explicit @RETURN@.
pFunctionParts :: BP (Ann, String, [SParam], STerm)
pFunctionParts = do
  (a, _) <- withAnn (kw "FUNCTION")
  nm <- pPlainName
  ps <- parens (commaSep (snd <$> withAnn pPlainName))
  items <- P.many (pBItem <* P.optional (symbol ":"))
  _ <- kw "RETURN"
  e <- pTerm
  _ <- P.optional (symbol ":")
  _ <- kw "END" *> kw "FUNCTION"
  let binds = collectBinds a items
      body = if null binds then e else STerm a (SLet binds e)
  pure (a, nm, [SParam a (pname p) | p <- ps], body)

-- | Pair signature lines into the bindings that follow them; turn bare terms into discarded (@_@) bindings.
collectBinds :: Ann -> [BItem] -> [SBinding]
collectBinds a items = map toBind (filter notSig items)
  where
    sigs = [(n, t) | BISig n t <- items]
    withSig b = case lookup (bName b) sigs of
      Just t -> b {bValue = STerm (bAnn b) (SAnn (bValue b) t)}
      Nothing -> b
    toBind (BIBind b) = withSig b
    toBind (BITerm t) = SBinding a (pname "_") Nothing t
    toBind (BISig _ _) = SBinding a (pname "_") Nothing (STerm a SHole)
    notSig (BISig _ _) = False
    notSig _ = True

-- | A @BLOCK … e END BLOCK@ let-block term.
pBlock :: BP STerm
pBlock = do
  (a, items) <- withAnn (kw "BLOCK" *> P.many (pBItem <* P.optional (symbol ":")) <* kw "END" <* kw "BLOCK")
  pure (assembleBlock a items)

-- | A block whose final non-signature item is the result; earlier items are bindings.
assembleBlock :: Ann -> [BItem] -> STerm
assembleBlock a items =
  let sigs = [(n, t) | BISig n t <- items]
      withSig b = case lookup (bName b) sigs of
        Just t -> b {bValue = STerm (bAnn b) (SAnn (bValue b) t)}
        Nothing -> b
      toBind (BIBind b) = withSig b
      toBind (BITerm t) = SBinding a (pname "_") Nothing t
      toBind (BISig _ _) = SBinding a (pname "_") Nothing (STerm a SHole)
      bodyOf (BITerm t) = t
      bodyOf (BIBind b) = bValue (withSig b)
      bodyOf (BISig _ _) = STerm a SHole
      notSig = \case BISig _ _ -> False; _ -> True
   in case filter notSig items of
        [] -> STerm a SHole
        real ->
          let binds = map toBind (init real)
              result = bodyOf (last real)
           in if null binds then result else STerm a (SLet binds result)

-- Terms --------------------------------------------------------------------------------------------------------------

pTerm :: BP STerm
pTerm = P.choice [pLambda, pInfix]

pInfix :: BP STerm
pInfix = do
  lhs <- pApp
  rest <- P.many ((,) <$> pOp <*> pApp)
  pure (resolveInfix lhs rest)

pOp :: BP String
pOp = P.try (nameRaw >>= check)
  where
    check s
      | s `elem` reserved = fail "reserved operator"
      | isOpName s = pure s
      | otherwise = fail "operator"
    reserved = ["=", "->", ":", "|", "<-"]

precFor :: String -> Precedence
precFor s = fromMaybe (InfixOp Lowest) (operatorPrecedence (Text.pack (lastSeg s)))
  where
    lastSeg = reverse . takeWhile (/= '.') . reverse

resolveInfix :: STerm -> [(String, STerm)] -> STerm
resolveInfix lhs0 toks0 = fst (parseExpr lhs0 toks0 Bottom)
  where
    parseExpr lhs toks minPrec = case toks of
      ((op, rhs) : rest)
        | precFor op >= minPrec ->
            let (rhs', rest') = climbRhs rhs rest (precFor op)
             in parseExpr (STerm (tAnn lhs) (SBinOp (sname op) (precFor op) lhs rhs')) rest' minPrec
      _ -> (lhs, toks)
    climbRhs rhs toks opPrec = case toks of
      ((op2, _) : _)
        | precFor op2 > opPrec ->
            let (rhs', rest') = parseExpr rhs toks (increment opPrec)
             in climbRhs rhs' rest' opPrec
      _ -> (rhs, toks)

pLambda :: BP STerm
pLambda = do
  (a, (ps, body)) <- withAnn do
    _ <- kw "LAMBDA"
    ps <- parens (commaSep (snd <$> withAnn pPlainName))
    body <- pTerm
    pure (ps, body)
  pure (STerm a (SLam [SParam a (pname p) | p <- ps] body))

pApp :: BP STerm
pApp = do
  (a, h) <- withAnn pAtom
  calls <- P.many (parens (commaSep pTerm))
  let force args = if null args then [STerm a (STuple [])] else args
  pure (foldl (\f args -> STerm a (SApp f (force args))) h calls)

pAtom :: BP STerm
pAtom = P.choice [pDoc, pStringTerm, pCharTerm, pList, pIf, pMatch, pBlock, pHandle, pDelay, pParen, pNameAtom]

pIf :: BP STerm
pIf = do
  (a, _) <- withAnn (kw "IF")
  c <- pTerm
  _ <- kw "THEN"
  t <- pTerm
  _ <- kw "ELSE"
  e <- pTerm
  _ <- kw "END" *> kw "IF"
  pure (STerm a (SIf c t e))

pMatch :: BP STerm
pMatch = do
  (a, _) <- withAnn (kw "SELECT" *> kw "CASE")
  s <- pTerm
  cs <- P.many pCase
  _ <- kw "END" *> kw "SELECT"
  pure (STerm a (SMatch s cs))

pCase :: BP SCase
pCase = do
  _ <- kw "CASE"
  pat <- pPattern
  guard_ <- P.optional (kw "IF" *> pTerm)
  _ <- symbol ":"
  SCase pat guard_ <$> pTerm

pHandle :: BP STerm
pHandle = do
  (a, _) <- withAnn (kw "HANDLE")
  e <- pTerm
  _ <- kw "WITH"
  h <- pTerm
  _ <- kw "END" *> kw "HANDLE"
  pure (STerm a (SHandle h e))

pDelay :: BP STerm
pDelay = do
  (a, _) <- withAnn (kw "DELAY")
  STerm a . SDelay <$> parens pTerm

pDoc :: BP STerm
pDoc = do
  (a, txt) <- withAnn pDocRaw
  pure (STerm a (SDocLit (Text.pack txt)))

pDocRaw :: BP String
pDocRaw = lexeme (C.string "{{" *> scanDoc (1 :: Int) "{{")
  where
    scanDoc depth acc =
      P.choice
        [ P.try (C.string "}}") *> (let d = depth - 1 in if d == 0 then pure (acc <> "}}") else scanDoc d (acc <> "}}")),
          P.try (C.string "{{") *> scanDoc (depth + 1) (acc <> "{{"),
          P.anySingle >>= \c -> scanDoc depth (acc <> [c])
        ]

pNameAtom :: BP STerm
pNameAtom = do
  (a, nm) <- withAnn pPlainName
  pure $ STerm a case nm of
    "TRUE" -> SLit (SBool True)
    "FALSE" -> SLit (SBool False)
    _
      | Just n <- readNat nm -> SLit (SNat n)
      | Just i <- readInt nm -> SLit (SInt i)
      | Just f <- readFloat nm -> SLit (SFloat f)
      | otherwise -> SName (sname nm)

pStringTerm :: BP STerm
pStringTerm = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (STerm a (SLit (SText (Text.pack s))))

pCharTerm :: BP STerm
pCharTerm = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (STerm a (SLit (SChar c)))

pList :: BP STerm
pList = do
  (a, xs) <- withAnn (brackets (commaSep pTerm))
  pure (STerm a (SList xs))

pParen :: BP STerm
pParen = do
  (a, f) <- withAnn (parens (P.option (STuple []) pParenBody))
  pure (STerm a f)
  where
    pParenBody = do
      e <- pTerm
      P.choice
        [ kw "AS" *> (SAnn e <$> pType),
          kw "AND" *> (SAnd e <$> pTerm),
          kw "OR" *> (SOr e <$> pTerm),
          symbol "," *> ((\rest -> STuple (e : rest)) <$> commaSep pTerm),
          pure (tOut e)
        ]

-- Patterns -----------------------------------------------------------------------------------------------------------

pPattern :: BP SPattern
pPattern = P.choice [pListPat, pParenPat, pEffectPat, P.try pAsPat, P.try pCtorPattern, pStringPat, pCharPat, pAtomPat]

pListPat :: BP SPattern
pListPat = do
  (a, subs) <- withAnn (brackets (commaSep pPattern))
  pure (SPattern a (SPList subs))

pParenPat :: BP SPattern
pParenPat = do
  (a, items) <- withAnn (parens (commaSep pSeqPat))
  pure case items of
    [p] -> p
    _ -> SPattern a (SPTuple items)

pSeqPat :: BP SPattern
pSeqPat = do
  l <- pPattern
  P.optional pSeqOp >>= \case
    Nothing -> pure l
    Just op -> do
      r <- pPattern
      pure (SPattern (patAnn l) (SPSeqOp l op r))

pSeqOp :: BP SSeqOp
pSeqOp = P.choice [SCons <$ P.try (symbol "+:"), SSnoc <$ P.try (symbol ":+"), SConcat <$ P.try (symbol "++")]

pAsPat :: BP SPattern
pAsPat = do
  (a, nm) <- withAnn pPlainName
  _ <- symbol "@"
  SPattern a . SPAs (pname nm) <$> pPattern

pEffectPat :: BP SPattern
pEffectPat = braces (P.choice [P.try pRequest, pPure])
  where
    pRequest = do
      (a, nm) <- withAnn pPlainName
      subs <- parens (commaSep pPattern)
      _ <- symbol "->"
      SPattern a . SPEffect (sname nm) subs <$> pPattern
    pPure = do
      p <- pPattern
      pure (SPattern (patAnn p) (SPEffectPure p))

pStringPat :: BP SPattern
pStringPat = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (SPattern a (SPLit (SText (Text.pack s))))

pCharPat :: BP SPattern
pCharPat = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (SPattern a (SPLit (SChar c)))

pAtomPat :: BP SPattern
pAtomPat = do
  (a, nm) <- withAnn pPlainName
  pure $ SPattern a case nm of
    "_" -> SPWild
    "TRUE" -> SPLit (SBool True)
    "FALSE" -> SPLit (SBool False)
    _
      | Just n <- readNat nm -> SPLit (SNat n)
      | Just i <- readInt nm -> SPLit (SInt i)
      | Just f <- readFloat nm -> SPLit (SFloat f)
      | otherwise -> SPVar (pname nm)

pCtorPattern :: BP SPattern
pCtorPattern = do
  (a, (nm, subs)) <- withAnn ((,) <$> pPlainName <*> parens (commaSep pPattern))
  pure (SPattern a (SPCtor (sname nm) subs))

-- Types --------------------------------------------------------------------------------------------------------------

pType :: BP SType
pType = P.choice [pForallTy, pArrow]

pForallTy :: BP SType
pForallTy = do
  (a, (vs, body)) <- withAnn do
    _ <- kw "FORALL"
    vs <- commaSep pPlainName
    _ <- symbol "."
    body <- pType
    pure (vs, body)
  pure (SType a (STyForall (map pname vs) body))

pAppTy :: BP SType
pAppTy = do
  (a, t) <- withAnn pTypeAtom
  margs <- P.optional (angles (commaSep pType))
  pure case margs of
    Nothing -> t
    Just args -> SType a (STyApp t args)

pTypeAtom :: BP SType
pTypeAtom = P.choice [pEffectsTy, pParenTy, pNameTy]

pNameTy :: BP SType
pNameTy = do
  (a, nm) <- withAnn pPlainName
  pure (SType a (STyVar (pname nm)))

pEffectsTy :: BP SType
pEffectsTy = do
  (a, es) <- withAnn (braces (commaSep pType))
  P.optional pAppTy >>= \case
    Nothing -> pure (SType a (STyEffects es))
    Just t -> pure (SType a (STyEffectful es t))

pParenTy :: BP SType
pParenTy = do
  (a, items) <- withAnn (parens (commaSep pArrow))
  pure case items of
    [t] -> t
    _ -> SType a (STyTuple items)

pArrow :: BP SType
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
