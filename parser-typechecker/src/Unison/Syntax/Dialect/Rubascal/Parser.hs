{-# LANGUAGE OverloadedStrings #-}

-- | The Rubascal /parser/: text -> 'SFile'.
--
-- Reads the Pascal\/Ruby hybrid that 'Unison.Syntax.Dialect.Rubascal' prints, producing the 'Surface' IR;
-- 'Unison.Syntax.Surface.Elaborate' does the rest. Keywords are lowercase; statements within a block are separated by
-- @;@ (Pascal) and blocks close with a bare @end@ (Ruby). Grammar sketch (matching the renderer):
--
-- > name : Type
-- > def name(x, y) … e end                 -- last expression is the result (implicit return)
-- > name := e                              -- value binding (Pascal assignment)
-- > f(a, b)                                -- application
-- > if c then t else e end
-- > case s when p then body … end
-- > begin … e end                          -- let block
-- > type Color … Red … Blue … end
module Unison.Syntax.Dialect.Rubascal.Parser
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

type RP = P.Parsec Void String

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

sc :: RP ()
sc = L.space C.space1 P.empty P.empty

lexeme :: RP a -> RP a
lexeme = L.lexeme sc

symbol :: String -> RP String
symbol = L.symbol sc

-- | A whole-word keyword.
kw :: String -> RP ()
kw s = void (lexeme (P.try (C.string s <* P.notFollowedBy (P.satisfy isWordyChar))))

-- | A single @:@ that is not the start of @:=@ (the type-ascription colon, vs the assignment operator).
colon :: RP ()
colon = void (lexeme (P.try (C.char ':' <* P.notFollowedBy (C.char '='))))

parens, brackets, braces, angles :: RP a -> RP a
parens p = symbol "(" *> p <* symbol ")"
brackets p = symbol "[" *> p <* symbol "]"
braces p = symbol "{" *> p <* symbol "}"
angles p = symbol "<" *> p <* symbol ">"

commaSep :: RP a -> RP [a]
commaSep p = P.sepBy p (symbol ",")

withAnn :: RP a -> RP (Ann, a)
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

nameRaw :: RP String
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

reservedWords :: [String]
reservedWords =
  [ "def",
    "end",
    "begin",
    "if",
    "then",
    "else",
    "case",
    "when",
    "handle",
    "with",
    "delay",
    "and",
    "or",
    "forall",
    "type",
    "ability",
    "record",
    "structural",
    "termLink",
    "typeLink"
  ]

pPlainName :: RP String
pPlainName = P.try do
  nm <- nameRaw
  if nm `elem` reservedWords then fail ("keyword " <> nm) else pure nm

-- Top level ----------------------------------------------------------------------------------------------------------

pSFile :: RP SFile
pSFile = do
  sc
  forms <- concat <$> P.many pTopForm
  P.eof
  let sigs = [(n, t) | RSig n t <- forms]
      binds = [b {bType = lookup (bName b) sigs <|> bType b} | RBind b <- forms]
      decls = [d | RDecl d <- forms]
      watches = [w | RWatch w <- forms]
  pure (SFile Nothing decls binds watches)

pTopForm :: RP [RawForm]
pTopForm =
  P.choice
    [ P.try ((: []) <$> pWatch),
      pDocumented
    ]
  where
    pDocumented = do
      mdoc <- P.optional (withAnn pDocRaw)
      form <- P.choice [pDecl, pFunction, P.try pSigForm, pValueForm]
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

pWatch :: RP RawForm
pWatch = do
  (a, kind) <- withAnn (P.choice [WK.TestWatch <$ symbol "test>", WK.RegularWatch <$ symbol ">"])
  RWatch . SWatch a kind <$> pTerm

srcAnn :: P.SourcePos -> Ann
srcAnn sp = Ann.Ann p p where p = Pos.Pos (P.unPos (P.sourceLine sp)) (P.unPos (P.sourceColumn sp))

-- | A standalone type signature: @name : Type@.
pSigForm :: RP RawForm
pSigForm = do
  nm <- pPlainName
  colon
  RSig (pname nm) <$> pType

-- | A top-level value binding: @name := e@.
pValueForm :: RP RawForm
pValueForm = do
  (a, nm) <- withAnn pPlainName
  _ <- symbol ":="
  RBind . SBinding a (pname nm) Nothing <$> pTerm

-- Declarations -------------------------------------------------------------------------------------------------------

pModifier :: RP SModifier
pModifier = P.option (SUnique "") (SStructural <$ kw "structural")

data DeclKind = DKData | DKAbility | DKRecord deriving (Eq)

pDecl :: RP RawForm
pDecl = do
  start <- P.getSourcePos
  modi <- pModifier
  kind <- P.choice [DKData <$ kw "type", DKAbility <$ kw "ability", DKRecord <$ kw "record"]
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
      _ <- kw "end"
      let fieldNames = map fst fields
          ctorType = foldr (\(_, t) acc -> SType a (STyArrow t Nothing acc)) selfTy fields
      pure (RDecl (SDecl a modi False (pname nm) tyvarNames [SConstructor a (pname nm) ctorType] (Just fieldNames)))
    _ -> do
      let isAb = kind == DKAbility
      ctors <- P.many (pCtor isAb selfTy)
      _ <- kw "end"
      pure (RDecl (SDecl a modi isAb (pname nm) tyvarNames ctors Nothing))

pField :: RP (Name, SType)
pField = (\nm t -> (pname nm, t)) <$> pPlainName <* colon <*> pType

pCtor :: Bool -> SType -> RP SConstructor
pCtor isAb selfTy = do
  (ca, cn) <- withAnn pPlainName
  if isAb
    then colon *> (SConstructor ca (pname cn) <$> pType)
    else do
      margs <- P.optional (parens (commaSep pType))
      let args = fromMaybe [] margs
      pure (SConstructor ca (pname cn) (foldr (\arg acc -> SType ca (STyArrow arg Nothing acc)) selfTy args))

-- Functions and blocks -----------------------------------------------------------------------------------------------

data BItem = BISig Name SType | BIBind SBinding | BITerm STerm

pBItem :: RP BItem
pBItem =
  P.choice
    [ P.try pSig,
      BIBind <$> pNestedDef,
      BIBind <$> P.try pValueBind,
      BITerm <$> pTerm
    ]
  where
    pSig = do
      nm <- pPlainName
      colon
      BISig (pname nm) <$> pType
    pValueBind = do
      (a, nm) <- withAnn pPlainName
      _ <- symbol ":="
      SBinding a (pname nm) Nothing <$> pTerm

pFunction :: RP RawForm
pFunction = do
  (a, nm, ps, body) <- pDefParts
  pure (RBind (SBinding a (pname nm) Nothing (STerm a (SLam ps body))))

pNestedDef :: RP SBinding
pNestedDef = do
  (a, nm, ps, body) <- pDefParts
  pure (SBinding a (pname nm) Nothing (STerm a (SLam ps body)))

-- | The shared @def name(p, …) … end@ form. The final non-signature item is the result (implicit return).
pDefParts :: RP (Ann, String, [SParam], STerm)
pDefParts = do
  (a, _) <- withAnn (kw "def")
  nm <- pPlainName
  ps <- parens (commaSep (snd <$> withAnn pPlainName))
  items <- P.many (pBItem <* P.optional (symbol ";"))
  _ <- kw "end"
  pure (a, nm, [SParam a (pname p) | p <- ps], assembleBlock a items)

pBlock :: RP STerm
pBlock = do
  (a, items) <- withAnn (kw "begin" *> P.many (pBItem <* P.optional (symbol ";")) <* kw "end")
  pure (assembleBlock a items)

-- | A block whose final non-signature item is the result; earlier items are bindings (sigs pair into them).
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

pTerm :: RP STerm
pTerm = P.choice [pLambda, pInfix]

pInfix :: RP STerm
pInfix = do
  lhs <- pApp
  rest <- P.many ((,) <$> pOp <*> pApp)
  pure (resolveInfix lhs rest)

pOp :: RP String
pOp = P.try (nameRaw >>= check)
  where
    check s
      | s `elem` reserved = fail "reserved operator"
      | isOpName s = pure s
      | otherwise = fail "operator"
    reserved = ["=", ":=", "->", ":", "|", "<-"]

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

-- | A stabby lambda: @->(x, y) body@.
pLambda :: RP STerm
pLambda = do
  (a, (ps, body)) <- withAnn do
    _ <- symbol "->"
    ps <- parens (commaSep (snd <$> withAnn pPlainName))
    body <- pTerm
    pure (ps, body)
  pure (STerm a (SLam [SParam a (pname p) | p <- ps] body))

pApp :: RP STerm
pApp = do
  (a, h) <- withAnn pAtom
  calls <- P.many (parens (commaSep pTerm))
  let force args = if null args then [STerm a (STuple [])] else args
  pure (foldl (\f args -> STerm a (SApp f (force args))) h calls)

pAtom :: RP STerm
pAtom = P.choice [pDoc, pStringTerm, pCharTerm, pList, pIf, pMatch, pBlock, pHandle, pDelay, pParen, pNameAtom]

pIf :: RP STerm
pIf = do
  (a, _) <- withAnn (kw "if")
  c <- pTerm
  _ <- kw "then"
  t <- pTerm
  _ <- kw "else"
  e <- pTerm
  _ <- kw "end"
  pure (STerm a (SIf c t e))

pMatch :: RP STerm
pMatch = do
  (a, _) <- withAnn (kw "case")
  s <- pTerm
  cs <- P.many pCase
  _ <- kw "end"
  pure (STerm a (SMatch s cs))

pCase :: RP SCase
pCase = do
  _ <- kw "when"
  pat <- pPattern
  guard_ <- P.optional (kw "if" *> pTerm)
  _ <- kw "then"
  SCase pat guard_ <$> pTerm

pHandle :: RP STerm
pHandle = do
  (a, _) <- withAnn (kw "handle")
  e <- pTerm
  _ <- kw "with"
  h <- pTerm
  _ <- kw "end"
  pure (STerm a (SHandle h e))

pDelay :: RP STerm
pDelay = do
  (a, _) <- withAnn (kw "delay")
  STerm a . SDelay <$> parens pTerm

pDoc :: RP STerm
pDoc = do
  (a, txt) <- withAnn pDocRaw
  pure (STerm a (SDocLit (Text.pack txt)))

pDocRaw :: RP String
pDocRaw = lexeme (C.string "{{" *> scanDoc (1 :: Int) "{{")
  where
    scanDoc depth acc =
      P.choice
        [ P.try (C.string "}}") *> (let d = depth - 1 in if d == 0 then pure (acc <> "}}") else scanDoc d (acc <> "}}")),
          P.try (C.string "{{") *> scanDoc (depth + 1) (acc <> "{{"),
          P.anySingle >>= \c -> scanDoc depth (acc <> [c])
        ]

pNameAtom :: RP STerm
pNameAtom = do
  (a, nm) <- withAnn pPlainName
  pure $ STerm a case nm of
    "true" -> SLit (SBool True)
    "false" -> SLit (SBool False)
    _
      | Just n <- readNat nm -> SLit (SNat n)
      | Just i <- readInt nm -> SLit (SInt i)
      | Just f <- readFloat nm -> SLit (SFloat f)
      | otherwise -> SName (sname nm)

pStringTerm :: RP STerm
pStringTerm = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (STerm a (SLit (SText (Text.pack s))))

pCharTerm :: RP STerm
pCharTerm = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (STerm a (SLit (SChar c)))

pList :: RP STerm
pList = do
  (a, xs) <- withAnn (brackets (commaSep pTerm))
  pure (STerm a (SList xs))

pParen :: RP STerm
pParen = do
  (a, f) <- withAnn (parens (P.option (STuple []) pParenBody))
  pure (STerm a f)
  where
    pParenBody = do
      e <- pTerm
      P.choice
        [ colon *> (SAnn e <$> pType),
          kw "and" *> (SAnd e <$> pTerm),
          kw "or" *> (SOr e <$> pTerm),
          symbol "," *> ((\rest -> STuple (e : rest)) <$> commaSep pTerm),
          pure (tOut e)
        ]

-- Patterns -----------------------------------------------------------------------------------------------------------

pPattern :: RP SPattern
pPattern = P.choice [pListPat, pParenPat, pEffectPat, P.try pAsPat, P.try pCtorPattern, pStringPat, pCharPat, pAtomPat]

pListPat :: RP SPattern
pListPat = do
  (a, subs) <- withAnn (brackets (commaSep pPattern))
  pure (SPattern a (SPList subs))

pParenPat :: RP SPattern
pParenPat = do
  (a, items) <- withAnn (parens (commaSep pSeqPat))
  pure case items of
    [p] -> p
    _ -> SPattern a (SPTuple items)

pSeqPat :: RP SPattern
pSeqPat = do
  l <- pPattern
  P.optional pSeqOp >>= \case
    Nothing -> pure l
    Just op -> do
      r <- pPattern
      pure (SPattern (patAnn l) (SPSeqOp l op r))

pSeqOp :: RP SSeqOp
pSeqOp = P.choice [SCons <$ P.try (symbol "+:"), SSnoc <$ P.try (symbol ":+"), SConcat <$ P.try (symbol "++")]

pAsPat :: RP SPattern
pAsPat = do
  (a, nm) <- withAnn pPlainName
  _ <- symbol "@"
  SPattern a . SPAs (pname nm) <$> pPattern

pEffectPat :: RP SPattern
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

pStringPat :: RP SPattern
pStringPat = do
  (a, s) <- withAnn (lexeme (C.char '"' *> P.manyTill L.charLiteral (C.char '"')))
  pure (SPattern a (SPLit (SText (Text.pack s))))

pCharPat :: RP SPattern
pCharPat = do
  (a, c) <- withAnn (lexeme (C.char '\'' *> L.charLiteral <* C.char '\''))
  pure (SPattern a (SPLit (SChar c)))

pAtomPat :: RP SPattern
pAtomPat = do
  (a, nm) <- withAnn pPlainName
  pure $ SPattern a case nm of
    "_" -> SPWild
    "true" -> SPLit (SBool True)
    "false" -> SPLit (SBool False)
    _
      | Just n <- readNat nm -> SPLit (SNat n)
      | Just i <- readInt nm -> SPLit (SInt i)
      | Just f <- readFloat nm -> SPLit (SFloat f)
      | otherwise -> SPVar (pname nm)

pCtorPattern :: RP SPattern
pCtorPattern = do
  (a, (nm, subs)) <- withAnn ((,) <$> pPlainName <*> parens (commaSep pPattern))
  pure (SPattern a (SPCtor (sname nm) subs))

-- Types --------------------------------------------------------------------------------------------------------------

pType :: RP SType
pType = P.choice [pForallTy, pArrow]

pForallTy :: RP SType
pForallTy = do
  (a, (vs, body)) <- withAnn do
    _ <- kw "forall"
    vs <- commaSep pPlainName
    _ <- symbol "."
    body <- pType
    pure (vs, body)
  pure (SType a (STyForall (map pname vs) body))

pAppTy :: RP SType
pAppTy = do
  (a, t) <- withAnn pTypeAtom
  margs <- P.optional (angles (commaSep pType))
  pure case margs of
    Nothing -> t
    Just args -> SType a (STyApp t args)

pTypeAtom :: RP SType
pTypeAtom = P.choice [pEffectsTy, pParenTy, pNameTy]

pNameTy :: RP SType
pNameTy = do
  (a, nm) <- withAnn pPlainName
  pure (SType a (STyVar (pname nm)))

pEffectsTy :: RP SType
pEffectsTy = do
  (a, es) <- withAnn (braces (commaSep pType))
  P.optional pAppTy >>= \case
    Nothing -> pure (SType a (STyEffects es))
    Just t -> pure (SType a (STyEffectful es t))

pParenTy :: RP SType
pParenTy = do
  (a, items) <- withAnn (parens (commaSep pArrow))
  pure case items of
    [t] -> t
    _ -> SType a (STyTuple items)

pArrow :: RP SType
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
