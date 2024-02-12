{-# LANGUAGE DataKinds #-}

module Unison.LSP.SemanticTokens (semanticTokensFullHandler) where

import Control.Lens hiding (List)
import Data.Char qualified as Char
import Data.List.Extra qualified as List
import Language.LSP.Protocol.Lens hiding (id, to)
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Unison.ABT qualified as ABT
import Unison.Debug qualified as Debug
import Unison.LSP.Conversions (uToLspPos)
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Parser.Ann (Ann)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.Syntax.Lexer qualified as Lexer
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.UnisonFile qualified as UF

-- | We can improve this by using the parser-output rather than the lexer, but as a first step, this is fine, and as a bonus works on anything that can be lexed so you're not missing syntax highlighting when the file doesn't parse.
semanticTokensFullHandler :: Msg.TRequestMessage 'Msg.Method_TextDocumentSemanticTokensFull -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentSemanticTokensFull) -> Lsp ()) -> Lsp ()
semanticTokensFullHandler m respond = do
  mayTokens <- runMaybeT $ do
    let fileUri = m ^. params . textDocument . uri
    FileAnalysis {lexedSource = (_src, lexemes), parsedFile} <- getFileAnalysis fileUri
    let lexerTokens = mapMaybe semanticTokenForLexeme lexemes
    let parserTokens = case parsedFile of
          Nothing -> []
          Just (UF.UnisonFileId {terms, watches}) ->
            (terms ^.. folded . _3 . folding semanticTokensForTermABT)
              <> watches ^.. folded . folded . _3 . folding semanticTokensForTermABT
    Debug.debugM Debug.Temp "parser-tokens" parserTokens
    let allTokens =
          (parserTokens <> lexerTokens)
            & sortOn (\t -> (t ^. line, t ^. startChar))
            -- Might have duplicate tokens from parser AND lexer, prefer the parser ones which
            -- should be sorted first.
            & List.nubOrdOn (\t -> (t ^. line, t ^. startChar))
    case makeSemanticTokens LSP.defaultSemanticTokensLegend allTokens of
      Left err -> lift (logError err) *> empty
      Right tokens -> pure tokens
  case mayTokens of
    Nothing -> respond . Right $ InR Null
    Just result -> respond . Right . InL $ result

semanticTokensForTypeABT :: Type v Ann -> [SemanticTokenAbsolute]
semanticTokensForTypeABT abt = case ABT.out abt of
  ABT.Var {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Parameter ann
  ABT.Cycle r -> semanticTokensForTypeABT r
  ABT.Abs _v r -> semanticTokensForTypeABT r
  ABT.Tm f -> case f of
    Type.Ref {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Type ann
    Type.Arrow l r -> semanticTokensForTypeABT l <> semanticTokensForTypeABT r
    Type.Ann r _kind -> semanticTokensForTypeABT r
    Type.App l r -> semanticTokensForTypeABT l <> semanticTokensForTypeABT r
    Type.Effect l r -> semanticTokensForTypeABT l <> semanticTokensForTypeABT r
    Type.Effects effs -> foldMap semanticTokensForTypeABT effs
    Type.Forall r -> semanticTokensForTypeABT r
    Type.IntroOuter r -> semanticTokensForTypeABT r
  where
    ann = ABT.annotation abt

semanticTokensForTermABT :: Term v Ann -> [SemanticTokenAbsolute]
semanticTokensForTermABT abt = case ABT.out abt of
  ABT.Var {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Parameter ann
  ABT.Cycle r -> semanticTokensForTermABT r
  ABT.Abs _v r -> semanticTokensForTermABT r
  ABT.Tm f -> case f of
    Term.Int {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Number ann
    Term.Nat {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Number ann
    Term.Float {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Number ann
    Term.Boolean {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Keyword ann
    Term.Text {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_String ann
    Term.Char {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_String ann
    Term.Blank {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Macro ann
    -- Term.Ref {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Variable ann
    -- Ironically the lexer does a better job with these.
    Term.Ref {} -> []
    Term.Constructor {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Class ann
    Term.Request {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Interface ann
    Term.Handle l r -> semanticTokensForTermABT l <> semanticTokensForTermABT r
    Term.App l r -> semanticTokensForTermABT l <> semanticTokensForTermABT r
    Term.Ann r typ -> semanticTokensForTermABT r <> semanticTokensForTypeABT typ
    Term.List xs -> foldMap semanticTokensForTermABT xs
    Term.If c t e -> semanticTokensForTermABT c <> semanticTokensForTermABT t <> semanticTokensForTermABT e
    Term.And l r -> semanticTokensForTermABT l <> semanticTokensForTermABT r
    Term.Or l r -> semanticTokensForTermABT l <> semanticTokensForTermABT r
    Term.Lam r -> semanticTokensForTermABT r
    Term.LetRec _isTop bindings body -> foldMap semanticTokensForTermABT bindings <> semanticTokensForTermABT body
    Term.Let _isTop binding body -> semanticTokensForTermABT binding <> semanticTokensForTermABT body
    Term.Match lhs matches -> semanticTokensForTermABT lhs <> foldMap semanticTokensForMatchCase matches
    Term.TermLink {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Variable ann
    Term.TypeLink {} -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Type ann
  where
    ann = ABT.annotation abt

semanticTokensForMatchCase :: Term.MatchCase Ann (Term v Ann) -> [SemanticTokenAbsolute]
semanticTokensForMatchCase Term.MatchCase {matchPattern, matchGuard, matchBody} =
  semanticTokensForPatternABT matchPattern <> foldMap semanticTokensForTermABT matchGuard <> semanticTokensForTermABT matchBody

semanticTokensForPatternABT :: Pattern.Pattern Ann -> [SemanticTokenAbsolute]
semanticTokensForPatternABT = \case
  Pattern.Unbound loc -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Parameter loc
  Pattern.Var loc -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Parameter loc
  Pattern.Boolean loc _bool -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Keyword loc
  Pattern.Int loc _int -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Number loc
  Pattern.Nat loc _nat -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Number loc
  Pattern.Float loc _float -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_Number loc
  Pattern.Text loc _text -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_String loc
  Pattern.Char loc _cahr -> maybeToList $ mkSemanticTokenForAnn LSP.SemanticTokenTypes_String loc
  Pattern.Constructor loc _con pats -> maybeToList (mkSemanticTokenForAnn LSP.SemanticTokenTypes_Class loc) <> foldMap semanticTokensForPatternABT pats
  Pattern.As loc pat -> maybeToList (mkSemanticTokenForAnn LSP.SemanticTokenTypes_Parameter loc) <> semanticTokensForPatternABT pat
  Pattern.EffectPure loc pat -> maybeToList (mkSemanticTokenForAnn LSP.SemanticTokenTypes_Type loc) <> semanticTokensForPatternABT pat
  Pattern.EffectBind loc _con pats pat -> maybeToList (mkSemanticTokenForAnn LSP.SemanticTokenTypes_Class loc) <> foldMap semanticTokensForPatternABT (pat : pats)
  Pattern.SequenceLiteral _loc pats -> foldMap semanticTokensForPatternABT pats
  Pattern.SequenceOp _loc l _ r -> semanticTokensForPatternABT l <> semanticTokensForPatternABT r

semanticTokenForLexeme :: Lexer.Token Lexer.Lexeme -> Maybe SemanticTokenAbsolute
semanticTokenForLexeme token = do
  let Lexer.Token {payload, start, end} = token
  tokenType <- case payload of
    -- start of a block
    Lexer.Open {} -> Just SemanticTokenTypes_Keyword
    -- Separator
    Lexer.Semi {} -> Nothing
    -- end of a block
    Lexer.Close -> Just SemanticTokenTypes_Keyword
    -- reserved tokens such as `{`, `(`, `type`, `of`, etc
    Lexer.Reserved str ->
      if all Char.isAlphaNum str
        then Just SemanticTokenTypes_Keyword
        else Just SemanticTokenTypes_Operator
    -- text literals, `"foo bar"`
    Lexer.Textual {} -> Just SemanticTokenTypes_String
    -- character literals, `?X`
    Lexer.Character {} -> Just SemanticTokenTypes_String
    -- a (non-infix) identifier
    Lexer.WordyId {} -> Just SemanticTokenTypes_Variable
    -- an infix identifier
    Lexer.SymbolyId {} -> Just SemanticTokenTypes_Operator
    -- a typed hole or placeholder
    Lexer.Blank {} -> Just SemanticTokenTypes_Macro {- Not a perfect match, but might as well have a different color -}
    -- numeric literals
    Lexer.Numeric {} -> Just SemanticTokenTypes_Number
    -- bytes literals
    Lexer.Bytes {} -> Just SemanticTokenTypes_Number
    -- hash literals
    Lexer.Hash {} -> Just SemanticTokenTypes_Namespace {- Not a perfect match, but might as well have a different color -}
    Lexer.Err {} -> Nothing
  let lspStart = uToLspPos start
  let lspEnd = uToLspPos end
  pure $
    LSP.SemanticTokenAbsolute
      { _line = lspStart ^. line,
        _startChar = lspStart ^. character,
        _length = (lspEnd ^. character) - (lspStart ^. character),
        _tokenType = tokenType,
        _tokenModifiers = []
      }

mkSemanticTokenForAnn :: LSP.SemanticTokenTypes -> Ann -> Maybe LSP.SemanticTokenAbsolute
mkSemanticTokenForAnn tokenType ann = do
  lspRange <- Cv.annToRange ann
  let lspStart = lspRange ^. start
  let lspEnd = lspRange ^. end
  pure $
    LSP.SemanticTokenAbsolute
      { _line = lspStart ^. line,
        _startChar = lspStart ^. character,
        _length = (lspEnd ^. character) - (lspStart ^. character),
        _tokenType = tokenType,
        _tokenModifiers = []
      }
