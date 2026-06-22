{-# LANGUAGE RankNTypes #-}

-- | Pluggable frontend syntax ("dialects").
--
-- Unison stores all code as content-addressed AST; the surface syntax is purely a UI rendered on top of that AST. A
-- 'Dialect' bundles the two directions of that UI:
--
--   * a parser (text -> 'UnisonFile'), and
--   * a family of printers ('Term'/'Type'/'Decl' -> @Pretty SyntaxText@).
--
-- The default dialect is literally the existing Haskell-like functions ('Unison.Parsers.parseFile',
-- 'Unison.Syntax.TermPrinter.prettyBinding', etc.) — which is the proof that this abstraction is layered at the right
-- seam: every downstream consumer already speaks @Pretty SyntaxText@ and 'UnisonFile'.
--
-- This module lives in @parser-typechecker@ so that both @unison-cli@ and @unison-share-api@ can import it (the latter
-- so a future Unison Share GUI can pick a syntax per-request / per-definition via 'PrintDialect').
--
-- The print fields are exposed as a separate 'PrintDialect' so that print-only code (notably @unison-share-api@) can
-- thread the rendering functions around without also carrying a parser.
module Unison.Syntax.Dialect
  ( Dialect (..),
    PrintDialect (..),
    printDialect,
    defaultDialect,
    defaultPrintDialect,
    allDialects,
    dialectByName,
    allDialectNames,

    -- * Re-exports for consumers of the printer fields
    RenderUniqueTypeGuids (..),
    AccessorName,
    SyntaxText,
  )
where

import Control.Monad.Writer (Writer)
import Data.List qualified as List
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Reference (Reference, TypeReference)
import Unison.Referent (Referent)
import Unison.Syntax.DeclPrinter (AccessorName, RenderUniqueTypeGuids (..))
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Dialect.Curlison qualified as Curlison
import Unison.Syntax.Dialect.Curlison.Parser qualified as Curlison.Parser
import Unison.Syntax.Dialect.Pyson qualified as Pyson
import Unison.Syntax.Dialect.Pyson.Parser qualified as Pyson.Parser
import Unison.Syntax.Dialect.SExpr qualified as SExpr
import Unison.Syntax.Dialect.SExpr.Parser qualified as SExpr.Parser
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term2)
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile)
import Unison.Util.Pretty (Pretty)
import Unison.Util.SyntaxText qualified as S
import Unison.Var (Var)

-- | The structured, syntax-highlighted document type every printer produces; the common currency of the print seam.
type SyntaxText = S.SyntaxText' Reference

-- | The print half of a 'Dialect': how to render the core AST back to text. Carried on its own so that print-only
-- code (e.g. @unison-share-api@) need not also hold a parser.
--
-- Each field is given its own @forall@ (rank-N) so it is exactly as polymorphic as the underlying printer; this lets
-- the default dialect be a direct field-by-field assignment of the existing functions with no wrapping. (Note: do not
-- use @OverloadedRecordDot@ on these fields — @HasField@ is not derivable for higher-rank fields. Use the plain
-- selectors, e.g. @pdPrettyBinding pd ppe hq tm@.)
data PrintDialect = PrintDialect
  { -- | The dialect's name (e.g. @"unison"@). Lets callers special-case the default dialect to preserve its exact
    -- output where an alt dialect would render differently.
    pdName :: Text,
    -- | Render a bare term (e.g. the body of a @>@ watch expression).
    pdPrettyTerm ::
      forall v at ap a.
      (Var v) =>
      PrettyPrintEnv ->
      Term2 v at ap v a ->
      Pretty SyntaxText,
    pdPrettyBinding ::
      forall v at ap a.
      (Var v) =>
      PrettyPrintEnv ->
      HQ.HashQualified Name ->
      Term2 v at ap v a ->
      Pretty SyntaxText,
    pdPrettyBindingWithoutTypeSignature ::
      forall v at ap a.
      (Var v) =>
      PrettyPrintEnv ->
      HQ.HashQualified Name ->
      Term2 v at ap v a ->
      Pretty SyntaxText,
    -- | The @Writer (Set AccessorName)@ variant of decl printing, used by the file composer to discover which record
    -- accessors will be regenerated on re-parse (and therefore must not be printed as standalone terms).
    pdPrettyDeclW ::
      forall v a.
      (Var v) =>
      PrettyPrintEnvDecl ->
      RenderUniqueTypeGuids ->
      TypeReference ->
      HQ.HashQualified Name ->
      DD.Decl v a ->
      Writer (Set AccessorName) (Pretty SyntaxText),
    pdPrettyDecl ::
      forall v a.
      (Var v) =>
      PrettyPrintEnvDecl ->
      RenderUniqueTypeGuids ->
      TypeReference ->
      HQ.HashQualified Name ->
      DD.Decl v a ->
      Pretty SyntaxText,
    pdPrettyType ::
      forall v a.
      (Var v) =>
      PrettyPrintEnv ->
      Type v a ->
      Pretty SyntaxText,
    -- | Render a list of @name : type@ signatures (used by @find@ and the slurp\/add preview). One 'Pretty' per entry.
    pdPrettySignatures ::
      forall v a.
      (Var v) =>
      PrettyPrintEnv ->
      [(Referent, HQ.HashQualified Name, Type v a)] ->
      [Pretty SyntaxText],
    -- | Render a term as Doc syntax, if it is a Doc. 'Nothing' if the term is not a Doc.
    pdPrettyDoc2 ::
      forall v at ap a.
      (Var v) =>
      PrettyPrintEnv ->
      Term2 v at ap v a ->
      Maybe (Pretty SyntaxText)
  }

-- | A surface syntax: a parser plus a 'PrintDialect'.
data Dialect = Dialect
  { -- | Stable machine name, e.g. @"unison"@ or @"sexpr"@. This is what the @syntax.dialect@ config key and the
    -- @UNISON_SYNTAX@ environment variable are matched against.
    dialectName :: Text,
    parseFile ::
      forall m v.
      (Monad m, Var v) =>
      FilePath ->
      String ->
      Parser.ParsingEnv m ->
      m (Either (Parser.Err v) (UnisonFile v Ann)),
    dialectPrint :: PrintDialect
  }

-- | Project out the print half of a 'Dialect'.
printDialect :: Dialect -> PrintDialect
printDialect = dialectPrint

-- | The print fields of the default Haskell-like dialect: the existing printers, unchanged.
defaultPrintDialect :: PrintDialect
defaultPrintDialect =
  PrintDialect
    { pdName = "unison",
      pdPrettyTerm = TermPrinter.goPretty,
      pdPrettyBinding = TermPrinter.prettyBinding,
      pdPrettyBindingWithoutTypeSignature = TermPrinter.prettyBindingWithoutTypeSignature,
      pdPrettyDeclW = DeclPrinter.prettyDeclW,
      pdPrettyDecl = DeclPrinter.prettyDecl,
      pdPrettyType = TypePrinter.prettySyntax,
      pdPrettySignatures = TypePrinter.prettySignaturesST,
      pdPrettyDoc2 = TermPrinter.prettyDoc2
    }

-- | The default Haskell-like dialect: the existing parser and printers, unchanged. This is what UCM uses when no
-- @syntax.dialect@ config value and no @UNISON_SYNTAX@ override are set.
defaultDialect :: Dialect
defaultDialect =
  Dialect
    { dialectName = "unison",
      parseFile = Parsers.parseFile,
      dialectPrint = defaultPrintDialect
    }

-- | The Clojure-like / S-expression dialect.
--
-- Terms, types, data\/ability declarations, records (with field accessors), watch expressions and @{{ … }}@ docs all
-- print and parse in S-expr syntax (full round-trip through the 'Unison.Syntax.Surface' IR).
sexprDialect :: Dialect
sexprDialect =
  Dialect
    { dialectName = "sexpr",
      parseFile = SExpr.Parser.parseFile,
      dialectPrint =
        PrintDialect
          { pdName = "sexpr",
            pdPrettyTerm = SExpr.prettyTerm,
            pdPrettyBinding = SExpr.prettyBinding,
            pdPrettyBindingWithoutTypeSignature = SExpr.prettyBindingWithoutTypeSignature,
            pdPrettyDeclW = SExpr.prettyDeclW,
            pdPrettyDecl = SExpr.prettyDecl,
            pdPrettyType = SExpr.prettyType,
            pdPrettySignatures = SExpr.prettySignatures,
            pdPrettyDoc2 = SExpr.prettyDoc2
          }
    }

-- | The Curlison dialect. Like 'sexprDialect', everything (terms, types, decls, records, watches, docs)
-- round-trips through the 'Surface' IR; infix operators use precedence-driven minimal parentheses.
curlisonDialect :: Dialect
curlisonDialect =
  Dialect
    { dialectName = "curlison",
      parseFile = Curlison.Parser.parseFile,
      dialectPrint =
        PrintDialect
          { pdName = "curlison",
            pdPrettyTerm = Curlison.prettyTerm,
            pdPrettyBinding = Curlison.prettyBinding,
            pdPrettyBindingWithoutTypeSignature = Curlison.prettyBindingWithoutTypeSignature,
            pdPrettyDeclW = Curlison.prettyDeclW,
            pdPrettyDecl = Curlison.prettyDecl,
            pdPrettyType = Curlison.prettyType,
            pdPrettySignatures = Curlison.prettySignatures,
            pdPrettyDoc2 = Curlison.prettyDoc2
          }
    }

-- | The Pyson-like (indentation-significant) dialect.
pysonDialect :: Dialect
pysonDialect =
  Dialect
    { dialectName = "pyson",
      parseFile = Pyson.Parser.parseFile,
      dialectPrint =
        PrintDialect
          { pdName = "pyson",
            pdPrettyTerm = Pyson.prettyTerm,
            pdPrettyBinding = Pyson.prettyBinding,
            pdPrettyBindingWithoutTypeSignature = Pyson.prettyBindingWithoutTypeSignature,
            pdPrettyDeclW = Pyson.prettyDeclW,
            pdPrettyDecl = Pyson.prettyDecl,
            pdPrettyType = Pyson.prettyType,
            pdPrettySignatures = Pyson.prettySignatures,
            pdPrettyDoc2 = Pyson.prettyDoc2
          }
    }

-- | All registered dialects. New dialects are added here.
allDialects :: [Dialect]
allDialects =
  [ defaultDialect,
    sexprDialect,
    curlisonDialect,
    pysonDialect
  ]

-- | Look up a dialect by its 'dialectName'.
dialectByName :: Text -> Maybe Dialect
dialectByName n = List.find ((== n) . dialectName) allDialects

-- | The names of all registered dialects (for help text and error messages).
allDialectNames :: [Text]
allDialectNames = map dialectName allDialects
