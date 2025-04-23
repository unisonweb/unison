module Unison.Runtime.Exception
  ( module InternalError,
    RuntimeExn (BU, PE),
    bugMsg,
    die,
    dieP,
    exn,
    listErrors,
    tabulateErrors,
    peStr,
    prettyRuntimeExn,
    prettyRuntimeExnSansCtx,
  )
where

import Control.Exception (throw, throwIO)
import Data.Text (isPrefixOf)
import GHC.Stack (CallStack, callStack)
import Unison.Builtin.Decls qualified as RF
import Unison.Codebase.Runtime (Error)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Referent qualified as RF (pattern Ref)
import Unison.Runtime.Decompile (DecompError, DecompResult, decompile, renderDecompError)
import Unison.Runtime.InternalError as InternalError
import Unison.Runtime.Stack (Val)
import Unison.Symbol (Symbol)
import Unison.Syntax.NamePrinter (prettyHashQualified)
import Unison.Syntax.TermPrinter (pretty)
import Unison.Term qualified as Tm
import Unison.Util.Pretty as P

data RuntimeExn
  = -- | pretty exception
    PE CallStack [Word] (P.Pretty P.ColorText)
  | -- | __TODO__: What is `BU`? Boxed/Unboxed?
    BU [(Reference, Int)] Text Val

prettyRuntimeExn :: PrettyPrintEnv -> (Reference -> Reference) -> (Val -> DecompResult Symbol) -> RuntimeExn -> Pretty P.ColorText
prettyRuntimeExn ppe backmap decom = \case
  PE _ issues err ->
    P.fatalCallout $
      P.lines $
        [ P.wrap "You’ve encountered a Unison runtime error!",
          "",
          P.indentN 2 err,
          ""
        ]
          <> if null issues
            then [P.wrap "Please report it at https://github.com/unisonweb/unison/issues."]
            else
              [ P.wrap $
                  "See if one of these issues at https://github.com/unisonweb/unison/issues reflects what you’re seeing."
                    <> "If not, please open a new one:",
                P.bulleted $ P.string . show <$> issues
              ]
  BU tr0 nm c -> bugMsg ppe tr nm $ decom c
    where
      tr = first backmap <$> tr0

bugMsg ::
  PrettyPrintEnv ->
  [(Reference, Int)] ->
  Text ->
  (Set DecompError, Tm.Term Symbol ()) ->
  Pretty ColorText
bugMsg ppe tr name (errs, tm)
  | name == "blank expression" =
      P.callout icon . P.linesNonEmpty $
        [ P.wrap $ "I encountered a" <> P.red (P.text name) <> "with the following name/message:",
          "",
          P.indentN 2 $ pretty ppe tm,
          tabulateErrors errs,
          stackTrace ppe tr
        ]
  | "pattern match failure" `isPrefixOf` name =
      P.callout icon . P.linesNonEmpty $
        [ P.wrap $ "I've encountered a" <> P.red (P.text name) <> "while scrutinizing:",
          "",
          P.indentN 2 $ pretty ppe tm,
          "",
          P.wrap "This happens when calling a function that doesn't handle all possible inputs",
          tabulateErrors errs,
          stackTrace ppe tr
        ]
  | name == "builtin.raise" =
      P.callout icon . P.linesNonEmpty $
        [ P.wrap ("The program halted with an unhandled exception:"),
          "",
          P.indentN 2 $ pretty ppe tm,
          tabulateErrors errs,
          stackTrace ppe tr
        ]
  | name == "builtin.bug",
    RF.TupleTerm' [Tm.Text' msg, x] <- tm,
    "pattern match failure" `isPrefixOf` msg =
      P.callout icon . P.linesNonEmpty $
        [ P.wrap $ "I've encountered a" <> P.red (P.text msg) <> "while scrutinizing:",
          "",
          P.indentN 2 $ pretty ppe x,
          "",
          P.wrap "This happens when calling a function that doesn't handle all possible inputs",
          tabulateErrors errs,
          stackTrace ppe tr
        ]
  | otherwise =
      P.callout icon . P.linesNonEmpty $
        [ P.wrap $ "I've encountered a call to" <> P.red (P.text name) <> "with the following value:",
          "",
          P.indentN 2 $ pretty ppe tm,
          tabulateErrors errs,
          stackTrace ppe tr
        ]

stackTrace :: PrettyPrintEnv -> [(Reference, Int)] -> Pretty ColorText
stackTrace _ [] = mempty
stackTrace ppe tr = "\nStack trace:\n" <> P.indentN 2 (P.lines $ f <$> tr)
  where
    f (rf, n) = name <> count
      where
        count
          | n > 1 = " (" <> fromString (show n) <> " copies)"
          | otherwise = ""
        name =
          syntaxToColor
            . prettyHashQualified
            . PPE.termName ppe
            . RF.Ref
            $ rf

icon :: Pretty ColorText
icon = "💔💥"

listErrors :: Set DecompError -> [Error]
listErrors = fmap (P.indentN 2 . renderDecompError) . toList

tabulateErrors :: Set DecompError -> Error
tabulateErrors errs | null errs = mempty
tabulateErrors errs =
  P.indentN 2 . P.lines $
    ""
      : P.wrap "The following errors occured while decompiling:"
      : (listErrors errs)

prettyRuntimeExnSansCtx :: RuntimeExn -> Pretty P.ColorText
prettyRuntimeExnSansCtx = prettyRuntimeExn mempty id (decompile pure \_ _ -> Nothing)

-- | __TODO__: With GHC 9.10, this implementation can be moved to `displayException` on the `Exception` instance, and
--             this instance can be derived again (see haskell/core-libraries-committee#198).
instance Show RuntimeExn where
  show = P.toPlain 0 . prettyRuntimeExnSansCtx

instance Exception RuntimeExn

peStr :: (HasCallStack) => [Word] -> String -> RuntimeExn
peStr issues = PE callStack issues . P.lit . fromString
{-# INLINE peStr #-}

die :: (HasCallStack) => [Word] -> String -> IO a
die issues = throwIO . peStr issues
{-# INLINE die #-}

dieP :: (HasCallStack) => [Word] -> P.Pretty P.ColorText -> IO a
dieP issues = throwIO . PE callStack issues
{-# INLINE dieP #-}

exn :: (HasCallStack) => [Word] -> String -> a
exn issues = throw . peStr issues
{-# INLINE exn #-}
