{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}

module Unison.CommandLine.DisplayValues where

import Unison.Prelude

import Data.Foldable (foldrM)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Unison.ABT as ABT
import qualified Unison.Runtime.IOSource as DD
import qualified Unison.Builtin.Decls as DD
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DP
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import qualified Unison.ShortHash as SH
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import qualified Unison.Codebase.Editor.DisplayObject as DO
import qualified Unison.CommandLine.OutputMessages as OutputMessages
import qualified Unison.ConstructorType as CT

type Pretty = P.Pretty P.ColorText

displayTerm :: (Var v, Monad m)
           => PPE.PrettyPrintEnvDecl
           -> (Reference -> m (Maybe (Term v ())))
           -> (Referent -> m (Maybe (Type v ())))
           -> (Term v () -> m (Maybe (Term v ())))
           -> (Reference -> m (Maybe (DD.Decl v ())))
           -> Term v ()
           -> m Pretty
displayTerm pped terms typeOf eval types = \case
  tm@(Term.Apps' (Term.Constructor' typ _) _)
    | typ == DD.docRef             -> displayDoc pped terms typeOf eval types tm
    | typ == DD.doc2Ref            -> do
      -- Pretty.get (doc.formatConsole tm)
      let tm' = Term.app() (Term.ref() DD.prettyGetRef)
                           (Term.app() (Term.ref() DD.doc2FormatConsoleRef) tm)
      tm <- eval tm'
      case tm of
        Nothing -> pure $ errMsg tm'
        Just tm -> displayTerm pped terms typeOf eval types tm
    | typ == DD.prettyAnnotatedRef -> displayPretty pped terms typeOf eval types tm
  tm -> pure $ src tm
  where
    errMsg tm = P.fatalCallout $ P.lines [
      P.wrap $ "I couldn't render this document, because the"
            <> "rendering function produced an error when"
            <> "evaluating this expression:",
      "",
      P.indentN 2 $ src tm,
      "",
      P.wrap $ "Sadly, I don't know the error, but you can evaluate"
            <> "the above expression in a scratch file to see it."
      ]
    src tm = TP.pretty (PPE.suffixifiedPPE pped) tm

-- assume this is given a
-- Pretty.Annotated ann (Either SpecialForm ConsoleText)
displayPretty :: forall v m . (Var v, Monad m)
              => PPE.PrettyPrintEnvDecl
              -> (Reference -> m (Maybe (Term v ())))
              -> (Referent  -> m (Maybe (Type v ())))
              -> (Term v () -> m (Maybe (Term v ())))
              -> (Reference -> m (Maybe (DD.Decl v ())))
              -> Term v ()
              -> m Pretty
displayPretty pped terms typeOf eval types tm = go tm
  where
  go = \case
    DD.PrettyEmpty _ -> pure mempty
    DD.PrettyGroup _ p -> P.group <$> go p
    DD.PrettyLit _ (DD.EitherLeft' special) -> goSpecial special
    DD.PrettyLit _ (DD.EitherRight' consoleTxt) -> goConsole consoleTxt
    DD.PrettyWrap _ p -> P.wrap' (pure . P.lit) <$> go p
    DD.PrettyOrElse _ p1 p2 -> P.orElse <$> go p1 <*> go p2
    DD.PrettyIndent _ initial afterNl p -> do
      initial <- go initial
      afterNl <- go afterNl
      p <- go p
      pure $ initial <> P.indentAfterNewline afterNl p
    DD.PrettyAppend _ ps -> mconcat . toList <$> traverse go ps
    DD.PrettyTable _ rows ->
      let f :: Term v () -> [[Pretty]] -> m [[Pretty]]
          f (Term.List' (toList -> row)) rows = (:) <$> traverse go row <*> pure rows
          f _row rows = pure rows
      in  P.table <$> foldrM f [] rows
    tm -> displayTerm pped terms typeOf eval types tm

  goSrc es = do
    -- we ignore the annotations; but this could be extended later
    -- to do some ascii art rendering
    let tys = [ ref | DD.TupleTerm' [DD.EitherLeft' (Term.TypeLink' ref),_anns] <- toList es ]
        toRef (Term.Ref' r) = Just r
        toRef (Term.RequestOrCtor' r _) = Just r
        toRef _ = Nothing
        tms = [ ref | DD.TupleTerm' [DD.EitherRight' (DD.Doc2Term (toRef -> Just ref)),_anns] <- toList es ]
    typeMap <- let
      -- todo: populate the variable names / kind once BuiltinObject supports that
      go ref@(Reference.Builtin _) = pure (ref, DO.BuiltinObject)
      go ref = (ref,) <$> do
        decl <- types ref
        let missing = DO.MissingObject (SH.unsafeFromText $ Reference.toText ref)
        pure $ maybe missing DO.UserObject decl
      in Map.fromList <$> traverse go tys
    termMap <- let
      -- todo: populate the type signature once BuiltinObject supports that
      go ref@(Reference.Builtin _) = pure (ref, DO.BuiltinObject)
      go ref = (ref,) <$> do
        tm <- terms ref
        let missing = DO.MissingObject (SH.unsafeFromText $ Reference.toText ref)
        pure $ maybe missing DO.UserObject tm
      in Map.fromList <$> traverse go tms
    pure . P.indentN 4 $ OutputMessages.displayDefinitions' pped typeMap termMap

  goSpecial = \case

    DD.Doc2SpecialFormFoldedSource (Term.List' es) -> goSrc es

    -- Source [Either Link.Type Doc2.Term]
    DD.Doc2SpecialFormSource (Term.List' es) -> goSrc es

    -- Example Nat Doc2.Term
    -- Examples like `foo x y` are encoded as `Example 2 (_ x y -> foo)`, where
    -- 2 is the number of variables that should be dropped from the rendering.
    -- So this will render as `foo x y`.
    DD.Doc2SpecialFormExample n (DD.Doc2Example vs body) ->
      P.backticked <$> displayTerm pped terms typeOf eval types ex
      where ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body

    -- Link (Either Link.Type Doc2.Term)
    DD.Doc2SpecialFormLink e -> let
      ppe = PPE.suffixifiedPPE pped
      go = pure . P.underline . P.syntaxToColor . NP.prettyHashQualified
      in case e of
        DD.EitherLeft' (Term.TypeLink' ref) -> go $ PPE.typeName ppe ref
        DD.EitherRight' (Term.Ref' ref) -> go $ PPE.termName ppe (Referent.Ref ref)
        DD.EitherRight' (Term.Request' ref cid) ->
          go $ PPE.termName ppe (Referent.Con ref cid CT.Effect)
        DD.EitherRight' (Term.Constructor' ref cid) ->
          go $ PPE.termName ppe (Referent.Con ref cid CT.Data)
        _ -> P.red <$> displayTerm pped terms typeOf eval types e

    -- Signature [Doc2.Term]
    DD.Doc2SpecialFormSignature (Term.List' tms) ->
      let referents = [ r | DD.Doc2Term (toReferent -> Just r) <- toList tms ]
      in P.indentN 2 . P.sep "\n\n" <$> traverse goSignature referents

    -- InlineSignature Doc2.Term
    DD.Doc2SpecialFormInlineSignature (DD.Doc2Term tm) -> P.backticked <$> case toReferent tm of
      Just r -> goSignature r
      _ -> displayTerm pped terms typeOf eval types tm

    -- Eval Doc2.Term
    DD.Doc2SpecialFormEval (DD.Doc2Term tm) -> eval tm >>= \case
      Nothing -> do
        p <- displayTerm pped terms typeOf eval types tm
        pure . P.indentN 4 $ P.lines [p, "⧨", P.red "🆘  An error occured during evaluation"]
      Just result -> do
        p1 <- displayTerm pped terms typeOf eval types tm
        p2 <- displayTerm pped terms typeOf eval types result
        pure . P.indentN 4 $ P.lines [p1, "⧨", P.green p2]

    -- InlineEval Doc2.Term
    DD.Doc2SpecialFormInlineEval (DD.Doc2Term tm) -> eval tm >>= \case
      Nothing -> pure . P.backticked . P.red $ "🆘  An error occurred during evaluation"
      Just result -> displayTerm pped terms typeOf eval types result

    -- Embed Any
    DD.Doc2SpecialFormEmbed (Term.App' _ any) ->
      displayTerm pped terms typeOf eval types any <&> \p ->
        P.indentN 2 $ "\n" <> "{{ embed {{" <> p <> "}} }}" <> "\n"

    -- InlineEmbed Any
    DD.Doc2SpecialFormInlineEmbed any ->
      displayTerm pped terms typeOf eval types any <&> \p ->
        "{{ embed {{" <> p <> "}} }}"

    tm -> P.red <$> displayTerm pped terms typeOf eval types tm

  toReferent tm = case tm of
    Term.Ref' r -> Just (Referent.Ref r)
    Term.Constructor' r cid -> Just (Referent.Con r cid CT.Data)
    Term.Request' r cid -> Just (Referent.Con r cid CT.Effect)
    _ -> Nothing

  goSignature r = typeOf r >>= \case
    Nothing -> pure $ termName (PPE.unsuffixifiedPPE pped) r
    Just typ -> pure . P.group $
      TypePrinter.prettySignatures
        (PPE.suffixifiedPPE pped)
        [(PPE.termName (PPE.unsuffixifiedPPE pped) r, typ)]

  goColor c = case c of
    DD.AnsiColorBlack -> P.black
    DD.AnsiColorRed -> P.red
    DD.AnsiColorGreen -> P.green
    DD.AnsiColorYellow -> P.yellow
    DD.AnsiColorBlue -> P.blue
    DD.AnsiColorMagenta -> P.purple
    DD.AnsiColorCyan -> P.cyan
    DD.AnsiColorWhite -> P.white
    DD.AnsiColorBrightBlack -> P.hiBlack
    DD.AnsiColorBrightRed -> P.hiRed
    DD.AnsiColorBrightGreen -> P.hiGreen
    DD.AnsiColorBrightYellow -> P.hiYellow
    DD.AnsiColorBrightBlue -> P.hiBlue
    DD.AnsiColorBrightMagenta -> P.hiPurple
    DD.AnsiColorBrightCyan -> P.hiCyan
    DD.AnsiColorBrightWhite -> P.hiWhite
    _ -> id

  goConsole = \case
    DD.ConsoleTextPlain (Term.Text' txt) -> pure $ P.text txt
    DD.ConsoleTextForeground color txt -> goColor color <$> goConsole txt
    DD.ConsoleTextBackground color txt -> do
      txt <- goConsole txt
      color <- pure $ goColor color
      pure $ P.background color txt
    DD.ConsoleTextBold txt -> P.bold <$> goConsole txt
    DD.ConsoleTextUnderline txt -> P.underline <$> goConsole txt
    DD.ConsoleTextInvert txt -> P.invert <$> goConsole txt
    tm -> displayTerm pped terms typeOf eval types tm

-- pattern DocBlob txt <- Term.App' (Term.Constructor' DocRef DocBlobId) (Term.Text' txt)

displayDoc :: forall v m . (Var v, Monad m)
           => PPE.PrettyPrintEnvDecl
           -> (Reference -> m (Maybe (Term v ())))
           -> (Referent  -> m (Maybe (Type v ())))
           -> (Term v () -> m (Maybe (Term v ())))
           -> (Reference -> m (Maybe (DD.Decl v ())))
           -> Term v ()
           -> m Pretty
displayDoc pped terms typeOf evaluated types = go
  where
  go (DD.DocJoin docs) = fold <$> traverse go docs
  go (DD.DocBlob txt) = pure $ P.paragraphyText txt
  go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) =
    pure $ P.underline (termName (PPE.suffixifiedPPE pped) r)
  go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) =
    pure $ P.underline (typeName (PPE.suffixifiedPPE pped) r)
  go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = prettyTerm terms r
  go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = prettyType r
  go (DD.DocSignature (Term.TermLink' r)) = prettySignature r
  go (DD.DocEvaluate (Term.TermLink' r)) = prettyEval (evaluated . Term.ref()) r
  go tm = pure $ TP.pretty (PPE.suffixifiedPPE pped) tm
  prettySignature r = typeOf r >>= \case
    Nothing -> pure $ termName (PPE.unsuffixifiedPPE pped) r
    Just typ -> pure . P.group $
      TypePrinter.prettySignatures
        (PPE.suffixifiedPPE pped)
        [(PPE.termName (PPE.unsuffixifiedPPE pped) r, typ)]
  prettyEval terms r = case r of
    Referent.Ref (Reference.Builtin n) -> pure . P.syntaxToColor $ P.text n
    Referent.Ref ref ->
      let ppe = PPE.declarationPPE pped ref
      in  terms ref >>= \case
            Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
            Just tm -> pure $ TP.pretty ppe tm
    Referent.Con r _ _ -> pure $ typeName (PPE.declarationPPE pped r) r
  prettyTerm terms r = case r of
    Referent.Ref (Reference.Builtin _) -> prettySignature r
    Referent.Ref ref -> let ppe = PPE.declarationPPE pped ref in terms ref >>= \case
      Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
      Just tm -> pure . P.syntaxToColor $ P.group $ TP.prettyBinding ppe (PPE.termName ppe r) tm
    Referent.Con r _ _ -> prettyType r
  prettyType r = let ppe = PPE.declarationPPE pped r in types r >>= \case
    Nothing -> pure $ "😶  Missing type source for: " <> typeName ppe r
    Just ty -> pure . P.syntaxToColor $ P.group $ DP.prettyDecl ppe r (PPE.typeName ppe r) ty

termName :: PPE.PrettyPrintEnv -> Referent -> Pretty
termName ppe r = P.syntaxToColor $
  NP.styleHashQualified'' (NP.fmt $ S.Referent r) name
  where name = PPE.termName ppe r

typeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
typeName ppe r = P.syntaxToColor $
  NP.styleHashQualified'' (NP.fmt $ S.Reference r) name
  where name = PPE.typeName ppe r
