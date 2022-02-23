{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Doc where

import Control.Lens ((^.), view)
import Control.Monad
-- import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable
import Data.Functor
-- import Data.Maybe (fromMaybe)
-- import Data.Text (Text)
-- import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
-- import GHC.Generics (Generic)
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.Syntax (SyntaxText)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import Unison.Prelude
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Builtin.Decls as Decls
import qualified Unison.Codebase.Editor.DisplayObject as DO
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DeclPrinter
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as DD
import qualified Unison.Server.Syntax as Syntax
import qualified Unison.ShortHash as SH
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TermPrinter
import qualified Unison.Type as Type
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import qualified Unison.Util.List as List

type Nat = Word64

type SSyntaxText = S.SyntaxText' Reference

data Doc
  = Word Text
  | Code Doc
  | CodeBlock Text Doc
  | Bold Doc
  | Italic Doc
  | Strikethrough Doc
  | Style Text Doc
  | Anchor Text Doc
  | Blockquote Doc
  | Blankline
  | Linebreak
  | SectionBreak
  | Tooltip Doc Doc
  | Aside Doc
  | Callout (Maybe Doc) Doc
  | Table [[Doc]]
  | Folded Bool Doc Doc
  | Paragraph [Doc]
  | BulletedList [Doc]
  | NumberedList Nat [Doc]
  | Section Doc [Doc]
  | NamedLink Doc Doc
  | Image Doc Doc (Maybe Doc)
  | Special SpecialForm
  | Join [Doc]
  | UntitledSection [Doc]
  | Column [Doc]
  | Group Doc
  deriving (Eq,Show,Generic)

type UnisonHash = Text

data Ref a = Term a | Type a deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

data MediaSource = MediaSource { mediaSourceUrl :: Text, mediaSourceMimeType :: Maybe Text } deriving (Eq, Show, Generic)

data SpecialForm
  = Source [Ref (UnisonHash, DisplayObject SyntaxText Src)]
  | FoldedSource [Ref (UnisonHash, DisplayObject SyntaxText Src)]
  | Example SyntaxText
  | ExampleBlock SyntaxText
  | Link SyntaxText
  | Signature [SyntaxText]
  | SignatureInline SyntaxText
  | Eval SyntaxText SyntaxText
  | EvalInline SyntaxText SyntaxText
  | Embed SyntaxText
  | EmbedInline SyntaxText
  | Video [MediaSource] (Map Text Text)
  | FrontMatter (Map Text [Text])
  deriving (Eq,Show,Generic)

-- `Src folded unfolded`
data Src = Src SyntaxText SyntaxText deriving (Eq,Show,Generic)

renderDoc :: forall v m . (Var v, Monad m)
          => PPE.PrettyPrintEnvDecl
          -> (Reference -> m (Maybe (Term v ())))
          -> (Referent -> m (Maybe (Type v ())))
          -> (Term v () -> m (Maybe (Term v ())))
          -> (Reference -> m (Maybe (DD.Decl v ())))
          -> Term v ()
          -> m Doc
renderDoc pped terms typeOf eval types tm = eval tm >>= \case
  Nothing -> pure $ Word "🆘 doc rendering failed during evaluation"
  Just tm -> go tm
  where
  go = \case
    DD.Doc2Word txt -> pure $ Word txt
    DD.Doc2Code d -> Code <$> go d
    DD.Doc2CodeBlock lang d -> CodeBlock lang <$> go d
    DD.Doc2Bold d -> Bold <$> go d
    DD.Doc2Italic d -> Italic <$> go d
    DD.Doc2Strikethrough d -> Strikethrough <$> go d
    DD.Doc2Style s d -> Style s <$> go d
    DD.Doc2Anchor id d -> Anchor id <$> go d
    DD.Doc2Blockquote d -> Blockquote <$> go d
    DD.Doc2Blankline -> pure Blankline
    DD.Doc2Linebreak -> pure Linebreak
    DD.Doc2SectionBreak -> pure SectionBreak
    DD.Doc2Tooltip d1 d2 -> Tooltip <$> go d1 <*> go d2
    DD.Doc2Aside d -> Aside <$> go d
    DD.Doc2Callout Decls.OptionalNone' d -> Callout Nothing <$> go d
    DD.Doc2Callout (Decls.OptionalSome' icon) d -> Callout <$> (Just <$> go icon) <*> go d
    DD.Doc2Table rows -> Table <$> traverse r rows
      where r (Term.List' ds) = traverse go (toList ds)
            r _ = pure [Word "🆘 invalid table"]
    DD.Doc2Folded isFolded d d2 -> Folded isFolded <$> go d <*> go d2
    DD.Doc2Paragraph ds -> Paragraph <$> traverse go ds
    DD.Doc2BulletedList ds -> BulletedList <$> traverse go ds
    DD.Doc2NumberedList n ds -> NumberedList n <$> traverse go ds
    DD.Doc2Section title ds -> Section <$> go title <*> traverse go ds
    DD.Doc2NamedLink d1 d2 -> NamedLink <$> go d1 <*> go d2
    DD.Doc2Image d1 d2 Decls.OptionalNone' -> Image <$> go d1 <*> go d2 <*> pure Nothing
    DD.Doc2Image d1 d2 (Decls.OptionalSome' d) -> Image <$> go d1 <*> go d2 <*> (Just <$> go d)
    DD.Doc2Special sf -> Special <$> goSpecial sf
    DD.Doc2Join ds -> Join <$> traverse go ds
    DD.Doc2UntitledSection ds -> UntitledSection <$> traverse go ds
    DD.Doc2Column ds -> Column <$> traverse go ds
    DD.Doc2Group d -> Group <$> go d
    wat -> pure . Word . Text.pack . P.toPlain (P.Width 80) . P.indent "🆘  "
                . TermPrinter.pretty (PPE.suffixifiedPPE pped) $ wat

  formatPretty = fmap Syntax.convertElement . P.render (P.Width 70)
  formatPrettyType ppe typ = formatPretty (TypePrinter.prettySyntax ppe typ)

  source :: Term v () -> m SyntaxText
  source tm = (pure . formatPretty . TermPrinter.prettyBlock' True (PPE.suffixifiedPPE pped)) tm

  goSignatures :: [Referent] -> m [P.Pretty SSyntaxText]
  goSignatures rs = runMaybeT (traverse (MaybeT . typeOf) rs) >>= \case
    Nothing -> pure ["🆘  codebase is missing type signature for these definitions"]
    Just types -> pure . fmap P.group $
      TypePrinter.prettySignaturesST
        (PPE.suffixifiedPPE pped)
        [ (r, PPE.termName (PPE.suffixifiedPPE pped) r, ty) | (r,ty) <- zip rs types]

  goSpecial :: Term v () -> m SpecialForm
  goSpecial = \case

    DD.Doc2SpecialFormFoldedSource (Term.List' es) -> FoldedSource <$> goSrc (toList es)

    -- Source [Either Link.Type Doc2.Term]
    DD.Doc2SpecialFormSource (Term.List' es) -> Source <$> goSrc (toList es)

    -- Example Nat Doc2.Term
    -- Examples like `foo x y` are encoded as `Example 2 (_ x y -> foo)`, where
    -- 2 is the number of variables that should be dropped from the rendering.
    -- So this will render as `foo x y`.
    DD.Doc2SpecialFormExample n (DD.Doc2Example vs body) ->
      Example <$> source ex
      where ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body

    DD.Doc2SpecialFormExampleBlock n (DD.Doc2Example vs body) ->
      ExampleBlock <$> source ex
      where ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body

    -- Link (Either Link.Type Doc2.Term)
    DD.Doc2SpecialFormLink e -> let
      ppe = PPE.suffixifiedPPE pped
      tm :: Referent -> P.Pretty SSyntaxText
      tm r = (NP.styleHashQualified'' (NP.fmt (S.TermReference r)) . PPE.termName ppe) r
      ty :: Reference -> P.Pretty SSyntaxText
      ty r = (NP.styleHashQualified'' (NP.fmt (S.TypeReference r)) . PPE.typeName ppe) r
      in Link <$> case e of
        DD.EitherLeft' (Term.TypeLink' r) -> (pure . formatPretty . ty) r
        DD.EitherRight' (DD.Doc2Term (Term.Referent' r)) -> (pure . formatPretty . tm) r
        _ -> source e

    DD.Doc2SpecialFormSignature (Term.List' tms) ->
      let rs = [ r | DD.Doc2Term (Term.Referent' r) <- toList tms ]
      in goSignatures rs <&> \s -> Signature (map formatPretty s)

    -- SignatureInline Doc2.Term
    DD.Doc2SpecialFormSignatureInline (DD.Doc2Term (Term.Referent' r)) ->
      goSignatures [r] <&> \s -> SignatureInline (formatPretty (P.lines s))

    -- Eval Doc2.Term
    DD.Doc2SpecialFormEval (DD.Doc2Term tm) -> eval tm >>= \case
      Nothing -> Eval <$> source tm <*> pure evalErrMsg
      Just result -> Eval <$> source tm <*> source result

    -- EvalInline Doc2.Term
    DD.Doc2SpecialFormEvalInline (DD.Doc2Term tm) -> eval tm >>= \case
      Nothing -> EvalInline <$> source tm <*> pure evalErrMsg
      Just result -> EvalInline <$> source tm <*> source result

    -- Embed Video
    DD.Doc2SpecialFormEmbedVideo sources config -> 
      pure $ trace "video pattern" $ Video sources' config' 
      where
        sources' = [ MediaSource url mimeType | DD.Doc2MediaSource (Term.Text' url) (maybeText -> mimeType) <- sources ]
        config' = Map.fromList [ (k,v) | Decls.TupleTerm' [Term.Text' k, Term.Text' v] <- config]
        maybeText (Term.App' _ (Term.Text' a)) = Just a 
        maybeText _ = Nothing

    -- Embed FrontMatter
    DD.Doc2SpecialFormEmbedFrontMatter frontMatter -> 
      pure $ trace "frontmatter pattern" $ FrontMatter frontMatter'
      where
        frontMatter' = List.multimap [ (k, v) | Decls.TupleTerm' [Term.Text' k, Term.Text' v] <- frontMatter]

    -- Embed Any
    DD.Doc2SpecialFormEmbed (Term.App' _ any) ->
      source any <&> \p -> Embed ("{{ embed {{" <> p <> "}} }}")

    -- EmbedInline Any
    DD.Doc2SpecialFormEmbedInline any ->
      source any <&> \p -> EmbedInline ("{{ embed {{" <> p <> "}} }}")

    tm -> source tm <&> \p -> Embed ("🆘  unable to render " <> p)

  evalErrMsg = "🆘  An error occured during evaluation"

  goSrc :: [Term v ()] -> m [Ref (UnisonHash, DisplayObject SyntaxText Src)]
  goSrc es = do
    let toRef (Term.Ref' r) = Set.singleton r
        toRef (Term.RequestOrCtor' r) = Set.singleton (r ^. ConstructorReference.reference_)
        toRef _ = mempty
        ppe = PPE.suffixifiedPPE pped
        goType :: Reference -> m (Ref (UnisonHash, DisplayObject SyntaxText Src))
        goType r@(Reference.Builtin _) =
          pure (Type (Reference.toText r, DO.BuiltinObject name))
          where name = formatPretty . NP.styleHashQualified (NP.fmt (S.TypeReference r))
                     . PPE.typeName ppe $ r
        goType r = Type . (Reference.toText r,) <$> do
          d <- types r
          case d of
            Nothing -> pure (DO.MissingObject (SH.unsafeFromText $ Reference.toText r))
            Just decl ->
              pure $ DO.UserObject (Src folded full)
              where
                full = formatPretty (DeclPrinter.prettyDecl pped r (PPE.typeName ppe r) decl)
                folded = formatPretty (DeclPrinter.prettyDeclHeader (PPE.typeName ppe r) decl)

        go :: (Set.Set Reference, [Ref (UnisonHash, DisplayObject SyntaxText Src)])
           -> Term v ()
           -> m (Set.Set Reference, [Ref (UnisonHash, DisplayObject SyntaxText Src)])
        go s1@(!seen,!acc) = \case
          -- we ignore the annotations; but this could be extended later
          DD.TupleTerm' [DD.EitherRight' (DD.Doc2Term tm), _anns] ->
            (seen <> toRef tm,) <$> acc'
            where
            acc' = case tm of
              Term.Ref' r | Set.notMember r seen -> (:acc) . Term . (Reference.toText r,) <$> case r of
                Reference.Builtin _ -> typeOf (Referent.Ref r) <&> \case
                  Nothing -> DO.BuiltinObject "🆘 missing type signature"
                  Just ty -> DO.BuiltinObject (formatPrettyType ppe ty)
                ref -> terms ref >>= \case
                  Nothing -> pure $ DO.MissingObject (SH.unsafeFromText $ Reference.toText ref)
                  Just tm -> do
                    typ <- fromMaybe (Type.builtin() "unknown") <$> typeOf (Referent.Ref ref)
                    let name = PPE.termName ppe (Referent.Ref ref)
                    let folded = formatPretty . P.lines
                               $ TypePrinter.prettySignaturesST ppe [(Referent.Ref ref, name, typ)]
                    let full tm@(Term.Ann' _ _) _ =
                          formatPretty (TermPrinter.prettyBinding ppe name tm)
                        full tm typ =
                          formatPretty (TermPrinter.prettyBinding ppe name (Term.ann() tm typ))
                    pure (DO.UserObject (Src folded (full tm typ)))
              Term.RequestOrCtor' (view ConstructorReference.reference_ -> r) | Set.notMember r seen -> (:acc) <$> goType r
              _ -> pure acc
          DD.TupleTerm' [DD.EitherLeft' (Term.TypeLink' ref), _anns]
            | Set.notMember ref seen
            -> (Set.insert ref seen,) . (:acc) <$> goType ref
          _ -> pure s1
    reverse . snd <$> foldM go mempty es
