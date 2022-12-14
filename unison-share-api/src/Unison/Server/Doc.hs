{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Doc where

import Control.Lens (view, (^.))
import Control.Monad
import Data.Aeson (ToJSON)
import Data.Foldable
import Data.Functor
import qualified Data.Map as Map
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Builtin.Decls as Decls
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import qualified Unison.Codebase.Editor.DisplayObject as DO
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.DataDeclaration as DD
import qualified Unison.LabeledDependency as LD
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as DD
import Unison.Server.Orphans ()
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.NamePrinter as NP
import qualified Unison.Syntax.TermPrinter as TermPrinter
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.List as List
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)

type Nat = Word64

type SSyntaxText = S.SyntaxText' Reference

type SrcRefs = Ref (UnisonHash, DisplayObject SyntaxText Src)

type Doc = DocG (RenderError SyntaxText) SrcRefs SyntaxText SyntaxText SyntaxText SyntaxText SyntaxText UnisonHash

type ExpandedDoc v = DocG (RenderError (Term v ())) (ExpandedSrc v) (Term v ()) (Referent, Type v ()) (DD.Decl v ()) (Either (Term v ()) LD.LabeledDependency) Builtin ()

data DocG err src trm typ decl ref builtin hash
  = Word Text
  | Code (DocG err src trm typ decl ref builtin hash)
  | CodeBlock Text (DocG err src trm typ decl ref builtin hash)
  | Bold (DocG err src trm typ decl ref builtin hash)
  | Italic (DocG err src trm typ decl ref builtin hash)
  | Strikethrough (DocG err src trm typ decl ref builtin hash)
  | Style Text (DocG err src trm typ decl ref builtin hash)
  | Anchor Text (DocG err src trm typ decl ref builtin hash)
  | Blockquote (DocG err src trm typ decl ref builtin hash)
  | Blankline
  | Linebreak
  | SectionBreak
  | Tooltip (DocG err src trm typ decl ref builtin hash) (DocG err src trm typ decl ref builtin hash)
  | Aside (DocG err src trm typ decl ref builtin hash)
  | Callout (Maybe (DocG err src trm typ decl ref builtin hash)) (DocG err src trm typ decl ref builtin hash)
  | Table [[(DocG err src trm typ decl ref builtin hash)]]
  | Folded Bool (DocG err src trm typ decl ref builtin hash) (DocG err src trm typ decl ref builtin hash)
  | Paragraph [(DocG err src trm typ decl ref builtin hash)]
  | BulletedList [(DocG err src trm typ decl ref builtin hash)]
  | NumberedList Nat [(DocG err src trm typ decl ref builtin hash)]
  | Section (DocG err src trm typ decl ref builtin hash) [(DocG err src trm typ decl ref builtin hash)]
  | NamedLink (DocG err src trm typ decl ref builtin hash) (DocG err src trm typ decl ref builtin hash)
  | Image (DocG err src trm typ decl ref builtin hash) (DocG err src trm typ decl ref builtin hash) (Maybe (DocG err src trm typ decl ref builtin hash))
  | Special (SpecialFormG err src trm typ decl ref builtin hash)
  | Join [(DocG err src trm typ decl ref builtin hash)]
  | UntitledSection [(DocG err src trm typ decl ref builtin hash)]
  | Column [(DocG err src trm typ decl ref builtin hash)]
  | Group (DocG err src trm typ decl ref builtin hash)
  | RenderError err
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

deriving anyclass instance ToSchema Doc

type UnisonHash = Text

data Ref a = Term a | Type a
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON)

instance ToSchema a => ToSchema (Ref a)

data MediaSource = MediaSource {mediaSourceUrl :: Text, mediaSourceMimeType :: Maybe Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

type Builtin = Text

type SpecialForm = SpecialFormG (RenderError SyntaxText) SrcRefs SyntaxText SyntaxText SyntaxText SyntaxText SyntaxText UnisonHash

type ExpandedSpecialForm v = SpecialFormG (RenderError (Term v ())) (ExpandedSrc v) (Term v ()) (Referent, Type v ()) (DD.Decl v ()) (Either (Term v ()) LD.LabeledDependency) Text ()

data SpecialFormG err src trm sigtyp decl ref builtin hash
  = Source [src]
  | FoldedSource [src]
  | Example trm
  | ExampleBlock trm
  | Link ref
  | Signature [sigtyp]
  | SignatureInline sigtyp
  | -- Result is Nothing if there was an Eval failure
    Eval trm (Maybe trm)
  | -- Result is Nothing if there was an Eval failure
    EvalInline trm (Maybe trm)
  | Embed trm
  | EmbedInline trm
  | Video [MediaSource] (Map Text Text)
  | FrontMatter (Map Text [Text])
  | SpecialRenderError err
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

deriving anyclass instance ToSchema SpecialForm

-- `Src folded unfolded`
data Src = Src SyntaxText SyntaxText
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

evalAndRenderDoc ::
  forall v m.
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term v ()))) ->
  (Referent -> m (Maybe (Type v ()))) ->
  (Term v () -> m (Maybe (Term v ()))) ->
  (Reference -> m (Maybe (DD.Decl v ()))) ->
  Term v () ->
  m Doc
evalAndRenderDoc pped terms typeOf eval types tm =
  eval tm >>= \case
    Nothing -> pure $ Word "🆘 doc rendering failed during evaluation"
    Just tm -> expandDoc terms typeOf eval types tm >>= renderDoc pped

renderDoc ::
  forall v m.
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  ExpandedDoc v ->
  m Doc
renderDoc pped doc = go doc
  where
    suffixifiedPPE = PPE.suffixifiedPPE pped
    go :: ExpandedDoc v -> m Doc
    go = \case
      Word txt -> pure $ Word txt
      Code d -> Code <$> go d
      CodeBlock lang d -> CodeBlock lang <$> go d
      Bold d -> Bold <$> go d
      Italic d -> Italic <$> go d
      Strikethrough d -> Strikethrough <$> go d
      Style s d -> Style s <$> go d
      Anchor a d -> Anchor a <$> go d
      Blockquote d -> Blockquote <$> go d
      Blankline -> pure Blankline
      Linebreak -> pure Linebreak
      SectionBreak -> pure SectionBreak
      Tooltip d1 d2 -> Tooltip <$> go d1 <*> go d2
      Aside d -> Aside <$> go d
      Callout d1 d2 -> Callout <$> traverse go d1 <*> go d2
      Table rows -> Table <$> traverse (traverse go) rows
      Folded folded d1 d2 -> Folded folded <$> go d1 <*> go d2
      Paragraph ds -> Paragraph <$> traverse go ds
      BulletedList ds -> BulletedList <$> traverse go ds
      NumberedList n ds -> NumberedList n <$> traverse go ds
      Section d ds -> Section <$> go d <*> traverse go ds
      NamedLink d1 d2 -> NamedLink <$> go d1 <*> go d2
      Image d1 d2 d3 -> Image <$> go d1 <*> go d2 <*> traverse go d3
      Special s -> Special <$> goSpecial s
      Join ds -> Join <$> traverse go ds
      UntitledSection ds -> UntitledSection <$> traverse go ds
      Column ds -> Column <$> traverse go ds
      Group d -> Group <$> go d
      RenderError (InvalidTerm trm) -> pure . Word . Text.pack . P.toPlain (P.Width 80) . P.indent "🆘  " . TermPrinter.pretty (PPE.suffixifiedPPE pped) $ trm

    formatPretty = fmap Syntax.convertElement . P.render (P.Width 70)

    formatPrettyType :: PPE.PrettyPrintEnv -> Type v a -> SyntaxText
    formatPrettyType ppe typ = formatPretty (TypePrinter.prettySyntax ppe typ)

    source :: Term v () -> m SyntaxText
    source tm = pure . formatPretty $ TermPrinter.prettyBlock' True (PPE.suffixifiedPPE pped) tm

    goSignatures :: [(Referent, Type v ())] -> m [P.Pretty SSyntaxText]
    goSignatures types =
      pure . fmap P.group $
        TypePrinter.prettySignaturesST
          (PPE.suffixifiedPPE pped)
          [(r, PPE.termName (PPE.suffixifiedPPE pped) r, ty) | (r, ty) <- types]

    goSpecial :: ExpandedSpecialForm v -> m SpecialForm
    goSpecial = \case
      Source srcs -> Source <$> goSrc srcs
      FoldedSource srcs -> FoldedSource <$> goSrc srcs
      Example trm -> Example <$> source trm
      ExampleBlock trm -> ExampleBlock <$> source trm
      Link ref ->
        let ppe = PPE.suffixifiedPPE pped
            tm :: Referent -> P.Pretty SSyntaxText
            tm r = (NP.styleHashQualified'' (NP.fmt (S.TermReference r)) . PPE.termName ppe) r
            ty :: Reference -> P.Pretty SSyntaxText
            ty r = (NP.styleHashQualified'' (NP.fmt (S.TypeReference r)) . PPE.typeName ppe) r
         in Link <$> case ref of
              Left trm -> source trm
              Right ld -> case ld of
                LD.TermReferent r -> (pure . formatPretty . tm) r
                LD.TypeReference r -> (pure . formatPretty . ty) r
      Signature rs -> goSignatures rs <&> \s -> Signature (map formatPretty s)
      SignatureInline r -> goSignatures [r] <&> \s -> SignatureInline (formatPretty (P.lines s))
      Eval trm result -> do
        renderedTrm <- source trm
        case result of
          Nothing -> pure $ Eval renderedTrm Nothing
          Just renderedResult -> Eval renderedTrm <$> (Just <$> source renderedResult)
      EvalInline trm result -> do
        renderedTrm <- source trm
        case result of
          Nothing -> pure $ EvalInline renderedTrm Nothing
          Just renderedResult -> EvalInline renderedTrm <$> (Just <$> source renderedResult)
      Embed any -> source any <&> \p -> Embed ("{{ embed {{" <> p <> "}} }}")
      EmbedInline any -> source any <&> \p -> EmbedInline ("{{ embed {{" <> p <> "}} }}")
      Video sources config -> pure $ Video sources config
      FrontMatter frontMatter -> pure $ FrontMatter frontMatter
      SpecialRenderError (InvalidTerm tm) -> source tm <&> \p -> Embed ("🆘  unable to render " <> p)

    goSrc :: [ExpandedSrc v] -> m [Ref (UnisonHash, DisplayObject SyntaxText Src)]
    goSrc srcs =
      srcs & foldMapM \case
        ExpandedSrcDecl srcDecl -> case srcDecl of
          MissingDecl r -> pure [(Type (Reference.toText r, DO.MissingObject (SH.unsafeFromText $ Reference.toText r)))]
          BuiltinDecl r _builtin -> do
            let name =
                  formatPretty . NP.styleHashQualified (NP.fmt (S.TypeReference r))
                    . PPE.typeName suffixifiedPPE
                    $ r
            pure [Type (Reference.toText r, DO.BuiltinObject name)]
            where

          FoundDecl r decl -> do
            pure $ [Type (Reference.toText r, DO.UserObject (Src folded full))]
            where
              full = formatPretty (DeclPrinter.prettyDecl pped r (PPE.typeName suffixifiedPPE r) decl)
              folded = formatPretty (DeclPrinter.prettyDeclHeader (PPE.typeName suffixifiedPPE r) decl)
        ExpandedSrcTerm srcTerm -> case srcTerm of
          MissingBuiltinTypeSig r -> pure [(Type (Reference.toText r, DO.BuiltinObject "🆘 missing type signature"))]
          BuiltinTypeSig r typ -> do
            pure $ [Type (Reference.toText r, DO.BuiltinObject (formatPrettyType suffixifiedPPE typ))]
          MissingTerm r -> pure [Term (Reference.toText r, DO.MissingObject (SH.unsafeFromText $ Reference.toText r))]
          FoundTerm ref typ tm -> do
            let name = PPE.termName suffixifiedPPE (Referent.Ref ref)
            let folded =
                  formatPretty . P.lines $
                    TypePrinter.prettySignaturesST suffixifiedPPE [(Referent.Ref ref, name, typ)]
            let full tm@(Term.Ann' _ _) _ =
                  formatPretty (TermPrinter.prettyBinding suffixifiedPPE name tm)
                full tm typ =
                  formatPretty (TermPrinter.prettyBinding suffixifiedPPE name (Term.ann () tm typ))
            pure [Term (Reference.toText ref, DO.UserObject (Src folded (full tm typ)))]

expandDoc ::
  forall v m.
  (Var v, Monad m) =>
  (Reference -> m (Maybe (Term v ()))) ->
  (Referent -> m (Maybe (Type v ()))) ->
  (Term v () -> m (Maybe (Term v ()))) ->
  (Reference -> m (Maybe (DD.Decl v ()))) ->
  Term v () ->
  m (ExpandedDoc v)
expandDoc terms typeOf eval types tm = go tm
  where
    go :: Term v () -> m (ExpandedDoc v)
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
        where
          r (Term.List' ds) = traverse go (toList ds)
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
      wat -> pure $ RenderError (InvalidTerm wat)
    -- pure . Word . Text.pack . P.toPlain (P.Width 80) . P.indent "🆘  "
    --   . TermPrinter.pretty (PPE.suffixifiedPPE pped)
    --   $ wat

    source :: Term v () -> m (Term v ())
    source = pure

    goSignatures :: [Referent] -> m [(Referent, Type v ())]
    goSignatures rs =
      runMaybeT (traverse (MaybeT . typeOf) rs) >>= \case
        Nothing -> error "🆘  codebase is missing type signature for these definitions"
        Just types -> pure (zip rs types)

    goSpecial :: Term v () -> m (ExpandedSpecialForm v)
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
        where
          ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body
      DD.Doc2SpecialFormExampleBlock n (DD.Doc2Example vs body) ->
        ExampleBlock <$> source ex
        where
          ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body

      -- Link (Either Link.Type Doc2.Term)
      DD.Doc2SpecialFormLink e ->
        let tm :: Referent -> (Either a LD.LabeledDependency)
            tm r = Right $ LD.TermReferent r
            ty :: Reference -> (Either a LD.LabeledDependency)
            ty r = Right $ LD.TypeReference r
         in Link <$> case e of
              DD.EitherLeft' (Term.TypeLink' r) -> pure $ ty r
              DD.EitherRight' (DD.Doc2Term t) ->
                case Term.etaNormalForm t of
                  Term.Referent' r -> pure $ tm r
                  x -> Left <$> source x
              _ -> Left <$> source e
      DD.Doc2SpecialFormSignature (Term.List' tms) ->
        let rs = [r | DD.Doc2Term (Term.Referent' r) <- toList tms]
         in goSignatures rs <&> \s -> Signature s
      -- SignatureInline Doc2.Term
      DD.Doc2SpecialFormSignatureInline (DD.Doc2Term (Term.Referent' r)) ->
        goSignatures [r] <&> \[s] -> SignatureInline s
      -- Eval Doc2.Term
      DD.Doc2SpecialFormEval (DD.Doc2Term tm) -> do
        result <- eval tm
        Eval <$> source tm <*> traverse source result
      -- EvalInline Doc2.Term
      DD.Doc2SpecialFormEvalInline (DD.Doc2Term tm) -> do
        result <- eval tm
        Eval <$> source tm <*> traverse source result
      -- Embed Video
      DD.Doc2SpecialFormEmbedVideo sources config ->
        pure $ Video sources' config'
        where
          sources' = [MediaSource url mimeType | DD.Doc2MediaSource (Term.Text' url) (maybeText -> mimeType) <- sources]
          config' = Map.fromList [(k, v) | Decls.TupleTerm' [Term.Text' k, Term.Text' v] <- config]
          maybeText (Term.App' _ (Term.Text' a)) = Just a
          maybeText _ = Nothing

      -- Embed FrontMatter
      DD.Doc2SpecialFormEmbedFrontMatter frontMatter ->
        pure $ FrontMatter frontMatter'
        where
          frontMatter' = List.multimap [(k, v) | Decls.TupleTerm' [Term.Text' k, Term.Text' v] <- frontMatter]

      -- Embed Any
      DD.Doc2SpecialFormEmbed (Term.App' _ any) ->
        source any <&> \p -> Embed p
      -- EmbedInline Any
      DD.Doc2SpecialFormEmbedInline any ->
        source any <&> \p -> EmbedInline p
      tm -> source tm <&> \p -> SpecialRenderError $ InvalidTerm p

    goSrc :: [Term v ()] -> m [ExpandedSrc v]
    goSrc es = do
      let toRef (Term.Ref' r) = Set.singleton r
          toRef (Term.RequestOrCtor' r) = Set.singleton (r ^. ConstructorReference.reference_)
          toRef _ = mempty
          goType :: Reference -> m (ExpandedSrc v)
          goType r@(Reference.Builtin builtin) =
            pure (ExpandedSrcDecl (BuiltinDecl r builtin))
          goType r = do
            d <- types r
            case d of
              Nothing -> pure (ExpandedSrcDecl $ MissingDecl r)
              Just decl ->
                pure $ ExpandedSrcDecl (FoundDecl r decl)

          go ::
            (Set.Set Reference, [ExpandedSrc v]) ->
            Term v () ->
            m (Set.Set Reference, [ExpandedSrc v])
          go s1@(!seen, !acc) = \case
            -- we ignore the annotations; but this could be extended later
            DD.TupleTerm' [DD.EitherRight' (DD.Doc2Term tm), _anns] ->
              (seen <> toRef tm,) <$> acc'
              where
                acc' = case tm of
                  Term.Ref' r
                    | Set.notMember r seen ->
                        (: acc) <$> case r of
                          Reference.Builtin _ ->
                            typeOf (Referent.Ref r) <&> \case
                              Nothing -> ExpandedSrcTerm (MissingBuiltinTypeSig r)
                              Just ty -> ExpandedSrcTerm (BuiltinTypeSig r ty)
                          ref ->
                            terms ref >>= \case
                              Nothing -> pure . ExpandedSrcTerm . MissingTerm $ ref
                              Just tm -> do
                                typ <- fromMaybe (Type.builtin () "unknown") <$> typeOf (Referent.Ref ref)
                                pure $ ExpandedSrcTerm (FoundTerm ref typ tm)
                  Term.RequestOrCtor' (view ConstructorReference.reference_ -> r) | Set.notMember r seen -> (: acc) <$> goType r
                  _ -> pure acc
            DD.TupleTerm' [DD.EitherLeft' (Term.TypeLink' ref), _anns]
              | Set.notMember ref seen ->
                  (Set.insert ref seen,) . (: acc) <$> goType ref
            _ -> pure s1
      reverse . snd <$> foldM go mempty es

data RenderError trm
  = InvalidTerm trm
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

deriving anyclass instance ToSchema trm => ToSchema (RenderError trm)

data ExpandedSrc v
  = ExpandedSrcDecl (ExpandedDecl v)
  | ExpandedSrcTerm (ExpandedTerm v)

data ExpandedDecl v
  = MissingDecl Reference
  | BuiltinDecl Reference Builtin
  | FoundDecl Reference (DD.Decl v ())

data ExpandedTerm v
  = MissingTerm Reference
  | BuiltinTypeSig Reference (Type v ())
  | MissingBuiltinTypeSig Reference
  | FoundTerm Reference (Type v ()) (Term v ())

dependencies :: Ord v => ExpandedDoc v -> Set LD.LabeledDependency
dependencies = \case
  -- Word txt -> pure $ Word txt
  -- Code d -> Code <$> go d
  -- CodeBlock lang d -> CodeBlock lang <$> go d
  -- Bold d -> Bold <$> go d
  -- Italic d -> Italic <$> go d
  -- Strikethrough d -> Strikethrough <$> go d
  -- Style s d -> Style s <$> go d
  -- Anchor a d -> Anchor a <$> go d
  -- Blockquote d -> Blockquote <$> go d
  -- Blankline -> pure Blankline
  -- Linebreak -> pure Linebreak
  -- SectionBreak -> pure SectionBreak
  -- Tooltip d1 d2 -> Tooltip <$> go d1 <*> go d2
  -- Aside d -> Aside <$> go d
  -- Callout d1 d2 -> Callout <$> traverse go d1 <*> go d2
  -- Table rows -> Table <$> traverse (traverse go) rows
  -- Folded folded d1 d2 -> Folded folded <$> go d1 <*> go d2
  -- Paragraph ds -> Paragraph <$> traverse go ds
  -- BulletedList ds -> BulletedList <$> traverse go ds
  -- NumberedList n ds -> NumberedList n <$> traverse go ds
  -- Section d ds -> Section <$> go d <*> traverse go ds
  -- NamedLink d1 d2 -> NamedLink <$> go d1 <*> go d2
  -- Image d1 d2 d3 -> Image <$> go d1 <*> go d2 <*> traverse go d3
  -- Special s -> Special <$> goSpecial s
  -- Join ds -> Join <$> traverse go ds
  -- UntitledSection ds -> UntitledSection <$> traverse go ds
  -- Column ds -> Column <$> traverse go ds
  -- Group d -> Group <$> go d
  -- RenderError (InvalidTerm trm) -> pure . Word . Text.pack . P.toPlain (P.Width 80) . P.indent "🆘  " . TermPrinter.pretty (PPE.suffixifiedPPE pped) $ trmm
  Word _txt -> mempty
  Code d -> dependencies d
  CodeBlock _lang d -> dependencies d
  Bold d -> dependencies d
  Italic d -> dependencies d
  Strikethrough d -> dependencies d
  Style _s d -> dependencies d
  Anchor _a d -> dependencies d
  Blockquote d -> dependencies d
  Blankline -> mempty
  Linebreak -> mempty
  SectionBreak -> mempty
  Tooltip d1 d2 -> dependencies d1 <> dependencies d2
  Aside d -> dependencies d
  Callout d1 d2 -> foldMap dependencies d1 <> dependencies d2
  Table rows -> foldMap (foldMap dependencies) rows
  Folded _folded d1 d2 -> dependencies d1 <> dependencies d2
  Paragraph ds -> foldMap dependencies ds
  BulletedList ds -> foldMap dependencies ds
  NumberedList _n ds -> foldMap dependencies ds
  Section d ds -> dependencies d <> foldMap dependencies ds
  NamedLink d1 d2 -> dependencies d1 <> dependencies d2
  Image d1 d2 d3 -> dependencies d1 <> dependencies d2 <> foldMap dependencies d3
  Special s -> dependenciesSpecial s
  Join ds -> foldMap dependencies ds
  UntitledSection ds -> foldMap dependencies ds
  Column ds -> foldMap dependencies ds
  Group d -> dependencies d
  RenderError {} -> mempty

-- | Determines all dependencies of a special form
dependenciesSpecial :: forall v. Ord v => ExpandedSpecialForm v -> Set LD.LabeledDependency
dependenciesSpecial = \case
  Source srcs -> srcDeps srcs
  FoldedSource srcs -> srcDeps srcs
  Example trm -> Term.labeledDependencies trm
  ExampleBlock trm -> Term.labeledDependencies trm
  Link ref -> either Term.labeledDependencies Set.singleton ref
  Signature sigtyps -> sigtypDeps sigtyps
  SignatureInline sig -> sigtypDeps [sig]
  Eval trm mayTrm -> Term.labeledDependencies trm <> foldMap Term.labeledDependencies mayTrm
  EvalInline trm mayTrm -> Term.labeledDependencies trm <> foldMap Term.labeledDependencies mayTrm
  Embed trm -> Term.labeledDependencies trm
  EmbedInline trm -> Term.labeledDependencies trm
  Video {} -> mempty
  FrontMatter {} -> mempty
  SpecialRenderError (InvalidTerm trm) -> Term.labeledDependencies trm
  where
    sigtypDeps :: [(Referent, Type v a)] -> Set LD.LabeledDependency
    sigtypDeps sigtyps =
      sigtyps & foldMap \(ref, typ) ->
        Set.singleton (LD.TermReferent ref) <> Type.labeledDependencies typ
    srcDeps :: [ExpandedSrc v] -> Set LD.LabeledDependency
    srcDeps srcs =
      srcs & foldMap \case
        ExpandedSrcDecl srcDecl -> case srcDecl of
          MissingDecl ref -> Set.singleton (LD.TypeReference ref)
          BuiltinDecl ref _ -> Set.singleton (LD.TypeReference ref)
          FoundDecl ref decl -> Set.singleton (LD.TypeReference ref) <> DD.labeledDeclDependencies decl
        ExpandedSrcTerm srcTerm -> case srcTerm of
          MissingTerm ref -> Set.singleton (LD.TermReference ref)
          BuiltinTypeSig ref _ -> Set.singleton (LD.TermReference ref)
          MissingBuiltinTypeSig ref -> Set.singleton (LD.TermReference ref)
          FoundTerm ref typ trm -> Set.singleton (LD.TermReference ref) <> Type.labeledDependencies typ <> Term.labeledDependencies trm
