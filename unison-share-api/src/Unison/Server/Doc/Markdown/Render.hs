-- | Render Unison.Server.Doc as plain markdown, used in the LSP
module Unison.Server.Doc.Markdown.Render (toMarkdown) where

import Control.Monad.Reader
import Data.Foldable
import qualified Data.Text as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Prelude
import Unison.Server.Doc
import qualified Unison.Server.Doc as Doc
import qualified Unison.Server.Doc.Markdown.Types as Md
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax
import Unison.Util.Monoid (foldMapM)

data EmbeddedSource
  = EmbeddedSource SyntaxText SyntaxText
  | Builtin SyntaxText

embeddedSource :: Ref (UnisonHash, DisplayObject SyntaxText Src) -> Maybe EmbeddedSource
embeddedSource ref =
  let embeddedSource' (_, displayObj) =
        case displayObj of
          BuiltinObject s -> Just (Builtin s)
          UserObject (Src sum det) -> Just (EmbeddedSource sum det)
          MissingObject _ -> Nothing
   in case ref of
        Term s -> embeddedSource' s
        Type s -> embeddedSource' s

normalizeHref :: [Md.Markdown] -> Doc -> MarkdownM [Md.Markdown]
normalizeHref label = \case
  Word w -> pure [Md.Link label w]
  Group d ->
    normalizeHref label d
  j@Join {} -> do
    let uri = toText j
    pure [Md.Link label uri]
  Special (Link {}) -> do
    -- We don't support cross-doc links in Markdown (yet)
    pure label
  _ -> pure label

embeddedSourceToMarkdown :: EmbeddedSource -> [Md.Markdown]
embeddedSourceToMarkdown source =
  case source of
    Builtin summary ->
      [ Md.CodeBlock "unison" (Syntax.toPlainText summary),
        Md.Txt "Built-in provided by the Unison runtime"
      ]
    EmbeddedSource _summary details ->
      [Md.CodeBlock "unison" $ Syntax.toPlainText details]

-- | Used when a contained block is expected to be raw text. E.g. inside a CodeBlock.
-- Other renderers may need to handle links and things in code blocks, but for Markdown we don't.
toText :: Doc -> Text
toText doc =
  case doc of
    Paragraph ds -> listToText ds <> "\n"
    Group d -> toText d
    Join ds -> listToText ds
    Bold d -> "**" <> toText d <> "** "
    Italic d -> "_" <> toText d <> "_ "
    Strikethrough d -> "~~" <> toText d <> "~~ "
    Blockquote d -> ">" <> toText d <> " "
    Section d ds ->
      Text.unlines
        [ "#" <> toText d,
          listToText ds
        ]
    UntitledSection ds -> listToText ds
    Column ds -> listToText ds
    Word w -> w <> " "
    Code code -> "`" <> toText code <> "` "
    CodeBlock lang code ->
      Text.unlines
        [ "```" <> lang,
          toText code,
          "```\n"
        ]
    Style {} -> ""
    Anchor {} -> ""
    Blankline -> "\n\n"
    Linebreak -> "\n"
    SectionBreak -> "---\n"
    Tooltip {} -> ""
    Aside {} -> ""
    Callout {} -> ""
    -- Most other things shouldn't appear anywhere inside links and such
    _ -> ""
  where
    listToText xs =
      xs
        & fmap toText
        & filter (not . Text.null)
        & Text.unwords

data MarkdownEnv = MarkdownEnv
  { section :: Word64
  }

-- | Tracks the current section level and accumulates side content
type MarkdownM = Reader MarkdownEnv

toMarkdown :: Doc -> [Md.Markdown]
toMarkdown doc = (runReader (toMarkdown_ doc) env)
  where
    env :: MarkdownEnv
    env = (MarkdownEnv {section = 1})

toMarkdown_ :: Doc -> MarkdownM [Md.Markdown]
toMarkdown_ doc =
  case doc of
    Tooltip {} ->
      -- We don't render tooltips in markdown for now
      pure mempty
    Word word -> do
      pure [Md.Txt word]
    Code (Word txt) -> do
      pure [Md.InlineCode txt]
    Code contents -> do
      pure [Md.InlineCode (toText contents)]
    CodeBlock lang (Word txt) -> do
      pure [Md.CodeBlock lang txt]
    CodeBlock lang contents -> do
      pure [Md.CodeBlock lang (toText contents)]
    Bold d -> do
      result <- toMarkdown_ d
      pure [Md.Strong result]
    Italic d -> do
      result <- toMarkdown_ d
      pure [Md.Italics result]
    Strikethrough d -> do
      result <- toMarkdown_ d
      pure [Md.Strikethrough result]
    Style {} -> pure mempty
    Anchor uri d -> do
      label <- toMarkdown_ d
      pure [Md.Link label uri]
    Blockquote d -> do
      contents <- toMarkdown_ d
      pure [Md.BlockQuote contents]
    Blankline ->
      pure [Md.Linebreak, Md.Linebreak]
    Linebreak ->
      pure [Md.Linebreak]
    SectionBreak -> do
      pure [Md.ThematicBreak]
    Aside d -> do
      contents <- toMarkdown_ d
      pure [Md.BlockQuote contents]
    Callout icon content -> do
      contents <- toMarkdown_ content
      pure [Md.BlockQuote $ [Md.Txt ico, Md.Linebreak] <> contents]
      where
        (ico :: Text) =
          case icon of
            Just emoji ->
              ( toText $ emoji
              )
            Nothing -> ("")
    Table rows -> do
      renderedRows <- traverse (traverse toMarkdown_) rows
      pure [Md.Table Nothing renderedRows]
    Folded _isFolded _summary details -> do
      -- We don't fold anything in Markdown
      toMarkdown_ details
    Paragraph docs -> do
      rendered <- for docs toMarkdown_
      pure $ fold rendered <> [Md.Linebreak]
    BulletedList items -> do
      rendered <- for items toMarkdown_
      pure [Md.UnorderedList rendered]
    NumberedList startNum items -> do
      rendered <- for items toMarkdown_
      pure [Md.OrderedList (fromIntegral startNum) rendered]
    Section title docs -> do
      sectionLevel <- asks section
      renderedTitle <- toMarkdown_ title
      body <- local (\env -> env {section = section env + 1}) $ foldMapM toMarkdown_ docs
      pure $ [Md.Heading (fromIntegral sectionLevel) renderedTitle] <> body
    NamedLink label url -> do
      renderedLabel <- toMarkdown_ label
      normalizeHref renderedLabel url
    Image altText src caption -> do
      renderedAltText <- toMarkdown_ altText
      renderedCaption <- traverse toMarkdown_ caption
      let srcText = toText src
      pure $ [Md.Image renderedAltText srcText] <> (fromMaybe mempty renderedCaption)
    Special specialForm -> do
      case specialForm of
        Source sources -> do
          pure $ foldMap (foldMap embeddedSourceToMarkdown . embeddedSource) sources
        FoldedSource sources -> do
          -- We can't fold in markdown
          pure $ foldMap (foldMap embeddedSourceToMarkdown . embeddedSource) sources
        Example syntax -> do
          pure [Md.InlineCode (Syntax.toPlainText syntax)]
        ExampleBlock syntax -> do
          pure [Md.CodeBlock "unison" (Syntax.toPlainText syntax)]
        Link syntax -> do
          pure [Md.InlineCode (Syntax.toPlainText syntax)]
        Signature signatures -> do
          signatures
            & foldMap (pure @[] . Md.CodeBlock "unison" . Syntax.toPlainText)
            & pure
        SignatureInline sig -> do
          pure [Md.InlineCode $ Syntax.toPlainText sig]
        Eval source result -> do
          pure
            [ Md.CodeBlock
                "unison"
                ( Text.unlines
                    [ Syntax.toPlainText source,
                      "⧨",
                      Syntax.toPlainText result
                    ]
                )
            ]
        EvalInline source result -> do
          --  I'm not sure of a good way to express this 'inline' in markdown
          pure
            [ Md.CodeBlock "unison" $
                Text.unlines
                  [ Syntax.toPlainText source,
                    "⧨",
                    Syntax.toPlainText result
                  ]
            ]
        Video sources _attrs -> do
          case sources of
            [] -> pure mempty
            (MediaSource src _ : _) -> do
              pure [Md.Image mempty src]
        Doc.FrontMatter {} -> pure mempty
        LaTeXInline latex -> do
          pure [Md.CodeBlock "latex" latex]
        Svg {} -> do pure [Md.Txt "{inline svg}"]
        Embed syntax -> do
          pure [Md.CodeBlock "unison" (Syntax.toPlainText syntax)]
        EmbedInline syntax -> do
          pure [Md.InlineCode (Syntax.toPlainText syntax)]
        RenderError (InvalidTerm err) -> do
          pure [Md.Txt $ Syntax.toPlainText err]
    Join docs -> do
      foldMapM toMarkdown_ docs
    UntitledSection docs -> do
      foldMapM toMarkdown_ docs
    Column docs -> do
      foldMapM toMarkdown_ docs
    Group content -> do
      toMarkdown_ content
