-- | Render Unison.Server.Doc as plain markdown, used in the LSP
module Unison.Doc.AsMarkdown (toMarkdown) where

import qualified CMarkGFM as M
import Control.Monad.Reader
import Control.Monad.Trans.Writer.CPS as Writer
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Name (Name)
import Unison.Prelude
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Server.Doc
import qualified Unison.Server.Doc as Doc
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax
import Unison.Util.Monoid (foldMapM)

markdownOptions :: [M.CMarkOption]
markdownOptions = [M.optSafe]

data NamedLinkHref
  = Href Text
  | DocLinkHref Name
  | ReferenceHref Text
  | InvalidHref

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

normalizeHref :: Map Referent Name -> Doc -> NamedLinkHref
normalizeHref docNamesByRef = go InvalidHref
  where
    go href doc =
      case doc of
        Word w ->
          case href of
            InvalidHref ->
              Href w
            Href h ->
              Href (h <> w)
            ReferenceHref _ ->
              href
            DocLinkHref _ ->
              href
        Group d_ ->
          go href d_
        Join ds ->
          foldl' go href ds
        Special (Link syntax) ->
          case Syntax.firstReference syntax of
            Just r ->
              -- Convert references to docs to names, so we can construct links
              -- matching the file structure being generated from all the docs
              case Referent.fromText r >>= flip Map.lookup docNamesByRef of
                Just n ->
                  DocLinkHref n
                Nothing ->
                  ReferenceHref r
            Nothing -> InvalidHref
        _ ->
          href

toMarkdownSource :: EmbeddedSource -> Markdown
toMarkdownSource source =
  case source of
    Builtin summary ->
      codeBlockText "unison" (Syntax.toPlainText summary)
        <> text "Built-in provided by the Unison runtime"
    EmbeddedSource _summary details -> codeBlockText "unison" $ Syntax.toPlainText details

-- | Merge down Doc to Text by merging Paragraphs and Words.
-- Used for things like extract an src of an image. I.e something that has to
-- be a Text and not a Doc
toText :: Text -> Doc -> Text
toText sep doc =
  case doc of
    Paragraph ds ->
      listToText ds
    Group d ->
      toText sep d
    Join ds ->
      listToText ds
    Bold d ->
      toText sep d
    Italic d ->
      toText sep d
    Strikethrough d ->
      toText sep d
    Blockquote d ->
      toText sep d
    Section d ds ->
      toText sep d <> sep <> listToText ds
    UntitledSection ds ->
      listToText ds
    Column ds ->
      listToText ds
    Word w ->
      w
    _ ->
      ""
  where
    isEmpty s =
      s == Text.empty

    listToText =
      Text.intercalate sep
        . filter (not . isEmpty)
        . map (toText sep)

-- | Markdown node
newtype Markdown = Markdown (Seq M.Node)

toNodes :: Markdown -> [M.Node]
toNodes (Markdown ns) = toList ns

markdownNode :: M.Node -> Markdown
markdownNode = Markdown . Seq.singleton

instance Semigroup Markdown where
  -- We represent words in our AST, but CMark doesn't, so we have to manually add spaces when
  -- joining plain text nodes.
  Markdown (a Seq.:|> M.Node _ (M.TEXT txt) _) <> Markdown (M.Node _ (M.TEXT txt') _ Seq.:<| b) =
    Markdown (a <> Seq.singleton (M.Node Nothing (M.TEXT (Text.unwords [txt, txt'])) []) <> b)
  Markdown a <> Markdown b = Markdown (a <> b)

instance Monoid Markdown where
  mempty = Markdown mempty

type MarkdownText = Text

data SideContent
  = FrontMatterContent (Map Text [Text])
  | TooltipContent Markdown

newtype FrontMatterData = FrontMatterData (Map Text [Text])

-- | Tracks the current section level and accumulates side content
type MarkdownM = ReaderT Word64 (Writer (Seq SideContent))

toMarkdown :: Map Referent Name -> Doc -> (FrontMatterData, MarkdownText, [MarkdownText])
toMarkdown docNamesByRef doc =
  ( FrontMatterData frontMatterContent,
    markdownToText $ document content,
    tooltips
  )
  where
    tooltips :: [MarkdownText]
    frontMatterContent :: Map Text [Text]
    (tooltips, frontMatterContent) =
      sideContent
        & foldMap \case
          FrontMatterContent map -> (mempty, map)
          TooltipContent tc -> ([markdownToText tc], mempty)

    content :: Markdown
    sideContent :: Seq SideContent
    (content, sideContent) = runWriter (runReaderT (toMarkdown_ doc) 1)

    toMarkdown_ ::
      Doc ->
      MarkdownM Markdown
    toMarkdown_ doc =
      let sectionContentToMarkdown ::
            (Doc -> MarkdownM Markdown) ->
            Doc ->
            MarkdownM Markdown
          sectionContentToMarkdown renderer doc_ =
            -- Block elements can't be children for <p> elements
            case doc_ of
              Paragraph [CodeBlock {}] -> renderer doc_
              Paragraph [Blockquote _] -> renderer doc_
              Paragraph [Blankline] -> renderer doc_
              Paragraph [SectionBreak] -> renderer doc_
              Paragraph [Callout {}] -> renderer doc_
              Paragraph [Table _] -> renderer doc_
              Paragraph [Folded {}] -> renderer doc_
              Paragraph [BulletedList _] -> renderer doc_
              Paragraph [NumberedList {}] -> renderer doc_
              -- Paragraph [Section _ _] -> renderer doc_
              Paragraph [Image {}] -> renderer doc_
              Paragraph [Special (Source _)] -> renderer doc_
              Paragraph [Special (FoldedSource _)] -> renderer doc_
              Paragraph [Special (ExampleBlock _)] -> renderer doc_
              Paragraph [Special (Signature _)] -> renderer doc_
              Paragraph [Special Eval {}] -> renderer doc_
              Paragraph [Special (Embed _)] -> renderer doc_
              Paragraph [UntitledSection ds] -> do
                contents <- foldMapM renderer ds
                pure $ paragraph contents
              Paragraph [Column _] -> renderer doc_
              Paragraph _ -> renderer doc_
              _ ->
                renderer doc_
       in case doc of
            Tooltip triggerContent tooltipContent -> do
              tooltip <- toMarkdown_ tooltipContent
              lift $ Writer.tell (Seq.singleton . TooltipContent $ tooltip)
              toMarkdown_ triggerContent
            Word word -> pure $ text word
            Code contents -> do
              pure $ inlineCodeText $ toText "" contents
            CodeBlock lang contents -> do
              result <- toMarkdown_ contents
              pure $ codeBlock lang result
            Bold d -> do
              result <- toMarkdown_ d
              pure $ bold result
            Italic d -> do
              result <- toMarkdown_ d
              pure $ italic result
            Strikethrough d -> do
              result <- toMarkdown_ d
              pure $ strikethrough result
            Style {} -> pure mempty
            Anchor id' d -> do
              label <- markdownToText <$> toMarkdown_ d
              pure $ link id' label
            Blockquote d -> do
              contents <- toMarkdown_ d
              pure $ blockquote contents
            Blankline ->
              pure $ linebreak <> linebreak
            Linebreak ->
              pure linebreak
            SectionBreak ->
              pure sectionBreak
            Aside d -> do
              contents <- toMarkdown_ d
              pure $ aside contents
            Callout icon content -> do
              contents <- toMarkdown_ content
              pure $ callout (text ico) contents
              where
                (ico :: Text) =
                  case icon of
                    Just emoji ->
                      ( toText "" $ emoji
                      )
                    Nothing -> ("")
            Table rows -> do
              renderedRows <- traverse (traverse toMarkdown_) rows
              pure $ table renderedRows
            -- TODO: test out fold behaviour
            Folded _isFolded _summary details -> do
              -- summary' <- toMarkdown_ summary
              details' <- toMarkdown_ details
              pure $ details'
            Paragraph docs ->
              case docs of
                [d] ->
                  toMarkdown_ d
                ds -> do
                  rendered <- foldMapM toMarkdown_ ds
                  pure $ paragraph rendered
            BulletedList items -> do
              rendered <- traverse toMarkdown_ items
              pure $ bulletList rendered
            NumberedList startNum items -> do
              rendered <- traverse toMarkdown_ items
              pure $ numberedList startNum rendered
            Section title docs -> do
              sectionLevel <- ask
              renderedTitle <- toMarkdown_ title
              body <- local (+ 1) $ foldMapM (sectionContentToMarkdown toMarkdown_) docs
              pure $ heading sectionLevel renderedTitle <> body
            NamedLink label url -> do
              renderedLabel <- toMarkdown_ label
              pure $ case normalizeHref docNamesByRef url of
                Href h -> link h (markdownToText renderedLabel)
                -- We don't currently support linking to other docs within markdown.
                DocLinkHref _name -> renderedLabel
                ReferenceHref _ref -> renderedLabel
                InvalidHref -> renderedLabel
            Image altText src caption -> do
              renderedAltText <- markdownToText <$> toMarkdown_ altText
              renderedCaption <- traverse toMarkdown_ caption
              let srcText = toText "" src
              pure $ image srcText renderedAltText <> (fromMaybe mempty renderedCaption)
            Special specialForm ->
              case specialForm of
                Source sources ->
                  pure $ foldMap (foldMap toMarkdownSource . embeddedSource) sources
                FoldedSource sources ->
                  -- We can't fold in markdown
                  pure $ foldMap (foldMap toMarkdownSource . embeddedSource) sources
                Example syntax ->
                  pure $ inlineCodeText (Syntax.toPlainText syntax)
                ExampleBlock syntax ->
                  pure $ codeBlockText "unison" (Syntax.toPlainText syntax)
                Link syntax ->
                  -- TODO: Is this correct?
                  pure $ inlineCodeText (Syntax.toPlainText syntax)
                Signature signatures ->
                  signatures
                    & foldMap (codeBlockText "unison " . Syntax.toPlainText)
                    & pure
                SignatureInline sig ->
                  pure . inlineCodeText $ Syntax.toPlainText sig
                Eval source result -> do
                  pure $
                    codeBlockText
                      "unison"
                      ( Text.unlines
                          [ Syntax.toPlainText source,
                            "⧨",
                            Syntax.toPlainText result
                          ]
                      )
                EvalInline source result ->
                  --  I'm not sure of a good way to express this 'inline' in markdown
                  pure $
                    codeBlockText "unison" $
                      Text.unlines
                        [ Syntax.toPlainText source,
                          "⧨",
                          Syntax.toPlainText result
                        ]
                Video sources _attrs ->
                  case sources of
                    [] -> pure mempty
                    (MediaSource src _ : _) -> do
                      pure $ video src ""
                Doc.FrontMatter fm -> do
                  lift $ Writer.tell (Seq.singleton $ FrontMatterContent fm)
                  pure mempty
                LaTeXInline latex -> pure $ codeBlockText "latex" latex
                Svg src ->
                  pure $ svg src
                Embed syntax ->
                  pure $ codeBlockText "unison" (Syntax.toPlainText syntax)
                EmbedInline syntax ->
                  pure $ inlineCodeText (Syntax.toPlainText syntax)
                RenderError (InvalidTerm err) ->
                  pure . text $ Syntax.toPlainText err
            Join docs -> foldMapM toMarkdown_ docs
            UntitledSection docs -> foldMapM (sectionContentToMarkdown toMarkdown_) docs
            Column docs -> foldMapM toMarkdown_ docs
            Group content -> toMarkdown_ content

markdownToText :: Markdown -> MarkdownText
markdownToText (Markdown ns) = case toList ns of
  [] -> mempty
  [n] -> M.nodeToCommonmark markdownOptions Nothing n
  nodes -> M.nodeToCommonmark markdownOptions Nothing (M.Node Nothing M.DOCUMENT nodes)

type BlockType = Text

codeBlock :: BlockType -> Markdown -> Markdown
codeBlock typ contents = markdownNode (M.Node Nothing (M.CODE_BLOCK typ $ markdownToText contents) [])

codeBlockText :: BlockType -> Text -> Markdown
codeBlockText typ contents = markdownNode $ M.Node Nothing (M.CODE_BLOCK typ contents) []

-- inlineCode :: Markdown -> Markdown
-- inlineCode contents = markdownNode $ M.Node Nothing (M.CODE $ markdownToText contents) []

inlineCodeText :: Text -> Markdown
inlineCodeText contents = markdownNode $ M.Node Nothing (M.CODE contents) []

text :: Text -> Markdown
text contents = markdownNode $ M.Node Nothing (M.TEXT contents) []

bold :: Markdown -> Markdown
bold contents = markdownNode $ M.Node Nothing M.STRONG (toNodes contents)

italic :: Markdown -> Markdown
italic contents = markdownNode $ M.Node Nothing M.EMPH (toNodes contents)

strikethrough :: Markdown -> Markdown
strikethrough contents = markdownNode $ M.Node Nothing M.STRIKETHROUGH (toNodes contents)

paragraph :: Markdown -> Markdown
paragraph contents = markdownNode $ M.Node Nothing M.PARAGRAPH (toNodes contents)

document :: Markdown -> Markdown
document contents = markdownNode $ M.Node Nothing M.DOCUMENT (toNodes contents)

link :: Text -> Text -> Markdown
link url label = markdownNode $ M.Node Nothing (M.LINK url label) []

image :: Text -> Text -> Markdown
image url label = markdownNode $ M.Node Nothing (M.IMAGE url label) []

svg :: Text -> Markdown
svg _src =
  -- We don't have a good way to render inline svgs in markdown.
  text "<svg>"

-- | TODO: check how this works
video :: Text -> Text -> Markdown
video url label = image url label

blockquote :: Markdown -> Markdown
blockquote contents = markdownNode $ M.Node Nothing M.BLOCK_QUOTE (toNodes contents)

linebreak :: Markdown
linebreak = markdownNode $ M.Node Nothing M.LINEBREAK []

sectionBreak :: Markdown
sectionBreak = markdownNode $ M.Node Nothing M.THEMATIC_BREAK []

table :: [[Markdown]] -> Markdown
table rows =
  rows
    & (fmap . fmap) (M.Node Nothing M.TABLE_CELL . toNodes)
    & fmap (M.Node Nothing M.TABLE_ROW)
    & M.Node Nothing (M.TABLE [M.NoAlignment])
    & markdownNode

aside :: Markdown -> Markdown
aside = blockquote

callout :: Markdown -> Markdown -> Markdown
callout ico contents = blockquote $ ico <> linebreak <> contents

bulletList :: [Markdown] -> Markdown
bulletList items =
  items
    & fmap (M.Node Nothing M.ITEM . toNodes)
    & M.Node Nothing (M.LIST listAttrs)
    & markdownNode
  where
    listAttrs =
      M.ListAttributes
        { listType = M.BULLET_LIST,
          -- Whether the list is 'tight', a.k.a. items are separated by a single newline,
          -- or 'loose', a.k.a. items are separated by two newlines.
          listTight = True,
          listStart = 1,
          listDelim = M.PERIOD_DELIM
        }

numberedList :: Word64 -> [Markdown] -> Markdown
numberedList startNum items =
  items
    & fmap (M.Node Nothing M.ITEM . toNodes)
    & M.Node Nothing (M.LIST listAttrs)
    & markdownNode
  where
    listAttrs =
      M.ListAttributes
        { listType = M.ORDERED_LIST,
          -- Whether the list is 'tight', a.k.a. items are separated by a single newline,
          -- or 'loose', a.k.a. items are separated by two newlines.
          listTight = True,
          listStart = fromIntegral startNum,
          listDelim = M.PERIOD_DELIM
        }

heading :: Word64 -> Markdown -> Markdown
heading level contents = markdownNode $ M.Node Nothing (M.HEADING $ fromIntegral level) (toNodes contents)
