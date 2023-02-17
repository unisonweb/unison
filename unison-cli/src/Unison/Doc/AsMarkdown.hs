-- | Render Unison.Server.Doc as plain markdown, used in the LSP
module Unison.Doc.AsMarkdown (toMarkdown) where

import qualified CMarkGFM as M
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Monad.Writer.Strict as Writer
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import qualified Unison.Debug as Debug
import Unison.Doc.Markdown.Types
import Unison.Name (Name)
import Unison.Prelude
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Server.Doc
import qualified Unison.Server.Doc as Doc
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax
import Unison.Util.Monoid (foldMapM)

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

embeddedSourceToMarkdown :: EmbeddedSource -> Markdown
embeddedSourceToMarkdown source =
  case source of
    Builtin summary ->
      codeBlockText "unison" (Syntax.toPlainText summary)
        <> text "Built-in provided by the Unison runtime"
    EmbeddedSource _summary details ->
      codeBlockText "unison" $ Syntax.toPlainText details

-- | Used when a contained block is expected to be raw text. E.g. inside a CodeBlock.
-- Other renderers may need to handle links and things, but for Markdown we don't.
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

toNodes :: Markdown -> [M.Node]
toNodes (Markdown ns) = toList ns

markdownNode :: M.Node -> Markdown
markdownNode = Markdown . Seq.singleton

instance Semigroup Markdown where
  -- We represent words in our AST, but CMark doesn't, so we have to manually add spaces when joining plain text nodes.
  Markdown (a Seq.:|> M.Node _ (M.TEXT txt) _) <> Markdown (M.Node _ (M.TEXT txt') _ Seq.:<| b) =
    Markdown (a <> Seq.singleton (M.Node Nothing (M.TEXT (Text.unwords [txt, txt'])) []) <> b)
  -- CMark doesn't add spaces in-between inline elements, we need to add them manually.
  Markdown a@(_ Seq.:|> x) <> Markdown b@(y Seq.:<| _)
    | isInlineNode x && isInlineNode y = Markdown (a <> Seq.singleton (M.Node Nothing (M.TEXT " ") []) <> b)
    | otherwise = Markdown (a <> b)
  Markdown a <> Markdown b = Markdown (a <> b)

isInlineNode :: M.Node -> Bool
isInlineNode (M.Node _ typ _) = case typ of
  M.DOCUMENT -> False
  M.THEMATIC_BREAK -> False
  M.PARAGRAPH -> False
  M.BLOCK_QUOTE -> False
  M.HTML_BLOCK {} -> False
  M.CUSTOM_BLOCK {} -> False
  M.CODE_BLOCK {} -> False
  M.HEADING {} -> False
  M.LIST {} -> False
  M.ITEM -> False
  M.TEXT {} -> True
  M.SOFTBREAK -> False
  M.LINEBREAK -> False
  M.HTML_INLINE {} -> True
  M.CUSTOM_INLINE {} -> True
  M.CODE {} -> True
  M.EMPH {} -> True
  M.STRONG {} -> True
  M.LINK {} -> True
  M.IMAGE {} -> False
  M.STRIKETHROUGH {} -> True
  M.TABLE {} -> False
  M.TABLE_ROW {} -> False
  M.TABLE_CELL {} -> False

instance Monoid Markdown where
  mempty = Markdown mempty

type MarkdownText = Text

newtype FrontMatterData = FrontMatterData (Map Text [Text])

data MarkdownEnv = MarkdownEnv
  { section :: Word64,
    -- CMark doesn't like nested paragraphs, so we don't make paragraph nodes if we're already
    -- in one.
    inParagraph :: Bool
  }

newtype MarkdownWriter = MarkdownWriter (Map Text [Text], Seq Markdown, Any)
  deriving newtype (Semigroup, Monoid)

-- | Tracks the current section level and accumulates side content
type MarkdownM = ReaderT MarkdownEnv (Writer MarkdownWriter)

toMarkdown :: Map Referent Name -> Doc -> (FrontMatterData, MarkdownText, [MarkdownText])
toMarkdown docNamesByRef doc = do
  ( FrontMatterData frontMatterContent,
    markdownToText . Debug.debug Debug.Temp "" $ document content,
    markdownToText <$> toList tooltips
    )
  where
    tooltips :: Seq Markdown
    env :: MarkdownEnv
    env = (MarkdownEnv {section = 1, inParagraph = False})
    content :: Markdown
    (content, MarkdownWriter (frontMatterContent, tooltips, _)) = runWriter (runReaderT (toMarkdown_ $ Debug.debug Debug.Temp "Raw Doc" doc) env)

    nonTextualElement = tell (MarkdownWriter (mempty, mempty, Any True))

    toMarkdown_ ::
      Doc ->
      MarkdownM Markdown
    toMarkdown_ doc =
      case doc of
        Tooltip triggerContent tooltipContent -> do
          tooltip <- toMarkdown_ tooltipContent
          lift $ Writer.tell (MarkdownWriter (mempty, Seq.singleton tooltip, mempty))
          toMarkdown_ triggerContent
        Word word -> do
          pure $ text word
        Code (Word txt) -> do
          pure $ inlineCodeText txt
        Code contents -> do
          pure $ inlineCodeText (toText contents)
        CodeBlock lang (Word txt) -> do
          nonTextualElement
          pure $ codeBlockText lang txt
        CodeBlock lang contents -> do
          nonTextualElement
          pure $ codeBlockText lang (toText contents)
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
          nonTextualElement
          contents <- toMarkdown_ d
          pure $ blockquote contents
        Blankline ->
          pure $ linebreak <> linebreak
        Linebreak ->
          pure linebreak
        SectionBreak -> do
          nonTextualElement
          pure sectionBreak
        Aside d -> do
          nonTextualElement
          contents <- toMarkdown_ d
          pure $ aside contents
        Callout icon content -> do
          nonTextualElement
          contents <- toMarkdown_ content
          pure $ callout (text ico) contents
          where
            (ico :: Text) =
              case icon of
                Just emoji ->
                  ( toText $ emoji
                  )
                Nothing -> ("")
        Table rows -> do
          nonTextualElement
          renderedRows <- traverse (traverse toMarkdown_) rows
          pure $ table renderedRows
        Folded _isFolded _summary details -> do
          nonTextualElement
          details' <- toMarkdown_ details
          pure $ details'
        Paragraph docs -> do
          inParagraph <- asks inParagraph
          (rendered, MarkdownWriter (_, _, Any nonTextualElements)) <- local (\env -> env {inParagraph = True}) $ listen (foldMapM toMarkdown_ docs)
          pure $
            if inParagraph || nonTextualElements
              then rendered
              else paragraph rendered
        BulletedList items -> do
          nonTextualElement
          rendered <- traverse toMarkdown_ items
          pure $ bulletList rendered
        NumberedList startNum items -> do
          nonTextualElement
          rendered <- traverse toMarkdown_ items
          pure $ numberedList startNum rendered
        Section title docs -> do
          nonTextualElement
          sectionLevel <- asks section
          let renderedTitle = toText title
          body <- local (\env -> env {section = section env + 1}) $ foldMapM toMarkdown_ docs
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
          nonTextualElement
          renderedAltText <- markdownToText <$> toMarkdown_ altText
          renderedCaption <- traverse toMarkdown_ caption
          let srcText = toText src
          pure $ image srcText renderedAltText <> (fromMaybe mempty renderedCaption)
        Special specialForm -> do
          case specialForm of
            Source sources -> do
              nonTextualElement
              pure $ foldMap (foldMap embeddedSourceToMarkdown . embeddedSource) sources
            FoldedSource sources -> do
              nonTextualElement
              -- We can't fold in markdown
              pure $ foldMap (foldMap embeddedSourceToMarkdown . embeddedSource) sources
            Example syntax -> do
              pure $ inlineCodeText (Syntax.toPlainText syntax)
            ExampleBlock syntax -> do
              nonTextualElement
              pure $ codeBlockText "unison" (Syntax.toPlainText syntax)
            Link syntax -> do
              -- TODO: Is this correct?
              pure $ inlineCodeText (Syntax.toPlainText syntax)
            Signature signatures -> do
              nonTextualElement
              signatures
                & foldMap (codeBlockText "unison" . Syntax.toPlainText)
                & pure
            SignatureInline sig -> do
              nonTextualElement
              pure . inlineCodeText $ Syntax.toPlainText sig
            Eval source result -> do
              nonTextualElement
              pure $
                codeBlockText
                  "unison"
                  ( Text.unlines
                      [ Syntax.toPlainText source,
                        "⧨",
                        Syntax.toPlainText result
                      ]
                  )
            EvalInline source result -> do
              --  I'm not sure of a good way to express this 'inline' in markdown
              pure $
                codeBlockText "unison" $
                  Text.unlines
                    [ Syntax.toPlainText source,
                      "⧨",
                      Syntax.toPlainText result
                    ]
            Video sources _attrs -> do
              nonTextualElement
              case sources of
                [] -> pure mempty
                (MediaSource src _ : _) -> do
                  pure $ video src ""
            Doc.FrontMatter fm -> do
              nonTextualElement
              lift $ Writer.tell $ MarkdownWriter (fm, mempty, mempty)
              pure mempty
            LaTeXInline latex -> do
              pure $ codeBlockText "latex" latex
            Svg src -> do
              nonTextualElement
              pure $ svg src
            Embed syntax -> do
              nonTextualElement
              pure $ codeBlockText "unison" (Syntax.toPlainText syntax)
            EmbedInline syntax -> do
              pure $ inlineCodeText (Syntax.toPlainText syntax)
            RenderError (InvalidTerm err) -> do
              nonTextualElement
              pure . text $ Syntax.toPlainText err
        Join docs -> do
          foldMapM toMarkdown_ docs
        UntitledSection docs -> do
          nonTextualElement
          foldMapM toMarkdown_ docs
        Column docs -> do
          foldMapM toMarkdown_ docs
        Group content -> do
          toMarkdown_ content

markdownToText :: Markdown -> MarkdownText
markdownToText (Markdown ns) = case toList ns of
  [] -> mempty
  [n] -> M.nodeToCommonmark markdownOptions Nothing n
  nodes -> M.nodeToCommonmark markdownOptions Nothing (M.Node Nothing M.DOCUMENT nodes)

type BlockType = Text

codeBlockText :: BlockType -> Text -> Markdown
codeBlockText typ contents =
  (markdownNode $ M.Node Nothing (M.CODE_BLOCK typ contents) [])
    <> linebreak
    <> linebreak

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
blockquote contents =
  markdownNode (M.Node Nothing M.BLOCK_QUOTE (toNodes contents))
    <> linebreak
    <> linebreak

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

heading :: Word64 -> Text -> Markdown
heading level contents = markdownNode $ M.Node Nothing (M.HEADING $ fromIntegral level) (toNodes $ text contents)
