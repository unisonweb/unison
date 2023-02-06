-- | Render Unison.Server.Doc as plain markdown, used in the LSP
module Unison.Doc.AsMarkdown (toMarkdown) where

import qualified CMarkGFM as M
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Lazy (runWriterT)
import qualified Data.Char as Char
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
import qualified Unison.Syntax.Name as Name (toText)
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

data IsFolded
  = IsFolded Bool [Markdown] [Markdown]
  | Disabled (Markdown)

foldedToHtml :: IsFolded -> Markdown
foldedToHtml isFolded =
  case isFolded of
    Disabled summary ->
      details_ attrs $ summary_ summary
    IsFolded isFolded summary details ->
      let attrsWithOpen =
            if isFolded
              then attrs
              else open_ "open" : attrs
       in details_ attrsWithOpen $ do
            summary_ [class_ "folded-content folded-summary"] $ sequence_ summary
            div_ [class_ "folded-content folded-details"] $ sequence_ details

foldedToHtmlSource :: Bool -> EmbeddedSource -> Markdown
foldedToHtmlSource isFolded source =
  case source of
    Builtin summary ->
      foldedToHtml
        [class_ "folded rich source"]
        ( Disabled
            ( div_
                [class_ "builtin-summary"]
                $ do
                  codeBlock [] $ Syntax.toHtml summary
                  badge $ do
                    span_ [] $ strong_ [] "Built-in"
                    span_ [] "provided by the Unison runtime"
            )
        )
    EmbeddedSource summary details ->
      foldedToHtml
        [class_ "folded rich source"]
        ( IsFolded
            isFolded
            [codeBlock [] $ Syntax.toHtml summary]
            [codeBlock [] $ Syntax.toHtml details]
        )

-- | Merge adjacent Word elements in a list to 1 element with a string of words
-- separated by space— useful for rendering to the dom without creating dom
-- elements for each and every word in the doc, but instead rely on textNodes
mergeWords :: Text -> [Doc] -> [Doc]
mergeWords sep = foldr merge_ []
  where
    merge_ :: Doc -> [Doc] -> [Doc]
    merge_ d acc =
      case (d, acc) of
        (Word w, Word w_ : rest) ->
          Word (w <> sep <> w_) : rest
        _ ->
          d : acc

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
type Markdown = M.Node

type MarkdownText = Text

data SideContent
  = FrontMatterContent (Map Text [Text])
  | TooltipContent (Markdown)

newtype FrontMatterData = FrontMatterData (Map Text [Text])

toMarkdown :: Map Referent Name -> Doc -> (FrontMatterData, Markdown, Markdown)
toMarkdown docNamesByRef document =
  ( FrontMatterData frontMatterContent,
    content,
    tooltips
  )
  where
    tooltips =
      foldl go mempty sideContent
      where
        go acc (FrontMatterContent _) = acc
        go acc (TooltipContent html) = acc <> html

    frontMatterContent =
      foldl go mempty sideContent
      where
        go acc (FrontMatterContent fm) = acc <> fm
        go acc (TooltipContent _) = acc

    (_ :: Markdown, (content, sideContent) :: (Markdown, Seq SideContent)) =
      runWriterT (evalStateT (toMarkdown_ 1 document) 0)

    toMarkdown_ ::
      forall m.
      (MonadState Int m, MonadWriter (Seq SideContent) m) =>
      Nat ->
      Doc ->
      m [Markdown]
    toMarkdown_ sectionLevel doc =
      let -- Make it simple to retain the sectionLevel when recurring.
          -- the Section variant increments it locally
          currentSectionLevelToMarkdown ::
            (MonadState Int m, MonadWriter (Seq SideContent) m) =>
            Doc ->
            m [Markdown]
          currentSectionLevelToMarkdown = toMarkdown_ sectionLevel

          sectionContentToMarkdown ::
            (MonadState Int m, MonadWriter (Seq SideContent) m) =>
            (Doc -> m [Markdown]) ->
            Doc ->
            m [Markdown]
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
                pure [paragraph contents]
              Paragraph [Column _] -> renderer doc_
              Paragraph _ -> renderer doc_
              _ ->
                renderer doc_
       in case doc of
            Tooltip triggerContent tooltipContent -> do
              tooltipNo <- State.get
              State.put (tooltipNo + 1)
              tooltip <- currentSectionLevelToMarkdown tooltipContent
              Writer.tell (Seq.fromList . fmap TooltipContent $ tooltip)
              currentSectionLevelToMarkdown triggerContent
            Word word -> pure [text word]
            Code contents -> do
              result <- currentSectionLevelToMarkdown contents
              pure [code result]
            CodeBlock lang contents -> do
              result <- currentSectionLevelToMarkdown contents
              pure [codeBlock lang result]
            Bold d -> do
              result <- currentSectionLevelToMarkdown d
              pure [bold result]
            Italic d -> do
              result <- currentSectionLevelToMarkdown d
              pure [italic result]
            Strikethrough d -> do
              result <- currentSectionLevelToMarkdown d
              pure [strikethrough result]
            Style {} -> pure []
            Anchor id' d -> do
              label <- nodeToMarkdownText <$> currentSectionLevelToMarkdown d
              pure [link id' label]
            Blockquote d -> do
              contents <- currentSectionLevelToMarkdown d
              pure [blockquote contents]
            Blankline ->
              pure [linebreak, linebreak]
            Linebreak ->
              pure [linebreak]
            SectionBreak ->
              pure [sectionBreak]
            Aside d -> do
              contents <- currentSectionLevelToMarkdown d
              pure [aside contents]
            Callout icon content -> do
              contents <- currentSectionLevelToMarkdown content
              pure [callout (text ico) contents]
              where
                (ico :: Text) =
                  case icon of
                    Just emoji ->
                      ( toText "" $ emoji
                      )
                    Nothing -> ("")
            Table rows -> do
              renderedRows <- traverse (traverse currentSectionLevelToMarkdown) rows
              pure [table renderedRows]
            -- TODO: test out fold behaviour
            Folded _isFolded _summary details -> do
              -- summary' <- currentSectionLevelToMarkdown summary
              details' <- currentSectionLevelToMarkdown details
              pure $ details'
            Paragraph docs ->
              case docs of
                [d] ->
                  currentSectionLevelToMarkdown d
                ds -> do
                  rendered <- foldMapM currentSectionLevelToMarkdown ds
                  pure [paragraph rendered]
            BulletedList items -> do
              rendered <- traverse currentSectionLevelToMarkdown items
              pure [bulletList rendered]
            NumberedList startNum items -> do
              rendered <- traverse currentSectionLevelToMarkdown items
              pure [numberedList startNum rendered]
            Section title docs -> do
              renderedTitle <- currentSectionLevelToMarkdown title
              body <- foldMapM (sectionContentToMarkdown (toMarkdown_ (sectionLevel + 1))) docs
              pure $ (heading sectionLevel renderedTitle : body)
            NamedLink label href ->
              case normalizeHref docNamesByRef href of
                Href h ->
                  -- Fragments (starting with a #) are links internal to the page
                  if Text.isPrefixOf "#" h
                    then a_ [class_ "named-link", href_ h] <$> currentSectionLevelToMarkdown label
                    else a_ [class_ "named-link", href_ h, rel_ "noopener", target_ "_blank"] <$> currentSectionLevelToMarkdown label
                DocLinkHref name ->
                  let href = "/" <> Text.replace "." "/" (Name.toText name) <> ".html"
                   in a_ [class_ "named-link doc-link", href_ href] <$> currentSectionLevelToMarkdown label
                ReferenceHref ref ->
                  span_ [class_ "named-link", data_ "ref" ref, data_ "ref-type" "term"] <$> currentSectionLevelToMarkdown label
                InvalidHref ->
                  span_ [class_ "named-link invalid-href"] <$> currentSectionLevelToMarkdown label
            Image altText src caption ->
              let altAttr =
                    [alt_ $ toText " " altText]

                  image =
                    img_ (altAttr ++ [src_ $ toText "" src])

                  imageWithCaption c = do
                    caption' <- currentSectionLevelToMarkdown c

                    pure $
                      div_ [class_ "image-with-caption"] $ do
                        image
                        div_ [class_ "caption"] caption'
               in maybe (pure image) imageWithCaption caption
            Special specialForm ->
              case specialForm of
                Source sources ->
                  let sources' =
                        mapMaybe
                          (fmap (foldedToHtmlSource False) . embeddedSource)
                          sources
                   in pure $ div_ [class_ "folded-sources"] $ sequence_ sources'
                FoldedSource sources ->
                  let sources' =
                        mapMaybe
                          (fmap (foldedToHtmlSource True) . embeddedSource)
                          sources
                   in pure $ div_ [class_ "folded-sources"] $ sequence_ sources'
                Example syntax ->
                  pure $ span_ [class_ "source rich example-inline"] $ inlineCode [] (Syntax.toHtml syntax)
                ExampleBlock syntax ->
                  pure $ div_ [class_ "source rich example"] $ codeBlock [] (Syntax.toHtml syntax)
                Link syntax ->
                  pure (inlineCode ["rich", "source"] $ Syntax.toHtml syntax)
                Signature signatures ->
                  pure
                    ( codeBlock
                        [class_ "rich source signatures"]
                        ( mapM_
                            (div_ [class_ "signature"] . Syntax.toHtml)
                            signatures
                        )
                    )
                SignatureInline sig ->
                  pure (inlineCode ["rich", "source", "signature-inline"] $ Syntax.toHtml sig)
                Eval source result ->
                  pure $
                    div_ [class_ "source rich eval"] $
                      codeBlock [] $
                        div_ [] $ do
                          Syntax.toHtml source
                          div_ [class_ "result"] $ do
                            "⧨"
                            div_ [] $ Syntax.toHtml result
                EvalInline source result ->
                  pure $
                    span_ [class_ "source rich eval-inline"] $
                      inlineCode [] $
                        span_ [] $ do
                          Syntax.toHtml source
                          span_ [class_ "result"] $ do
                            "⧨"
                            Syntax.toHtml result
                Video sources attrs ->
                  let source (MediaSource s Nothing) =
                        source_ [src_ s]
                      source (MediaSource s (Just m)) =
                        source_ [src_ s, type_ m]
                   in pure $ video_ attrs' $ mapM_ source sources
                Doc.FrontMatter fm -> do
                  Writer.tell (pure $ FrontMatterContent fm)
                  pure mempty
                LaTeXInline latex -> pure $ M.CODE_BLOCK "latex" (latex)
                Svg svg ->
                  pure $ iframe_ [class_ "embed svg", sandbox_ "true", srcdoc_ svg] $ sequence_ []
                Embed syntax ->
                  pure $ div_ [class_ "source rich embed"] $ codeBlock [] (Syntax.toHtml syntax)
                EmbedInline syntax ->
                  pure $ span_ [class_ "source rich embed-inline"] $ inlineCode [] (Syntax.toHtml syntax)
                RenderError (InvalidTerm err) -> pure $ Syntax.toHtml err
            Join docs ->
              span_ [class_ "join"] <$> renderSequence currentSectionLevelToMarkdown (mergeWords " " docs)
            UntitledSection docs ->
              section_ [] <$> renderSequence (sectionContentToHtml currentSectionLevelToMarkdown) docs
            Column docs ->
              ul_
                [class_ "column"]
                <$> renderSequence
                  (fmap (li_ []) . currentSectionLevelToMarkdown)
                  (mergeWords " " docs)
            Group content ->
              span_ [class_ "group"] <$> currentSectionLevelToMarkdown content

nodeToMarkdownText :: [M.Node] -> MarkdownText
nodeToMarkdownText = \case
  [] -> mempty
  [n] -> M.nodeToCommonmark markdownOptions Nothing n
  nodes -> M.nodeToCommonmark markdownOptions Nothing (M.Node Nothing M.PARAGRAPH nodes)

type BlockType = Text

codeBlock :: BlockType -> [Markdown] -> Markdown
codeBlock typ contents = M.Node Nothing (M.CODE_BLOCK typ $ nodeToMarkdownText contents) []

code :: [Markdown] -> Markdown
code contents = M.Node Nothing (M.CODE $ nodeToMarkdownText contents) []

text :: Text -> Markdown
text contents = M.Node Nothing (M.TEXT contents) []

bold :: [Markdown] -> Markdown
bold contents = M.Node Nothing M.STRONG contents

italic :: [Markdown] -> Markdown
italic contents = M.Node Nothing M.EMPH contents

strikethrough :: [Markdown] -> Markdown
strikethrough contents = M.Node Nothing M.STRIKETHROUGH contents

paragraph :: [Markdown] -> Markdown
paragraph contents = M.Node Nothing M.PARAGRAPH contents

link :: Text -> Text -> Markdown
link url label = M.Node Nothing (M.LINK url label) []

blockquote :: [Markdown] -> Markdown
blockquote contents = M.Node Nothing M.BLOCK_QUOTE contents

linebreak :: Markdown
linebreak = M.Node Nothing M.LINEBREAK []

sectionBreak :: Markdown
sectionBreak = M.Node Nothing M.THEMATIC_BREAK []

table :: [[[Markdown]]] -> Markdown
table rows =
  rows
    & (fmap . fmap) (M.Node Nothing M.TABLE_CELL)
    & fmap (M.Node Nothing M.TABLE_ROW)
    & M.Node Nothing (M.TABLE [M.NoAlignment])

aside :: [Markdown] -> Markdown
aside = blockquote

callout :: Markdown -> [Markdown] -> Markdown
callout ico contents = blockquote $ ico : linebreak : contents

bulletList :: [[Markdown]] -> Markdown
bulletList items =
  items
    & fmap (M.Node Nothing M.ITEM)
    & M.Node Nothing (M.LIST listAttrs)
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

numberedList :: Word64 -> [[Markdown]] -> Markdown
numberedList startNum items =
  items
    & fmap (M.Node Nothing M.ITEM)
    & M.Node Nothing (M.LIST listAttrs)
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

heading :: Word64 -> [Markdown] -> Markdown
heading level contents = M.Node Nothing (M.HEADING $ fromIntegral level) contents

-- HELPERS --------------------------------------------------------------------

-- | Unison Doc allows endlessly deep section nesting with
-- titles, but HTML only supports to h1-h6, so we clamp
-- the sectionLevel when converting
h :: Nat -> Text -> (Markdown -> Markdown)
h n anchorId =
  case n of
    1 -> h1_ [id_ anchorId]
    2 -> h2_ [id_ anchorId]
    3 -> h3_ [id_ anchorId]
    4 -> h4_ [id_ anchorId]
    5 -> h5_ [id_ anchorId]
    6 -> h6_ [id_ anchorId]
    _ -> h6_ [id_ anchorId]

badge :: Markdown -> Markdown
badge =
  span_ [class_ "badge"]

textToClass :: Text -> Text
textToClass =
  Text.replace " " "__"
