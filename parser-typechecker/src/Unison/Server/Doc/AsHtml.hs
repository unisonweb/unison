{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Render Unison.Server.Doc and embedded source to Html
module Unison.Server.Doc.AsHtml where

import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer.Class as Writer
import Control.Monad.Writer.Lazy (runWriterT)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import qualified Lucid as L
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Server.Doc
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax

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

inlineCode :: [Text] -> Html () -> Html ()
inlineCode classNames =
  code_ [classes_ ("inline-code" : classNames)]

codeBlock :: [Attribute] -> Html () -> Html ()
codeBlock attrs =
  pre_ attrs . code_ []

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
  = IsFolded Bool [Html ()] [Html ()]
  | Disabled (Html ())

foldedToHtml :: [Attribute] -> IsFolded -> Html ()
foldedToHtml attrs isFolded =
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

foldedToHtmlSource :: Bool -> EmbeddedSource -> Html ()
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

toHtml :: Map Referent Name -> Doc -> Html ()
toHtml docNamesByRef document =
  article_ [class_ "unison-doc"] $ do
    content
    div_ [class_ "tooltips", style_ "display: none;"] $ sequence_ tooltips
  where
    (_ :: Html (), (content, tooltips) :: (Html (), Seq (Html ()))) =
      runWriterT (evalStateT (toHtml_ 1 document) 0)

    toHtml_ ::
      forall m.
      (MonadState Int m, MonadWriter (Seq (Html ())) m) =>
      Nat ->
      Doc ->
      m (Html ())
    toHtml_ sectionLevel doc =
      let -- Make it simple to retain the sectionLevel when recurring.
          -- the Section variant increments it locally
          currentSectionLevelToHtml ::
            (MonadState Int m, MonadWriter (Seq (Html ())) m) =>
            Doc ->
            m (Html ())
          currentSectionLevelToHtml =
            toHtml_ sectionLevel

          renderSequence :: (a -> m (Html ())) -> [a] -> m (Html ())
          renderSequence f xs =
            sequence_ <$> traverse f xs

          sectionContentToHtml ::
            (MonadState Int m, MonadWriter (Seq (Html ())) m) =>
            (Doc -> m (Html ())) ->
            Doc ->
            m (Html ())
          sectionContentToHtml renderer doc_ =
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
              Paragraph [UntitledSection ds] ->
                renderSequence (sectionContentToHtml renderer) ds
              Paragraph [Column _] -> renderer doc_
              Paragraph _ -> p_ [] <$> renderer doc_
              _ ->
                renderer doc_
       in case doc of
            Tooltip triggerContent tooltipContent -> do
              tooltipNo <- State.get
              let tooltipId = "tooltip-" <> (Text.pack . show $ tooltipNo)
              State.put (tooltipNo + 1)
              tooltip <-
                div_ [class_ "tooltip-content", id_ tooltipId]
                  <$> currentSectionLevelToHtml tooltipContent
              Writer.tell (pure tooltip)

              span_
                [ class_ "tooltip-trigger",
                  data_ "tooltip-content-id" tooltipId
                ]
                <$> currentSectionLevelToHtml triggerContent
            Word word ->
              pure $ span_ [class_ "word"] (L.toHtml word)
            Code code ->
              span_ [class_ "rich source inline-code"] . inlineCode [] <$> currentSectionLevelToHtml code
            CodeBlock lang code ->
              div_ [class_ $ "rich source code " <> textToClass lang] . codeBlock [] <$> currentSectionLevelToHtml code
            Bold d ->
              strong_ [] <$> currentSectionLevelToHtml d
            Italic d ->
              span_ [class_ "italic"] <$> currentSectionLevelToHtml d
            Strikethrough d ->
              span_ [class_ "strikethrough"] <$> currentSectionLevelToHtml d
            Style cssclass_ d ->
              div_ [class_ $ textToClass cssclass_] <$> currentSectionLevelToHtml d
            Anchor id' d ->
              a_ [id_ id', href_ $ "#" <> id'] <$> currentSectionLevelToHtml d
            Blockquote d ->
              blockquote_ [] <$> currentSectionLevelToHtml d
            Blankline ->
              pure
                ( div_ [] $ do
                    br_ []
                    br_ []
                )
            Linebreak ->
              pure $ br_ []
            SectionBreak ->
              pure $ hr_ []
            Aside d ->
              span_
                [class_ "aside-anchor"]
                . aside_ []
                <$> currentSectionLevelToHtml d
            Callout icon content -> do
              callout <- currentSectionLevelToHtml content
              pure $
                div_ [cls] $ do
                  ico
                  div_ [class_ "callout-content"] callout
              where
                (cls :: Attribute, ico :: Html ()) =
                  case icon of
                    Just emoji ->
                      ( class_ "callout callout-with-icon",
                        div_ [class_ "callout-icon"] $ L.toHtml . toText "" $ emoji
                      )
                    Nothing ->
                      (class_ "callout", "")
            Table rows ->
              let cellToHtml c =
                    td_ [] <$> currentSectionLevelToHtml c

                  rowToHtml cells =
                    tr_ [] <$> renderSequence cellToHtml (mergeWords " " cells)

                  rows_ =
                    renderSequence rowToHtml rows
               in table_ [] . tbody_ [] <$> rows_
            Folded isFolded summary details -> do
              summary' <- currentSectionLevelToHtml summary
              details' <- currentSectionLevelToHtml details

              pure $
                foldedToHtml [class_ "folded"] $
                  IsFolded
                    isFolded
                    [summary']
                    -- We include the summary in the details slot to make it
                    -- symmetric with code folding, which currently always
                    -- includes the type signature in the details portion
                    [div_ [] summary', details']
            Paragraph docs ->
              case docs of
                [d] ->
                  currentSectionLevelToHtml d
                ds ->
                  span_ [class_ "span"] <$> renderSequence currentSectionLevelToHtml (mergeWords " " ds)
            BulletedList items ->
              let itemToHtml i =
                    li_ [] <$> currentSectionLevelToHtml i
               in ul_ [] <$> renderSequence itemToHtml (mergeWords " " items)
            NumberedList startNum items ->
              let itemToHtml i =
                    li_ [] <$> currentSectionLevelToHtml i
               in ol_ [start_ $ Text.pack $ show startNum]
                    <$> renderSequence itemToHtml (mergeWords " " items)
            Section title docs -> do
              titleEl <-
                h sectionLevel <$> currentSectionLevelToHtml title

              docs' <- renderSequence (sectionContentToHtml (toHtml_ (sectionLevel + 1))) docs

              pure $ section_ [] $ titleEl *> docs'
            NamedLink label href ->
              case normalizeHref docNamesByRef href of
                Href h ->
                  -- Fragments (starting with a #) are links internal to the page
                  if Text.isPrefixOf "#" h
                    then a_ [class_ "named-link", href_ h] <$> currentSectionLevelToHtml label
                    else a_ [class_ "named-link", href_ h, rel_ "noopener", target_ "_blank"] <$> currentSectionLevelToHtml label
                DocLinkHref name ->
                  let href = "/" <> Text.replace "." "/" (Name.toText name) <> ".html"
                   in a_ [class_ "named-link doc-link", href_ href] <$> currentSectionLevelToHtml label
                ReferenceHref ref ->
                  span_ [class_ "named-link", data_ "ref" ref, data_ "ref-type" "term"] <$> currentSectionLevelToHtml label
                InvalidHref ->
                  span_ [class_ "named-link invalid-href"] <$> currentSectionLevelToHtml label
            Image altText src caption ->
              let altAttr =
                    [alt_ $ toText " " altText]

                  image =
                    img_ (altAttr ++ [src_ $ toText "" src])

                  imageWithCaption c = do
                    caption' <- currentSectionLevelToHtml c

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
                Embed syntax ->
                  pure $ div_ [class_ "source rich embed"] $ codeBlock [] (Syntax.toHtml syntax)
                EmbedInline syntax ->
                  pure $ span_ [class_ "source rich embed-inline"] $ inlineCode [] (Syntax.toHtml syntax)
            Join docs ->
              span_ [class_ "join"] <$> renderSequence currentSectionLevelToHtml (mergeWords " " docs)
            UntitledSection docs ->
              section_ [] <$> renderSequence (sectionContentToHtml currentSectionLevelToHtml) docs
            Column docs ->
              ul_
                [class_ "column"]
                <$> renderSequence
                  (fmap (li_ []) . currentSectionLevelToHtml)
                  (mergeWords " " docs)
            Group content ->
              span_ [class_ "group"] <$> currentSectionLevelToHtml content

-- HELPERS --------------------------------------------------------------------

-- | Unison Doc allows endlessly deep section nesting with
-- titles, but HTML only supports to h1-h6, so we clamp
-- the sectionLevel when converting
h :: Nat -> (Html () -> Html ())
h n =
  case n of
    1 -> h1_
    2 -> h2_
    3 -> h3_
    4 -> h4_
    5 -> h5_
    6 -> h6_
    _ -> h6_

badge :: Html () -> Html ()
badge =
  span_ [class_ "badge"]

textToClass :: Text -> Text
textToClass =
  Text.replace " " "__"
