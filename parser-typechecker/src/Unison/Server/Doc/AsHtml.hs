--
-- Render Unison.Server.Doc and embedded source to Html
--
{-# LANGUAGE OverloadedStrings #-}

module Unison.Server.Doc.AsHtml where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Server.Doc
import Unison.Server.Html
  ( Html,
    a,
    article,
    aside,
    blank,
    blockquote,
    br,
    code,
    details,
    div_,
    hr,
    img,
    li,
    ol,
    p,
    pre,
    section,
    span_,
    strong,
    summary,
    table,
    tbody,
    td,
    text,
    tr,
    ul,
  )
import qualified Unison.Server.Html as Html
import Unison.Server.Html.Attribute
  ( Attribute,
    alt,
    class_,
    data_,
    href,
    id_,
    open,
    rel,
    src,
    start,
    style,
    target,
  )
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax

data NamedLinkHref
  = Href Text
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

inlineCode :: [Attribute] -> Html -> Html
inlineCode attrs content =
  pre (class_ "inline-code" : attrs) [code [] [content]]

codeBlock :: [Attribute] -> Html -> Html
codeBlock attrs content =
  pre attrs [code [] [content]]

normalizeHref :: NamedLinkHref -> Doc -> NamedLinkHref
normalizeHref href doc =
  case doc of
    Word w ->
      case href of
        InvalidHref ->
          Href w
        Href h ->
          Href (h <> w)
        ReferenceHref _ ->
          href
    Group d_ ->
      normalizeHref href d_
    Join ds ->
      foldl' normalizeHref href ds
    Special (Link syntax) ->
      let folder acc seg =
            case acc of
              Nothing ->
                Syntax.reference seg
              _ ->
                acc
       in maybe InvalidHref ReferenceHref (Syntax.foldl folder Nothing syntax)
    _ ->
      href

data IsFolded
  = IsFolded Bool [Html] [Html]
  | Disabled Html

foldedToHtml :: [Attribute] -> IsFolded -> Html
foldedToHtml attrs isFolded =
  case isFolded of
    Disabled summary_ ->
      details attrs [summary [] [summary_]]
    IsFolded isFolded summary_ details_ ->
      let attrsWithOpen =
            if isFolded
              then open : attrs
              else attrs
       in details attrsWithOpen (summary [] summary_ : details_)

foldedToHtmlSource :: Bool -> EmbeddedSource -> Html
foldedToHtmlSource isFolded source =
  case source of
    Builtin summary ->
      foldedToHtml
        [class_ "rich source"]
        ( Disabled
            ( div_
                [class_ "builtin-summary"]
                [ codeBlock [] (Syntax.toHtml summary),
                  badge (span_ [] [strong [] [text "Built-in "], span_ [] [text "provided by the Unison runtime"]])
                ]
            )
        )
    EmbeddedSource summary details ->
      foldedToHtml
        [class_ "rich source"]
        ( IsFolded
            isFolded
            [codeBlock [] (Syntax.toHtml summary)]
            [codeBlock [] (Syntax.toHtml details)]
        )

-- Merge adjacent Word elements in a list to 1 element with a string of words
-- separated by space— useful for rendering to the dom without creating dom
-- elements for each and every word in the doc, but instead rely on textNodes
mergeWords :: [Doc] -> [Doc]
mergeWords = foldr merge_ [] where
  merge_ :: Doc -> [Doc] -> [Doc]
  merge_ d acc =
    case (d, acc) of
      (Word w, Word w_ : rest) ->
          Word (w <> " " <> w_) : rest

      _ ->
            d : acc

toHtml :: Doc -> Html
toHtml document =
  let toHtml_ sectionLevel doc =
        let -- Make it simple to retain the sectionLevel when recurring.
            -- the Section variant increments it locally
            currentSectionLevelToHtml =
              toHtml_ sectionLevel

            sectionContentToHtml renderer doc_ =
              case doc_ of
                Paragraph _ ->
                  p [] [renderer doc_]
                _ ->
                  renderer doc_
         in case doc of
              Word word ->
                span_ [class_ "word"] [text word]
              Code code ->
                span_ [class_ "rich source inline-code"] [inlineCode [] (currentSectionLevelToHtml code)]
              CodeBlock lang code ->
                div_ [class_ "rich source code", class_ $ textToClass lang] [codeBlock [] (currentSectionLevelToHtml code)]
              Bold d ->
                strong [] [currentSectionLevelToHtml d]
              Italic d ->
                span_ [class_ "italic"] [currentSectionLevelToHtml d]
              Strikethrough d ->
                span_ [class_ "strikethrough"] [currentSectionLevelToHtml d]
              Style cssclass_ d ->
                span_ [class_ $ textToClass cssclass_] [currentSectionLevelToHtml d]
              Anchor id' d ->
                a [id_ id', target id'] [currentSectionLevelToHtml d]
              Blockquote d ->
                blockquote [] [currentSectionLevelToHtml d]
              Blankline ->
                div_ [] [br [], br []]
              Linebreak ->
                br []
              SectionBreak ->
                hr []
              Tooltip triggerContent tooltipContent ->
                span_
                  [class_ "tooltip below arrow-start"]
                  [ span_ [class_ "tooltip-trigger"] [currentSectionLevelToHtml triggerContent],
                    div_ [class_ "tooltip-bubble", style "display: none"] [currentSectionLevelToHtml tooltipContent]
                  ]
              Aside d ->
                span_
                  [class_ "aside-anchor"]
                  [ aside [] [currentSectionLevelToHtml d]
                  ]
              Callout icon content ->
                let (cls, ico) =
                      case icon of
                        Just (Word emoji) ->
                          (class_ "callout callout-with-icon", div_ [class_ "callout-icon"] [text emoji])
                        _ ->
                          (class_ "callout", blank)
                 in div_
                      [cls]
                      [ ico,
                        div_
                          [class_ "callout-content"]
                          [currentSectionLevelToHtml content]
                      ]
              Table rows ->
                let cellToHtml d =
                      td [] [currentSectionLevelToHtml d]

                    rowToHtml cells =
                      tr [] (map cellToHtml (mergeWords cells))
                 in table [] [tbody [] (map rowToHtml rows)]
              Folded isFolded summary details ->
                let content =
                      if isFolded
                        then [currentSectionLevelToHtml summary]
                        else
                          [ currentSectionLevelToHtml summary,
                            currentSectionLevelToHtml details
                          ]
                 in foldedToHtml [] (IsFolded isFolded content [])
              Paragraph docs ->
                case docs of
                  [d] ->
                    currentSectionLevelToHtml d
                  ds ->
                    span_ [class_ "span"] (map currentSectionLevelToHtml (mergeWords ds))
              BulletedList items ->
                let itemToHtml d =
                      li [] [currentSectionLevelToHtml d]
                 in ul [] (map itemToHtml (mergeWords items))
              NumberedList startNum items ->
                let itemToHtml d =
                      li [] [currentSectionLevelToHtml d]
                 in ol [start startNum] (map itemToHtml (mergeWords items))
              Section title docs ->
                let -- Unison Doc allows endlessly deep section nesting with
                    -- titles, but HTML only supports to h1-h6, so we clamp
                    -- the sectionLevel when converting
                    level =
                      min 6 sectionLevel

                    titleEl =
                      Html.element (Text.pack $ "h" ++ show level) [] [currentSectionLevelToHtml title]
                 in section [] (titleEl : map (sectionContentToHtml (toHtml_ (sectionLevel + 1))) docs)
              NamedLink label href_ ->
                case normalizeHref InvalidHref href_ of
                  Href h ->
                    a [class_ "named-link", href h, rel "noopener", target "_blank"] [currentSectionLevelToHtml label]
                  ReferenceHref ref ->
                    a [class_ "named-link", data_ "ref" ref] [currentSectionLevelToHtml label]
                  InvalidHref ->
                    span_ [class_ "named-link invalid-href"] [currentSectionLevelToHtml label]
              Image altText src_ caption ->
                let altAttr =
                      case altText of
                        Word t ->
                          [alt t]
                        _ ->
                          []

                    image =
                      case src_ of
                        Word s ->
                          img (altAttr ++ [src s])
                        _ ->
                          blank

                    imageWithCaption c =
                      div_
                        [class_ "image-with-caption"]
                        [ image,
                          div_ [class_ "caption"] [currentSectionLevelToHtml c]
                        ]
                 in maybe image imageWithCaption caption
              Special specialForm ->
                case specialForm of
                  Source sources ->
                    div_
                      [class_ "folded-sources"]
                      (mapMaybe (fmap (foldedToHtmlSource False) . embeddedSource) sources)
                  FoldedSource sources ->
                    div_
                      [class_ "folded-sources"]
                      (mapMaybe (fmap (foldedToHtmlSource True) . embeddedSource) sources)
                  Example syntax ->
                    span_ [class_ "source rich example-inline"] [inlineCode [] (Syntax.toHtml syntax)]
                  ExampleBlock syntax ->
                    div_ [class_ "source rich example"] [codeBlock [] (Syntax.toHtml syntax)]
                  Link syntax ->
                    inlineCode [class_ "rich source"] (Syntax.toHtml syntax)
                  Signature signatures ->
                    div_
                      [class_ "rich source signatures"]
                      ( map
                          (\sig -> div_ [class_ "signature"] [Syntax.toHtml sig])
                          signatures
                      )
                  SignatureInline sig ->
                    span_ [class_ "rich source signature-inline"] [Syntax.toHtml sig]
                  Eval source result ->
                    div_
                      [class_ "source rich eval"]
                      [codeBlock [] (div_ [] [Syntax.toHtml source, div_ [class_ "result"] [text "⧨", div_ [] [Syntax.toHtml result]]])]
                  EvalInline source result ->
                    span_
                      [class_ "source rich eval-inline"]
                      [inlineCode [] (span_ [] [Syntax.toHtml source, span_ [class_ "result"] [text "⧨", Syntax.toHtml result]])]
                  Embed syntax ->
                    div_ [class_ "source rich embed"] [codeBlock [] (Syntax.toHtml syntax)]
                  EmbedInline syntax ->
                    span_ [class_ "source rich embed-inline"] [inlineCode [] (Syntax.toHtml syntax)]
              Join docs ->
                span_ [class_ "join"] (map currentSectionLevelToHtml (mergeWords docs))
              UntitledSection docs ->
                section [] (map (sectionContentToHtml currentSectionLevelToHtml) docs)
              Column docs ->
                ul
                  [class_ "column"]
                  ( map
                      (\c -> li [] [currentSectionLevelToHtml c])
                      (mergeWords docs)
                  )
              Group content ->
                span_ [class_ "group"] [currentSectionLevelToHtml content]
   in article [class_ "unison-doc"] [toHtml_ 1 document]

-- HELPERS

badge :: Html -> Html
badge content =
  span_ [class_ "badge"] [content]

textToClass :: Text -> Text
textToClass =
  Text.replace " " "__"
