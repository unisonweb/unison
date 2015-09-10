{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.DocView where

import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom
import Unison.Doc (Doc)
import Unison.Dom (Dom)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import Unison.Path (Path)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element as Element
import qualified Unison.Doc as Doc
import qualified Unison.Dom as Dom
import qualified Unison.HTML as HTML
import qualified Unison.UI as UI

data DocView p = DocView
  { at :: (X,Y) -> [p]
  , contains :: (X,Y,Width,Height) -> [p]
  , intersects :: (X,Y,Width,Height) -> [p]
  , leafRegion :: [p] -> (X,Y,Width,Height)
  , regions :: [p] -> [(X,Y,Width,Height)] }

widget :: (Show p, Path p, Eq p, MonadWidget t m) => Width -> Doc Text p -> m (El t, DocView p, (Width,Height))
widget available d =
  let
    leaf txt = Text.replace " " "&nbsp;" txt
    width (_, (w,_)) = w
    box = Doc.bounds snd . Doc.box . Doc.layout width available <$> Doc.etraverse layout d
    layout txt = do
      node <- runDom (Dom.el "div" [("class", "docwidget")] [Dom.raw (leaf txt)])
      -- todo, this method of computing preferred dimensions seems pretty slow,
      -- try just using canvas measureText function, see
      -- http://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
      (w,h) <- liftIO $ UI.preferredDimensions (Element.castToElement node)
      pure (txt, (w,h))
    view box = DocView (Doc.at box)
                       (Doc.contains box)
                       (Doc.intersects box)
                       (Doc.leafRegion box)
                       (Doc.regions box)
    interpret b = Dom.el "div" [("class","docwidget")] [dom]
      where
      dom = fromMaybe (HTML.hbox []) . Doc.einterpret go $ b'
      b' = Doc.emap (\(txt, (Width w, Height h)) -> Just $ Dom.el "div" (fixDims w h) [Dom.raw (leaf txt)])
                    b -- (Doc.rewrite collapse b)
      fixDims w h = [( "style","width:" <> (Text.pack . show $ w) <> "px;height:" <>
                     (Text.pack . show $ h) <> "px;")]
      go b = case b of
        Doc.BEmpty -> Nothing
        Doc.BEmbed dom -> dom
        Doc.BFlow dir bs -> case [ b | Just b <- bs ] of
          [] -> Nothing
          bs -> Just $ flexbox dir bs
          where flexbox Doc.Horizontal = HTML.hbox
                flexbox Doc.Vertical = HTML.vbox
  in do
    b <- box
    liftIO . putStrLn $ Doc.debugBoxp b
    let (_, (_,_,w,h)) = Doc.root b
    node <- runDom $ interpret (Doc.flatten b)
    e <- el "div" $ unsafePlaceElement (Dom.unsafeAsHTMLElement node)
    pure $ (e, view b, (w,h))

selectionLayer :: MonadWidget t m => Height -> (X,Y,Width,Height) -> m ()
selectionLayer (Height h0) (X x, Y y, Width w, Height h) =
  let
    attrs = Map.fromList [("style",style), ("class", "selection-layer")]
    style = intercalate ";"
      [ "pointer-events:none"
      , "position:relative"
      , "width:" ++ show w ++ "px"
      , "height:" ++ show h ++ "px"
      , "left:" ++ show x ++ "px"
      , "top:" ++ show (fromIntegral y - fromIntegral h0 + 1 :: Int) ++ "px" ]
  in do
    elAttr "div" attrs $ pure ()
    pure ()

runDom :: MonadWidget t m => Dom a -> m a
runDom dom = do
  doc <- askDocument
  liftIO $ Dom.run dom (Document.toDocument doc)
