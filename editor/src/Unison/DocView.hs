{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.DocView where

import Control.Comonad.Cofree (Cofree(..), unwrap) -- (:<)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word)
import Reflex.Dom
import Unison.Doc (Box, Doc, Layout)
import Unison.Dom (Dom)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import Unison.Path (Path)
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
  , regions :: [p] -> [(X,Y,Width,Height)] }

widget :: (Path p, Eq p, MonadWidget t m) => Width -> Doc Text p -> m (El t, DocView p)
widget available d =
  let
    leaf txt = Text.replace " " "&nbsp;" txt
    width (_, (w,_)) = w
    box = Doc.bounds snd . Doc.box . Doc.layout width available <$> Doc.etraverse layout d
    layout txt = do
      node <- runDom (Dom.el "div" [("class", "docwidget")] [Dom.raw (leaf txt)])
      (w@(Width w'), h@(Height h')) <- liftIO $ UI.preferredDimensions (Element.castToElement node)
      liftIO $ putStrLn ("preferred (w,h): " ++ show (w',h'))
      pure (txt, (w,h))
    view box = DocView (Doc.at box) (Doc.contains box) (Doc.intersects box) (Doc.regions box)
    interpret b = Dom.el "div" [("class","docwidget")] [dom]
      where
      dom = fromMaybe (HTML.hbox []) . Doc.einterpret go $
              Doc.emap (\(txt,_) -> Just $ Dom.el' "div" [Dom.raw (leaf txt)]) b
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
    node <- runDom $ interpret (Doc.flatten b)
    e <- el "div" $ unsafePlaceElement (Dom.unsafeAsHTMLElement node)
    pure $ (e, view b)

runDom :: MonadWidget t m => Dom a -> m a
runDom dom = do
  doc <- askDocument
  liftIO $ Dom.run dom (Document.toDocument doc)
