{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.DocView where

import Control.Monad.IO.Class
import Control.Comonad.Cofree (Cofree(..)) -- (:<)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.These (These(This,That,These))
import Reflex.Dom
import Unison.Doc (Doc)
import Unison.Dom (Dom)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import Unison.Path (Path)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element as Element
import qualified Reflex.Dynamic as Dynamic
import qualified Unison.Doc as Doc
import qualified Unison.Dom as Dom
import qualified Unison.HTML as HTML
import qualified Unison.UI as UI
import qualified Unison.Signals as S
import qualified Debug.Trace as Trace

widget :: (Show p, Path p, Eq p, MonadWidget t m)
       => Event t Int -> Width -> Doc Text p -> m (El t, (Width,Height), Dynamic t p)
widget keydown available d =
  let
    leaf txt = Text.replace " " "&nbsp;" txt
    width (_, (w,_)) = w
    box = Doc.bounds snd . Doc.box . Doc.layout width available <$> Doc.etraverse layout d
    layout txt = do
      -- node <- runDom (Dom.el "div" [("class", "docwidget")] [Dom.raw (leaf txt)])
      -- todo, this method of computing preferred dimensions seems pretty slow,
      -- try just using canvas measureText function, see
      -- http://stackoverflow.com/questions/118241/calculate-text-width-with-javascript/21015393#21015393
      -- (w,h) <- liftIO $ UI.preferredDimensions (Element.castToElement node)
      -- NB: precomputed 10px per character width, 20px per line height
      (w,h) <- pure $ (Width (fromIntegral (Text.length txt) * 10), Height 20)
      pure (txt, (w,h))
    interpret b = Dom.el "div" [("class","docwidget")] [dom]
      where
      dom = fromMaybe (HTML.hbox []) . Doc.einterpret go $ b'
      b' = Doc.emap (\(txt, (Width w, Height h)) -> Just $ Dom.el "div" (fixDims w h) [Dom.raw (leaf txt)])
                    b
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
    (e,_) <- el' "div" $ unsafePlaceElement (Dom.unsafeAsHTMLElement node)
    mouse <- UI.mouseMove' e
    nav <- pure $ mergeWith (.) [
      const (Doc.up b) <$> (traceEvent "up" $ S.upArrow keydown),
      const (Doc.down b) <$> (traceEvent "down" $ S.downArrow keydown),
      const (Doc.left b) <$> (traceEvent "left" $ S.leftArrow keydown),
      const (Doc.right b) <$> (traceEvent "right" $ S.rightArrow keydown),
      const (Doc.up b) <$> (traceEvent "up" $ ffilter (== 75) keydown), -- k
      const (Doc.down b) <$> (traceEvent "down" $ ffilter (== 74) keydown), -- j
      const (Doc.left b) <$> (traceEvent "left" $ ffilter (== 72) keydown), -- h
      const (Doc.right b) <$> (traceEvent "right" $ ffilter (== 76) keydown), -- l
      const (Doc.expand b) <$> (traceEvent "expand" $ ffilter (== 85) keydown), -- u
      const (Doc.contract b) <$> (traceEvent "contract" $ ffilter (== 68) keydown), -- d
      const (Doc.leftmost b) <$> (traceEvent "leftmost" $ ffilter (== 71) keydown), -- g
      const (Doc.rightmost b) <$> (traceEvent "rightmost" $ ffilter (== 186) keydown), -- ;
      (\pt _ -> Doc.at b pt) <$> mouse ]
    path <-
      let
        debug p = Trace.trace
          ("contains': "++ (show $ wrangle (Doc.contains' b (Doc.region b p))))
          p
        wrangle parents = [ (p,r) | ((p,r) :< _) <- parents ]
      in
        mapDyn debug =<< Dynamic.foldDyn ($) (Doc.at b (X 0, Y 0)) nav
    region <- Dynamic.traceDyn "region" <$> mapDyn (Doc.region b) path
    sel <- mapDyn selectionLayer region
    _ <- widgetHold (pure ()) (Dynamic.updated sel)
    pure (e, (w,h), path)

selectionLayer :: MonadWidget t m => (X,Y,Width,Height) -> m ()
selectionLayer (X x, Y y, Width w, Height h) =
  let
    attrs = Map.fromList [("style",style), ("class", "selection-layer")]
    style = intercalate ";"
      [ "pointer-events:none"
      , "position:absolute"
      , "width:" ++ show w ++ "px"
      , "height:" ++ show h ++ "px"
      , "left:" ++ show x ++ "px"
      , "top:" ++ show y ++ "px" ]
  in do
    elAttr "div" attrs $ pure ()
    pure ()

runDom :: MonadWidget t m => Dom a -> m a
runDom dom = do
  doc <- askDocument
  liftIO $ Dom.run dom (Document.toDocument doc)
