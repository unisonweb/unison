{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.DocView where

import Control.Monad.IO.Class
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom
import Unison.Doc (Doc,Box)
import Unison.Dom (Dom)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..), Region)
import Unison.Path (Path)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified GHCJS.DOM.Document as Document
import qualified Reflex.Dynamic as Dynamic
import qualified Unison.Doc as Doc
import qualified Unison.Dom as Dom
import qualified Unison.HTML as HTML
import qualified Unison.Path as Path
import qualified Unison.Signals as S
import qualified Unison.UI as UI

widgets :: (Show p, Path p, Eq p, MonadWidget t m)
        => Event t Int -> (Event t (X,Y) -> Event t (X,Y))
        -> Dynamic t p
        -> Width -> Dynamic t (Doc Text p)
        -> m ( Dynamic t (Maybe (El t))
             , Dynamic t (Width,Height)
             , Behavior t (Box Text (p,Region))
             , Event t p
             , Event t (X,Y,Width,Height) )
widgets keydown filterMouse paths available docs = do
  p <- dyn =<< mapDyn (\doc -> widget keydown filterMouse paths available doc) docs
  els <- holdDyn Nothing $ (\(e,_,_,_,_) -> Just e) <$> p
  dims <- holdDyn (Width 0, Height 0) ((\(_,xy,_,_,_) -> xy) <$> p)
  paths' <- S.switch' ((\(_,_,_,p,_) -> p) <$> p)
  regions' <- S.switch' ((\(_,_,_,_,r) -> updated r) <$> p)
  boxes <- hold ((Path.root, (X 0, Y 0, Width 0, Height 0)) :< Doc.BEmpty) ((\(_,_,b,_,_) -> b) <$> p)
  pure (els, dims, boxes, paths', regions')

view :: (MonadWidget t m, Path p)
     => Width -> Doc Text p
     -> m (El t, (Width,Height), Box (Text, (Width, Height)) (p, Region))
view available d =
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
    let (_, (_,_,w,h)) = Doc.root b
    node <- runDom $ interpret (Doc.flatten b)
    (e,_) <- el' "div" $ unsafePlaceElement (Dom.unsafeAsHTMLElement node)
    pure (e, (w,h), b)

widget :: (Show p, Path p, Eq p, MonadWidget t m)
       => Event t Int
       -> (Event t (X,Y) -> Event t (X,Y))
       -> Dynamic t p
       -> Width -> Doc Text p -> m (El t, (Width,Height), Box Text (p,Region), Event t p, Dynamic t (X,Y,Width,Height))
widget keydown filterMouse paths available d = do
  (e, (w,h), b) <- view available d
  mouse <- do
    xy <- filterMouse <$> UI.mouseMove e
    xy' <- holdDyn (X 0, Y 0) xy
    pure $ updated (nubDyn xy')
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
  paths' <- pure $
    let f update = update <$> sample (current paths)
    in traceEvent "DocView.path" $ pushAlways f nav
  region <- mapDyn (Doc.region b) paths
  sel <- mapDyn selectionLayer region
  _ <- widgetHold (pure ()) (Dynamic.updated sel)
  pure (e, (w,h), Doc.emap fst b, paths', region)

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
