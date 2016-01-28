{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings #-}

module Unison.UI (keepKeyEventIf, mouseMove, preferredDimensions, windowKeydown, windowKeyup) where

import Control.Monad.IO.Class
import GHCJS.Marshal
import GHCJS.Types (JSRef)
import GHCJS.DOM.Element (Element)
import GHCJS.DOM.Window (Window)
import Reflex
import Reflex.Dom
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.UIEvent as UIEvent
import qualified GHCJS.DOM.Window as Window
import qualified Unison.Signals as Signals

mouseMove :: (MonadWidget t m, Reflex t) => El t -> m (Event t (X,Y))
mouseMove e = Signals.evaluate tagOffsets movements where
  movements = domEvent Mousemove e
  tagOffsets (x,y) = do
    ex <- Element.getOffsetLeft (_el_element e)
    ey <- Element.getOffsetTop (_el_element e)
    return (X . fromIntegral $ x - floor ex, Y . fromIntegral $ y - floor ey)

keepKeyEventIf :: (MonadWidget t m, Reflex t) => (Int -> Bool) -> TextInput t -> m ()
keepKeyEventIf f input = do
  let tweak e = e >>= \i -> if f i then pure i else i <$ EventM.preventDefault
  tweakedKeydown <- wrapDomEvent (_textInput_element input)
                                 (\e -> EventM.on e Element.keyDown)
                                 (tweak getKeyEvent)
  performEvent_ (pure () <$ tweakedKeydown)

askWindow :: (MonadIO m, HasDocument m) => m Window
askWindow =  do
  (Just window) <- askDocument >>= liftIO . Document.getDefaultView
  return window

windowKeydown :: MonadWidget t m => m (Event t Int)
windowKeydown = do
  w <- askWindow
  wrapDomEvent w (\w -> EventM.on w Window.keyDown) (liftIO . UIEvent.getKeyCode =<< EventM.event)

windowKeyup :: MonadWidget t m => m (Event t Int)
windowKeyup = do
  w <- askWindow
  wrapDomEvent w (\w -> EventM.on w Window.keyUp) (liftIO . UIEvent.getKeyCode =<< EventM.event)

preferredDimensions :: Element.IsElement e => e -> IO (Width,Height)
preferredDimensions e = case Element.toElement e of
  e -> do
    Just (w,h) <- fromJSRef =<< preferredDimsImpl (Element.toElement e)
    return (Width w, Height h)

#ifdef __GHCJS__
foreign import javascript unsafe
  "{  var temp = document.pdi234098234; \
      /* use cached temporary div for performance */ \
      if (!temp) { \
        temp = document.createElement('div'); \
        temp.style.visibility = 'hidden'; \
        temp.style.styleFloat = 'left'; \
        temp.style.cssFloat = 'left'; \
        document.body.appendChild(temp); \
        document.pdi234098234 = temp; \
      }; \
      temp.appendChild($1); \
      var style = window.getComputedStyle(temp, null); \
      var w = Math.ceil(style.getPropertyValue('width').slice(0,-2) - 0); \
      var h = Math.ceil(style.getPropertyValue('height').slice(0,-2) - 0); \
      temp.removeChild($1); \
      $r = [w, h]; \
  }"
  preferredDimsImpl :: Element -> IO JSRef
#else
preferredDimsImpl = error "preferredDimsImpl: only available from JavaScript"
#endif
