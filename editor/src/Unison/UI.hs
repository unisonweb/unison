{-# LANGUAGE CPP, ForeignFunctionInterface, JavaScriptFFI, OverloadedStrings #-}

module Unison.UI (keepKeyEventIf, mouseMove, mouseMove', preferredDimensions, windowKeydown, windowKeyup) where

import Control.Monad.IO.Class
import Data.Text (Text)
import GHCJS.DOM.Element (elementOnkeydown)
import GHCJS.DOM.EventM (preventDefault)
import GHCJS.DOM.Types (Element,DOMWindow)
import GHCJS.Marshal
import GHCJS.Types (JSRef)
import Reflex
import Reflex.Dom
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.DOMWindow as DOMWindow
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.UIEvent as UIEvent

mouseLocal :: UIEvent.IsUIEvent e => Element -> e -> IO (X,Y)
mouseLocal e event = do
  x <- UIEvent.uiEventGetLayerX event
  y <- UIEvent.uiEventGetLayerY event
  ex <- Element.elementGetOffsetLeft e
  ey <- Element.elementGetOffsetTop e
  return (X . fromIntegral $ x - floor ex, Y . fromIntegral $ y - floor ey)

mouseMove :: Element.IsElement e => MonadWidget t m => e -> m (Event t (X,Y))
mouseMove e = case Element.toElement e of
  e -> wrapDomEvent
         e
         Element.elementOnmousemove
         (liftIO . mouseLocal e =<< EventM.event)

keepKeyEventIf :: (MonadWidget t m, Reflex t) => (Int -> Bool) -> TextInput t -> m ()
keepKeyEventIf f input = do
  let tweak e = e >>= \i -> if f i then pure i else i <$ preventDefault
  tweakedKeydown <- wrapDomEvent (_textInput_element input) Element.elementOnkeydown (tweak getKeyEvent)
  performEvent_ (pure () <$ tweakedKeydown)

askWindow :: (MonadIO m, HasDocument m) => m DOMWindow
askWindow =  do
  (Just window) <- askDocument >>= liftIO . Document.documentGetDefaultView
  return window

windowKeydown :: MonadWidget t m => m (Event t Int)
windowKeydown = do
  w <- askWindow
  wrapDomEvent w DOMWindow.domWindowOnkeydown (liftIO . UIEvent.uiEventGetKeyCode =<< EventM.event)

windowKeyup :: MonadWidget t m => m (Event t Int)
windowKeyup = do
  w <- askWindow
  wrapDomEvent w DOMWindow.domWindowOnkeyup (liftIO . UIEvent.uiEventGetKeyCode =<< EventM.event)

mouseMove' :: MonadWidget t m => El t -> m (Event t (X,Y))
mouseMove' = mouseMove . _el_element

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
  preferredDimsImpl :: Element -> IO (JSRef (Word,Word))
#else
preferredDimsImpl = error "preferredDimsImpl: only available from JavaScript"
#endif
