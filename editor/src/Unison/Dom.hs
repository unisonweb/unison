{-# LANGUAGE MultiParamTypeClasses #-}

module Unison.Dom where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Foldable
import Data.Functor (void)
import Data.Text (Text)
import GHCJS.DOM.Types (IsNode)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as Types
import qualified Reflex.Dom as Reflex

newtype Dom a = Dom { run :: Document -> IO a }

type Node = Node.Node
type Document = Document.Document
type Element = Element.Element
type HTMLElement = HTMLElement.HTMLElement

unsafeAsHTMLElement :: Node -> HTMLElement
unsafeAsHTMLElement node = Types.castToHTMLElement node

raw :: Text -> Dom Node
raw s = Dom $ \doc -> do
  Just n <- Document.documentCreateElement doc "div"
  let elem = unsafeAsHTMLElement (Node.toNode n)
  HTMLElement.htmlElementSetInnerHTML elem s
  pure $ Node.toNode n

text :: Text -> Dom Node
text s = Dom $ \doc -> do
  Just n <- Document.documentCreateTextNode doc s
  pure $ Node.toNode n

el :: IsNode n => Text -> [(Text,Text)] -> [Dom n] -> Dom Node
el tag attrs inners = Dom $ \doc -> do
  Just parent <- Document.documentCreateElement doc tag
  traverse_ (\inner -> run inner doc >>= \dom -> Node.nodeAppendChild parent (Just dom)) inners
  traverse_ (\(k,v) -> Element.elementSetAttribute parent k v) attrs
  pure $ Node.toNode parent

el' :: IsNode n => Text -> [Dom n] -> Dom Node
el' tag inner = el tag [] inner

askDocument :: Dom Document
askDocument = Dom $ \doc -> pure doc

instance MonadIO Dom where
  liftIO a = Dom (const a)

instance Monad Dom where
  return a = Dom (const (pure a))
  Dom a >>= f = Dom $ \d -> a d >>= (\a -> run (f a) d)

instance Functor Dom where
  fmap = liftM

instance Applicative Dom where
  pure = return
  (<*>) = ap
