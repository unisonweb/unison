{-# LANGUAGE FlexibleContexts #-}

module Unison.DocView where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Word (Word)
import Reflex.Dom
import Unison.Doc (Doc, Layout)
import Unison.Dimensions (X(..), Y(..), Width(..), Height(..))
import qualified Data.Text as Text
import qualified Unison.Doc as Doc
import qualified Unison.UI as UI

-- render :: Renderer e -> (e0 -> e) -> Layout e0 p -> e

data Picker p =
  Picker { at :: (X,Y) -> Maybe p
         , region :: p -> Maybe (X,Y,Width,Height) }

docWidget :: MonadWidget t m => Width -> Doc Text p -> m (Picker p)
docWidget available d = _g $ Doc.etraverse layout d
  where
  layout txt = do
    (e,_) <- el' "div" $ text (Text.unpack txt)
    (w,h) <- liftIO $ UI.preferredDimensions e
    pure (e, (w,h))
