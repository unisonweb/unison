module Unison.Signals where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.List
import Data.These hiding (mergeThese)
import Reflex
import Reflex.Dom
import Unison.Dimensions(X(..),Y(..))

modal :: (MonadWidget t m, Reflex t) => Dynamic t Bool -> a -> m a -> m (Event t a)
modal on whenOff ma =
  dyn =<< mapDyn (\b -> if b then ma else pure whenOff) on

evaluate :: (MonadWidget t m, Reflex t) => (a -> IO b) -> Event t a -> m (Event t b)
evaluate f actions = performEvent $ fmap (liftIO . f) actions

now :: (MonadWidget t m, Reflex t) => a -> m (Event t a)
now a = fmap (const a) <$> getPostBuild

offset :: (MonadWidget t m, Reflex t) => Dynamic t (X,Y) -> m a -> m a
offset topLeft inner = do
  f <- pure $
    let
      style (X x, Y y) = "style" =: intercalate ";"
        [ "position:absolute"
        , "left:" ++ show x ++ "px"
        , "top:" ++ show y ++ "px" ]
    in style
  attrs <- mapDyn f topLeft
  el "div" $ elDynAttr "div" attrs inner

delay :: (MonadHold t m, Reflex t) => a -> Event t a -> m (Event t a)
delay a e = do
  prev <- hold a e
  pure $ pushAlways (const (sample prev)) e

prepend :: (MonadWidget t m, Reflex t) => a -> Event t a -> m (Event t a)
prepend a e = do
  e0 <- now a
  pure $ leftmost [e0, e]

prependDyn :: (MonadWidget t m, Reflex t) => a -> Dynamic t a -> m (Dynamic t a)
prependDyn a e = do
  e0 <- now a
  holdDyn a (leftmost [e0, updated e])

toggle :: (MonadFix m, MonadHold t m, Reflex t) => Bool -> Event t a -> m (Dynamic t Bool)
toggle initial e = foldDyn (\b _ -> not b) initial (initial <$ e)

mergeThese :: Reflex t => Event t a -> Event t b -> Event t (These a b)
mergeThese a b = mergeWith g [fmap This a, fmap That b] where
  g (This a) (That b) = These a b
  g _ _ = error "not possible"

mergeLeft :: Reflex t => Event t a -> Event t b -> Event t (Either a b)
mergeLeft a b = mergeWith const [fmap Left a, fmap Right b]

-- | Tags each `a` event with `True` if the `b` event occurs simultaneously
coincides :: Reflex t => Event t a -> Event t b -> Event t (a, Bool)
coincides e1 e2 = push g (mergeThese e1 e2) where
  g (This a) = pure (Just (a, False))
  g (That _) = pure Nothing
  g (These a _) = pure (Just (a, True))

enter, upArrow, downArrow, leftArrow, rightArrow :: Reflex t => Event t Int -> Event t Int
leftArrow = ffilter (== 37)
upArrow = ffilter (== 38)
rightArrow = ffilter (== 39)
downArrow = ffilter (== 40)
enter = ffilter (== 13)
