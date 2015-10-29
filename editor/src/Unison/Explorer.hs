{-# LANGUAGE RecursiveDo #-}

module Unison.Explorer where

import Data.Semigroup
import Control.Monad.Fix
import Reflex.Dom
import Data.Default (def)
import Unison.Dimensions (X(..),Y(..))
import qualified Unison.Signals as Signals

sum0 :: (Reflex t, MonadHold t m, MonadFix m) => Event t Int -> m (Dynamic t Int)
sum0 s = mdo
  a <- holdDyn 0 (pushAlways (\x -> (x+) <$> (sample $ current a)) s)
  pure a

explorer :: (Reflex t, MonadWidget t m, Eq k, Semigroup s)
         => Event t Int
         -- -> take in a m (), for stuff that is loaded asynchronously on open of explorer
         -> (s -> String -> Action (m s) (k, m a))
         -> Dynamic t s
         -> m (Dynamic t (Maybe a, s))
explorer keydown processQuery s0 =
  let
    extractReq a = case a of Request r _ -> Just r; _ -> Nothing
    view list ind = do
      -- e <- holdDyn 0 never -- todo
      pure never
  in
    elClass "div" "explorer" $ mdo
      searchbox <- textInput def
      elClass "div" "top-separator" $ pure ()
      s <- sample (current s0)
      s' <- foldDyn (<>) s (updated responses)
      actions <- pure $
        pushAlways (\txt -> processQuery <$> sample (current s') <*> pure txt)
                   (updated $ _textInput_value searchbox)
      responses <- widgetHold (pure s) $ fmapMaybe extractReq actions
      list <- holdDyn [] $
        let
          f a = case a of
            Request _ l -> Just l
            Results l -> Just l
            _ -> Nothing
        in
          fmapMaybe f actions
      selectable <- widgetHold (pure []) $ fmap (traverse snd) (updated list)
      selectionIndex <- do
        let mouse = fmap (\i _ -> pure i) mouseEvent
        let nav f i l = if f i < length l && f i > 0 then f i else i
        let up = fmap (\_ i -> nav (-1+) i <$> sample (current list)) $ Signals.upArrow keydown
        let down = fmap (\_ i -> nav (1+) i <$> sample (current list)) $ Signals.downArrow keydown
        foldDynM ($) 0 $ mergeWith (\f g x -> g x >>= f) [mouse, up, down] --, newResults]
      mouseEvent <- elClass "div" "results" $
        let f list = view list <$> sample (current selectionIndex)
        in do
          phases <- widgetHold (pure never) $ pushAlways f (updated list)
          switchPromptly never (updated phases)
      let safeIndex i l = if i < length l then Just (l !! i) else Nothing
      selection <- combineDyn safeIndex selectionIndex selectable
      combineDyn (,) selection s'

data Action r a
  = Request r [a]
  | Results [a]
  | Cancel
  | Accept a

-- let enter = textInputGetEnter searchbox
