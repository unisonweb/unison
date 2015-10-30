{-# LANGUAGE RecursiveDo #-}

module Unison.Explorer where

import Data.Maybe
import Data.Semigroup
import Control.Monad.Fix
import Reflex.Dom
import Unison.Dimensions (X(..),Y(..))
import qualified Unison.Signals as Signals

explorer :: (Reflex t, MonadWidget t m, Eq k, Semigroup s)
         => Event t Int
         -> (s -> String -> Action (m s) (k, Bool, m a))
         -> Event t (m ()) -- loaded asynchronously on open of explorer
         -> Dynamic t s
         -> m (Dynamic t (Maybe a, s))
explorer keydown processQuery topContent s0 =
  let
    extractReq a = case a of Request r _ -> Just r; _ -> Nothing
    view list ind = do
      -- todo
      pure never
    viewInvalid list = pure ()
  in do
    let validAttrs = "class" =: "explorer valid"
    let invalidAttrs = "class" =: "explorer invalid"
    rec
      attrs <- holdDyn ("class" =: "explorer") (fmap (\l -> if null l then invalidAttrs else validAttrs) valids)
      (valids, result) <- elDynAttr "div" attrs $ mdo
        searchbox <- textInput def
        elClass "div" "top-separator" $ pure ()
        _ <- elClass "div" "top-content" $ widgetHold (pure ()) topContent -- todo: perhaps a spinner
        s <- sample (current s0)
        s' <- foldDyn (<>) s (updated responses)
        actions <- pure $
          pushAlways (\txt -> processQuery <$> sample (current s') <*> pure txt)
                     (updated $ _textInput_value searchbox)
        responses <- widgetHold (pure s) $ fmapMaybe extractReq actions
        list <- holdDyn [] $
          let f a = case a of Request _ l -> Just l; Results l -> Just l; _ -> Nothing
          in fmapMaybe f actions
        valids <- holdDyn [] $ fmap (mapMaybe (\(k,b,v) -> if b then Just (k,v) else Nothing)) (updated list)
        invalids <- mapDyn (mapMaybe (\(k,b,v) -> if not b then Just v else Nothing)) list
        selectable <- widgetHold (pure []) $ fmap (traverse snd) (updated valids)
        selectionIndex <- do
          let mouse = fmap (\i _ -> pure i) mouseEvent
          let nav f i l = if f i < length l && f i > 0 then f i else i
          let up = fmap (\_ i -> nav (-1+) i <$> sample (current list)) $ Signals.upArrow keydown
          let down = fmap (\_ i -> nav (1+) i <$> sample (current list)) $ Signals.downArrow keydown
          foldDynM ($) 0 $ mergeWith (\f g x -> g x >>= f) [mouse, up, down] --, newResults]
        mouseEvent <- elClass "div" "results" $
          let f list = view list <$> sample (current selectionIndex)
          in do
            phases <- widgetHold (pure never) $ pushAlways f (updated valids)
            switchPromptly never (updated phases)
        _ <- dyn =<<
          let f valids invalids
                | null valids && not (null invalids) = elClass "div" "invalid-results" $ viewInvalid invalids
                | otherwise = pure ()
          in combineDyn f valids invalids
        let safeIndex i l = if i < length l then Just (l !! i) else Nothing
        selection <- combineDyn safeIndex selectionIndex selectable
        d <- combineDyn (,) selection s'
        pure (updated valids, d)
    pure result

data Action r a
  = Request r [a]
  | Results [a]
  | Cancel
  | Accept a

-- let enter = textInputGetEnter searchbox
