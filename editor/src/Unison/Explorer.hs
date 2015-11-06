{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Unison.Explorer where

import Data.Functor
import Data.List
import Data.Maybe
import Data.Semigroup
import Reflex.Dom
import Unison.Dimensions (X(..),Y(..))
import qualified Unison.UI as UI
import qualified Unison.Signals as Signals

modal :: (MonadWidget t m, Reflex t) => Dynamic t Bool -> a -> m a -> m (Dynamic t a)
modal switch off on = do
  initial <- sample (current switch)
  let choose b = if b then on else pure off
  widgetHold (choose initial) (fmap choose (updated switch))

joinModal :: (MonadWidget t m, Reflex t) => Dynamic t Bool -> a -> m (Dynamic t a) -> m (Dynamic t a)
joinModal switch off on = holdDyn off never >>= \off -> joinDyn <$> modal switch off on

explorer :: forall t m k s a . (Reflex t, MonadWidget t m, Eq k, Semigroup s)
         => Event t Int
         -> (s -> String -> Action m s k a)
         -> Event t (m ()) -- loaded asynchronously on open of explorer
         -> Dynamic t s
         -> m (Dynamic t s, Event t (Maybe a))
explorer keydown processQuery topContent s0 = do
  let extractReq a = case a of Request r _ -> Just r; _ -> Nothing
  let validAttrs = "class" =: "explorer valid"
  let invalidAttrs = "class" =: "explorer invalid"
  rec
    attrs <- holdDyn ("class" =: "explorer") (fmap (\l -> if null l then invalidAttrs else validAttrs) valids)
    (valids, updatedS, closings) <- elDynAttr "div" attrs $ mdo
      searchbox <- textInput def
      UI.keepKeyEventIf (\i -> i /= 38 && i /= 40) searchbox -- disable up/down inside searchbox
      elClass "div" "top-separator" $ pure ()
      _ <- elClass "div" "top-content" $ widgetHold (pure ()) topContent -- todo: perhaps a spinner
      s <- sample (current s0)
      s' <- foldDyn (<>) s (updated responses)
      actions <- do
        t <- Signals.prepend "" (updated $ _textInput_value searchbox)
        pure $ pushAlways (\txt -> processQuery <$> sample (current s') <*> pure txt) t
      responses <- widgetHold (pure s) $ fmapMaybe extractReq actions
      list <- holdDyn [] $
        let f a = case a of Request _ l -> Just l; Results l -> Just l; _ -> Nothing
        in fmapMaybe f actions
      keys <- mapDyn (mapMaybe (\(k,b,_) -> if b then Just k else Nothing)) list
      valids <- holdDyn [] $ fmap (mapMaybe (\(k,b,v) -> if b then Just (k,v) else Nothing)) (updated list)
      invalids <- mapDyn (mapMaybe (\(_,b,v) -> if not b then Just v else Nothing)) list
      rec
        selectionIndex <- do
          let mouse = fmap (\i _ -> pure i) mouseEvent
          let nav f i l = if f i < length l && f i >= 0 then f i else i
          let up = fmap (\_ i -> nav (-1+) i <$> sample (current list)) $ Signals.upArrow keydown
          let down = fmap (\_ i -> nav (1+) i <$> sample (current list)) $ Signals.downArrow keydown
          let currentKey = safeIndex <$> current selectionIndex <*> current keys
          -- when new results arrive, try to keep selection on the same key, if it still exists
          newResults <- pure $
            let go ks k = fromMaybe 0 (k >>= \k -> elemIndex k ks)
            in pushAlways (\ks -> pure (\_ -> go ks <$> sample currentKey)) (updated keys)
          foldDynM ($) 0 $ mergeWith (\f g x -> g x >>= f) [mouse, up, down, newResults]
      (selectableRegion, (selectable, mouseEvent)) <- elAttr' "div" ("class" =: "results") $ do
        vi <- combineDyn (,) valids selectionIndex
        let c i j = "class" =: (if i == j then "result highlight" else "result")
        let vw (kmas,ind) = traverse (\(ma,i) -> elAttr' "div" (c i ind) ma) (map snd kmas `zip` [(0::Int)..])
        as <- widgetHold (pure []) $ fmap vw (updated vi)
        selectable <- mapDyn (map snd) as
        els <- mapDyn (map fst) as
        elHovers <- pure $
          let f :: [El t] -> [Event t Int]
              f els = map (\(el,i) -> i <$ domEvent Mousemove el) (els `zip` [(0::Int)..])
          in f
        e <- switchPromptly never $ leftmost . elHovers <$> updated els
        pure (selectable, e)
      _ <- dyn =<<
        let
          f valids invalids
            | null valids && not (null invalids) = elClass "div" "invalid-results" $ view invalids
            | otherwise = pure ()
          view l = elClass "div" "invalid-results" $ void $ traverse (elClass "div" "invalid-results-item") l
        in combineDyn f valids invalids
      selection <- combineDyn safeIndex selectionIndex selectable
      keyClosings <- pure $
        let f a = case a of Cancel -> Just Nothing; Accept a -> Just (Just a); _ -> Nothing
        in fmapMaybe f actions
      let mouseClosings = tag (current selection) (domEvent Click selectableRegion)
      let enterClosings = tag (current selection) (textInputGetEnter searchbox)
      pure (updated valids, s', leftmost [keyClosings, mouseClosings, enterClosings])
  pure (updatedS, closings)

safeIndex :: Int -> [a] -> Maybe a
safeIndex i l = if i < length l then Just (l !! i) else Nothing

data Action m s k a
  = Request (m s) [(k, Bool, m a)]
  | Results [(k, Bool, m a)]
  | Cancel
  | Accept a

-- let enter = textInputGetEnter searchbox
