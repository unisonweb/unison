{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Debug.Trace
import Data.Semigroup
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Reflex
import Reflex.Dom
import Unison.Dimensions (Width(..),X(..),Y(..),Height(..))
import Unison.Term
import Unison.UI (mouseMove')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Reflex.Dynamic as Dynamic
import qualified Unison.DocView as DocView
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Path as Path
import qualified Unison.Paths as Paths
import qualified Unison.Signals as Signals
import qualified Unison.Term as Term
import qualified Unison.TermExplorer as TermExplorer
import qualified Unison.TermExplorer as TermExplorer
import qualified Unison.UI as UI
import qualified Unison.Views as Views

term = builtin "Vector.concatenate" `app`
         (vector (map num [11..15])) `app`
         (vector ([builtin "Number.plus" `app` num 1 `app` num 1, num 2, num 9]))

termDoc = Views.term Views.defaultSymbol term

keepWhen :: Reflex t => Behavior t Bool -> Event t a -> Event t a
keepWhen = gate

dropWhen :: Reflex t => Behavior t Bool -> Event t a -> Event t a
dropWhen b = keepWhen (not <$> b)

termEditor :: (Reflex t, MonadWidget t m) => Term MemNode.V -> m ()
termEditor term0 = do
  node <- liftIO MemNode.make
  symbols0 <- (liftIO . Note.run . Node.metadatas node . Set.toList . Term.dependencies') term0
  keydown <- UI.windowKeydown
  rec
    openEvent <- pure $
      let
        events = leftmost
          [ void $ Signals.enter keydown
          , void $ ffilter (== 79) keydown -- [o]pen
          , void $ clickDoc ]
          -- todo: add advance events
      in dropWhen isExplorerOpen' events
    isExplorerOpen <- holdDyn False $
      let f p = case p of Just (_, True) -> True; _ -> False
      in leftmost [ True <$ openEvent, f <$> keepWhen isExplorerOpen' actions ]
    let isExplorerOpen' = current isExplorerOpen
    clickDoc <- Signals.switch' (maybe never (domEvent Click) <$> updated e)
    let s0 = (TermExplorer.S symbols0 Nothing 0)
    state <- foldDyn (<>) s0 state'
    docs <- id $
      let f term = sample (current state) >>= \(TermExplorer.S{..}) -> pure $ Views.termMd metadata term
      in mapDynM f terms
    terms <- id $
      let
        -- todo: interpret advance
        f (Just (a, advance)) oldTerm = case a of
          TermExplorer.Replace p term ->
            let msg = "replacing: " ++ show p ++ " with " ++ show term ++ " in\n " ++ show oldTerm
            in trace msg $ fromMaybe oldTerm $ Paths.modifyTerm (const term) p oldTerm
          _ -> error "todo: Eval + Step"
        f _ oldTerm = oldTerm
      in foldDyn f term0 actions
    paths <- id $
      let
        f (Just (TermExplorer.Replace p _, _)) = pure (Just p)
        f Nothing = Just <$> sample (current paths) -- refresh the path event on cancel
        f _ = pure Nothing
      -- the `guard` breaks a cycle - actions relies on paths
      in Signals.guard (push f actions) >>= \updates -> holdDyn Path.root (leftmost [paths', updates])
    (e, dims, paths', highlightRegion) <- elClass "div" "root" $
      DocView.widgets (dropWhen isExplorerOpen' keydown) (dropWhen isExplorerOpen') paths (Width 400) docs
    info <- do
      let f e p = liftIO . Note.run $ Node.localInfo node e p
      infos <- pure $ pushAlways
        (\_ -> f <$> sample (current terms) <*> sample (current paths))
        openEvent
      Signals.evaluate id infos
    explorerTopLeft <- holdDyn (X 0, Y 0) $ (\(X x, Y y, _, Height h) -> (X x, Y $ y + h)) <$> highlightRegion
    explorerResults <- Signals.offset "explorer-offset" explorerTopLeft . Signals.modal isExplorerOpen (never,never) $
                       TermExplorer.make node keydown info state paths terms
    state' <- Signals.switch' (fst <$> explorerResults)
    actions <- Signals.switch' (snd <$> explorerResults)
    -- causes cycle, because programPathUpdates is used to control current value of paths
    -- actions <- pure never -- works
  pure ()

main :: IO ()
main = mainWidget $ termEditor term
