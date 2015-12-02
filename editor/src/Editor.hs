{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Debug.Trace as Trace
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import Reflex
import Reflex.Dom
import Unison.Dimensions (Width(..),X(..),Y(..),Height(..),Region)
import Unison.Doc (Box)
import Unison.Node.MemNode (V)
import Unison.Term
import qualified Data.Set as Set
import qualified Unison.Doc as Doc
import qualified Unison.DocView as DocView
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Path as Path
import qualified Unison.Paths as Paths
import qualified Unison.Signals as Signals
import qualified Unison.Term as Term
import qualified Unison.TermExplorer as TermExplorer
import qualified Unison.UI as UI
import qualified Unison.Views as Views

term :: Term MemNode.V
term = builtin "Vector.concatenate" `app`
         (vector (map num [11..15])) `app`
         (vector ([builtin "Number.plus" `app` num 1 `app` num 1, num 2, num 9]))

termEditor :: (Reflex t, MonadWidget t m) => Term MemNode.V -> m ()
termEditor term0 = do
  node <- liftIO MemNode.make
  symbols0 <- (liftIO . Note.run . Node.metadatas node . Set.toList . Term.dependencies') term0
  keydown <- UI.windowKeydown
  rec
    openEvent <- id $
      let
        advanced (Just (_,True)) = True
        advanced _ = False
        events = leftmost
          [ void $ Signals.enter keydown
          , void $ ffilter (== 79) keydown -- [o]pen
          , void $ clickDoc ]
        event as = leftmost [ as, Signals.dropWhen isExplorerOpen' events ]
      -- hack to ensure actions has a chance to update the layout and new path before reopening
      in event <$> delay (20/1000) (void $ ffilter advanced actions)
    isExplorerOpen <- holdDyn False $
      -- todo, might want to pause a bit if the explorer is closed due to an advance event
      leftmost [ True <$ openEvent, Signals.keepWhen isExplorerOpen' (False <$ actions) ]
    let isExplorerOpen' = current isExplorerOpen
    clickDoc <- Signals.switch' (maybe never (domEvent Click) <$> updated e)
    state <- foldDyn (<>) (TermExplorer.S symbols0) state'
    docs <- id $
      let f term = sample (current state) >>= \(TermExplorer.S{..}) -> pure $ Views.termMd metadata term
      in mapDynM f terms
    terms <- id $
      let
        f (Just (a, _)) oldTerm = case a of
          TermExplorer.Replace p term ->
            let msg = "replacing: " ++ show p ++ " with " ++ show term ++ " in\n " ++ show oldTerm
            in Trace.trace msg $ fromMaybe oldTerm $ Paths.modifyTerm (const term) p oldTerm
          _ -> error "todo: Eval + Step"
        f _ oldTerm = oldTerm
      in foldDyn f term0 actions
    paths <- id $
      let
        a _ = advancePath <$> sample (current paths) <*> sample boxes <*> sample (current terms)
        keyAdvance = push a (traceEvent "advance-keypress" $ void $ ffilter (== 65) keydown) -- [a]dvance
        ok (Just (_, True)) = a ()
        ok _ = pure Nothing
        explorerAdvance = push ok <$> Signals.guard actions
        f (Just (TermExplorer.Replace p _, _)) = pure (Just p)
        f Nothing = Just <$> sample (current paths) -- refresh the path event on cancel
        f _ = pure Nothing
        events = leftmost [keyAdvance, paths', push f actions]
      in do
        e <- explorerAdvance
        holdDyn Path.root =<< Signals.guard (leftmost [events, e])
    (e, _, boxes, paths', highlightRegion) <- elClass "div" "root" $
      DocView.widgets (Signals.dropWhen isExplorerOpen' keydown) (Signals.dropWhen isExplorerOpen') paths (Width 400) docs
    explorerTopLeft <- holdDyn (X 0, Y 0) $ (\(X x, Y y, _, Height h) -> (X x, Y $ y + h + 20)) <$> highlightRegion
    explorerResults <- Signals.offset "explorer-offset" explorerTopLeft . Signals.modal isExplorerOpen (never,never) $
                       TermExplorer.make node keydown (current state) (current paths) (current terms)
    state' <- Signals.switch' (fst <$> explorerResults)
    actions <- Signals.switch' (snd <$> explorerResults)
  pure ()

advancePath :: (Eq p, Path.Path p) => p -> Box Text (p,Region) -> Term V -> Maybe p
advancePath p box _ =
  Doc.findLeafAt (== "_") p (fst <$> box) <|>
  (Doc.right' box p >>= \p -> Doc.findLeafAt (== "_") p (fst <$> box)) <|>
  (Doc.down' box p >>= \p -> Doc.findLeafAt (== "_") p (fst <$> box))

main :: IO ()
main = mainWidget $ termEditor term
