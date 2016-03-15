{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Debug.Trace as Trace
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
import Unison.Paths (Path)
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
        advanced (Just (_,TermExplorer.Advance)) = True
        advanced (Just (_,TermExplorer.Insert)) = True
        advanced _ = False
        events = leftmost
          [ void $ Signals.enter keydown
          , void $ ffilter (== 79) keydown -- [o]pen
          , void $ clickDoc ]
        event as = leftmost [ as, Signals.dropWhen isExplorerOpen' events ]
      -- hack to ensure actions has a chance to update the layout and new path before reopening
      in event <$> Signals.waitFor' (updated docs) (void $ ffilter advanced actions)
    isExplorerOpen <- holdDyn False $
      -- todo, might want to pause a bit if the explorer is closed due to an advance event
      -- okay, so basically, every time there is a `b`, you grab the last `a` value
      leftmost [ True <$ openEvent, Signals.keepWhen isExplorerOpen' (False <$ actions) ]
    let isExplorerOpen' = current isExplorerOpen
    clickDoc <- Signals.switch' (maybe never (domEvent Click) <$> updated e)
    state <- foldDyn (<>) (TermExplorer.S symbols0) state'
    docs <- id $
      let f term = sample (current state) >>= \(TermExplorer.S{..}) -> pure $ Views.termMd metadata term
      in mapDynM f terms
    terms <- id $
      let
        f (Just (a, nav)) oldTerm = case a of
          TermExplorer.Replace p term ->
            let
              msg = "replacing: " ++ show p ++ " with " ++ show term ++ " in\n " ++ show oldTerm
              insert p t | nav == TermExplorer.Insert = Paths.insertTerm p t
              insert _ t = Just t
            in Trace.trace msg $ fromMaybe oldTerm $ insert p =<< Paths.modifyTerm (const (Term.wrapV term)) p oldTerm
          _ -> error "todo: Eval + Step"
        f _ oldTerm = oldTerm
      in foldDyn f term0 actions
    wraps <- pure $ -- typing 'a' replaces current selection, x, with _ x
      let
        a _ = f <$> sample (current paths) <*> sample (current terms)
        f p t = Paths.atTerm p t >>= \t ->
          pure (Just (TermExplorer.Replace p (Term.blank `Term.app` t), TermExplorer.Advance))
      in
        push a (void $ Signals.dropWhen isExplorerOpen' $ ffilter (== 65) keydown)
    paths <- id $
      let
        a _ = advancePath <$> sample (current paths) <*> sample boxes <*> sample (current terms)
        ok (Just (_, TermExplorer.Advance)) = a ()
        ok (Just (_, TermExplorer.Insert)) = a ()
        ok _ = pure Nothing
        f (Just (TermExplorer.Replace p _, _)) = pure (Just p)
        f Nothing = Just <$> sample (current paths) -- refresh the path event on cancel
        f _ = pure Nothing
      in do
        advance <- push ok <$> Signals.waitFor' (updated docs) actions
        replace <- push f <$> Signals.guard actions
        holdDyn Path.root $ leftmost [paths', replace, advance]
    (e, _, boxes, paths', highlightRegion) <- elClass "div" "root" $
      DocView.widgets (Signals.dropWhen isExplorerOpen' keydown) (Signals.dropWhen isExplorerOpen') paths (Width 400) docs
    explorerTopLeft <- holdDyn (X 0, Y 0) $ (\(X x, Y y, _, Height h) -> (X x, Y $ y + h + 20)) <$> highlightRegion
    explorerResults <- Signals.offset "explorer-offset" explorerTopLeft . Signals.modal isExplorerOpen (never,never) $
                       TermExplorer.make node keydown (current state) (current paths) (current terms)
    state' <- Signals.switch' (fst <$> explorerResults)
    actions <- (\a -> leftmost [wraps,a]) <$> Signals.switch' (snd <$> explorerResults)
  pure ()

-- | Looks for leftmost, uppermost `Term.blank` relative to the current path.
advancePath :: Path -> Box Text (Path,Region) -> Term V -> Maybe Path
advancePath p box term =
  let
    isBlank p = maybe False (== Term.blank) (Paths.atTerm p term)
    scanHorizontal _ p | isBlank p = Just p
    scanHorizontal radius p = maybe (scanVertical radius p) (scanHorizontal radius) (Doc.right' box p)
    scanVertical 0 _ = Nothing
    scanVertical radius p = scanHorizontal (radius-1) =<< Doc.down' box p
    p' = Doc.contains box (Doc.region box p)
    p0 = leaf p'
    leaf p = maybe p leaf (Doc.contract' box p)
  in
    if isBlank p0 && p /= p0 then Just p0
    else scanHorizontal (3::Int) p0

main :: IO ()
main = mainWidget $ termEditor term
