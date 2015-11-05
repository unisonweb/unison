{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom
import Unison.Explorer as Explorer
import qualified Unison.Signals as S
import qualified Unison.UI as UI

--data Action m s k a
--  = Request (m s) [(k, Bool, m a)]
--  | Results [(k, Bool, m a)]
--  | Cancel
--  | Accept a

narrow :: MonadWidget t m => [String] -> String -> Action m [String] String String
narrow possible q = Results [(name, True, name <$ text name) | name <- possible, take (length q) name == q]

main :: IO ()
main = mainWidget $ mdo
  keydown <- UI.windowKeydown
  s <- holdDyn ["abra", "alice", "aardvark", "bob", "carol", "dave", "eve", "francis"] (updated s')
  prompt <- S.now (text "please select a name")
  (s', chosen) <- explorer keydown narrow prompt s
  pure ()

--explorer :: forall t m k s a . (Reflex t, MonadWidget t m, Eq k, Semigroup s)
--         => Event t Int
--         -> (s -> String -> Action m s k a)
--         -> Event t (m ()) -- loaded asynchronously on open of explorer
--         -> Dynamic t s
--         -> m (Dynamic t s, Event t (Maybe a))
