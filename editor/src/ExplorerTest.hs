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

-- narrow :: MonadWidget t m => [String] -> String -> Action m [String] String String
-- narrow possible q = Results [(name, True, name <$ text name) | name <- possible, take (length q) name == q]

{-
main2 :: IO ()
main2 = mainWidget $ mdo
  keydown <- UI.windowKeydown
  s <- holdDyn ["abra", "alice", "aardvark", "bob", "carol", "dave", "eve", "francis"] (updated s')
  prompt <- S.now (text "please select a name")
  (s', chosen) <- explorer keydown narrow prompt s
  pure ()
-}

main :: IO ()
main = mainWidget $ mdo
  keydown <- UI.windowKeydown
  n <- foldDyn (\a b -> a+b) (0::Int) press
  press <- pure $ pushAlways (\_ -> (1+) <$> sample (current n)) keydown
  display n
