{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The strict building blocks of the code generator: packed text, strict
-- pairs, and the few operations on them that "Unison.Util.Text" and
-- "Unison.Util.Deque" don't have.
--
-- The generator's state and everything it produces are made of these, of
-- 'Deque' (a finger tree with no laziness anywhere) and of strict record
-- fields, so that forcing the state to weak head normal form forces all of
-- it: no thunk over the text of a module outlives the step that made it,
-- and the text is packed from the start (a 'Text' is a rope of packed
-- chunks, so appending to a long one doesn't copy it).
module Unison.Runtime.JIT.Strict
  ( Text,
    Pair (..),
    Triple (..),
    tshow,
    unlinesT,
    intercalateT,
    iforM_,
  )
where

import Control.Monad (foldM_)
import Unison.Util.Deque (Deque)
import Unison.Util.Text (Text)
import Unison.Util.Text qualified as UT

data Pair a b = Pair !a !b

data Triple a b c = Triple !a !b !c

tshow :: (Show a) => a -> Text
tshow = UT.pack . show

-- | Every element followed by a newline.
unlinesT :: Deque Text -> Text
unlinesT = foldl' (\acc l -> acc <> l <> "\n") mempty

-- | The elements with the separator between each two.
intercalateT :: Text -> Deque Text -> Text
intercalateT sep xs = case foldl' step (Pair False mempty) xs of Pair _ t -> t
  where
    step (Pair False _) x = Pair True x
    step (Pair True acc) x = Pair True (acc <> sep <> x)

-- | 'forM_' with the element's position.
iforM_ :: (Monad m) => Deque a -> (Int -> a -> m ()) -> m ()
iforM_ xs f = foldM_ (\ !i x -> f i x >> pure (i + 1)) 0 xs
