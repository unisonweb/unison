{-# Language GADTs #-}

module Unison.Runtime.Stream where

import Control.Monad
import Data.Either
import Data.Maybe
import Unison.Runtime.Free (Free)
import qualified Unison.Runtime.Free as Free
import Prelude hiding (head)

data StreamF f a r where
  Effect :: f r -> StreamF f a r
  Emit :: a -> StreamF f a ()

type Stream f a r = Free (StreamF f a) r

eval :: f r -> Stream f a r
eval fr = Free.eval (Effect fr)

append :: Stream f a r1 -> Stream f a r2 -> Stream f a r2
append s1 s2 = s1 >> s2

empty :: Stream f a ()
empty = pure ()

emit :: a -> Stream f a ()
emit a = Free.eval (Emit a)

emits :: [a] -> Stream f a ()
emits as = foldr append (pure ()) (map emit as)

uncons :: Stream f a r -> Free f (Either r (a, Stream f a r))
uncons s = case s of
  Free.Pure r -> pure (Left r)
  Free.Bind (Effect fx) k -> Free.eval fx >>= (uncons . k)
  Free.Bind (Emit a) k -> pure (Right (a, k ()))

head :: Stream f a r -> Free f (Maybe a)
head s = either (const Nothing) (Just . fst) <$> uncons s

headOr :: a -> Stream f a r -> Free f a
headOr a s = fromMaybe a <$> head s

mapEmits :: (a -> b) -> Stream f a r -> Stream f b r
mapEmits f s = case s of
  Free.Pure r -> pure r
  Free.Bind (Effect fx) k -> Free.Bind (Effect fx) (mapEmits f . k)
  Free.Bind (Emit a) k -> Free.Bind (Emit (f a)) (mapEmits f . k)

bindEmits :: (a -> Stream f b r) -> Stream f a r -> Stream f b r
bindEmits f s = case s of
  Free.Pure r -> pure r
  Free.Bind (Effect fx) k -> Free.Bind (Effect fx) (bindEmits f . k)
  Free.Bind (Emit a) k -> f a `append` bindEmits f (k ())
