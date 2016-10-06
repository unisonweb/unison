module Unison.Note where

import Data.List
import Control.Monad
import Control.Applicative

-- | Hierarchical error message type used throughout Unison
newtype Note = Note [String]

-- | Monad transformer for adding notes
newtype Noted m a = Noted { unnote :: m (Either Note a) }

run :: Monad m => Noted m a -> m a
run (Noted m) = m >>= \e -> case e of
  Left (Note stack) -> fail ("\n" ++ intercalate "\n" stack)
  Right a -> return a

attemptRun :: Functor m => Noted m a -> m (Either String a)
attemptRun n = collapse <$> unnote n where
  collapse (Left (Note stack)) = Left ("\n" ++ intercalate "\n" stack)
  collapse (Right a) = Right a

noted :: m (Either Note a) -> Noted m a
noted = Noted

fromEither :: Applicative m => Either Note a -> Noted m a
fromEither = Noted . pure

fromMaybe :: Applicative m => String -> Maybe a -> Noted m a
fromMaybe msg Nothing = failure msg
fromMaybe _ (Just a) = pure a

noted' :: Functor m => String -> m (Maybe a) -> Noted m a
noted' ifNothing moa = noted (fmap (maybe (Left (note ifNothing)) Right) moa)

lift :: Functor m => m a -> Noted m a
lift = Noted . fmap Right

failure :: Applicative m => String -> Noted m a
failure = Noted . pure . Left . note

scoped :: Functor m => String -> Noted m a -> Noted m a
scoped msg inner = Noted $ fmap (scope msg) (unnote inner)

orElse :: Monad m => Noted m a -> Noted m a -> Noted m a
orElse a b = Noted $ unnote a >>= go
  where go (Left _) = unnote b
        go (Right a) = return (Right a)

instance Monad m => Monad (Noted m) where
  return = Noted . return . return
  fail s = Noted . return . Left . note $ s
  Noted a >>= f = Noted $ a >>= \e -> case e of
    Left e -> return $ Left e
    Right a -> unnote (f a)

instance Functor m => Functor (Noted m) where
  fmap f (Noted a) = Noted $ fmap go a where
    go (Left e) = Left e
    go (Right a) = Right (f a)

instance Applicative m => Applicative (Noted m) where
  pure = Noted . pure . pure
  (Noted f) <*> (Noted a) = Noted $ liftA2 (<*>) f a

instance Monad m => MonadPlus (Noted m) where
  mzero = Noted (pure (Left (Note [])))
  mplus (Noted n1) (Noted n2) = Noted $ do
    n1 <- n1
    case n1 of
      Left _ -> n2
      Right a -> pure (Right a)

instance Monad m => Alternative (Noted m) where
  empty = mzero
  (<|>) = mplus

note :: String -> Note
note s = Note [s]

note' :: String -> Maybe a -> Either Note a
note' s Nothing = Left (note s)
note' _ (Just a) = Right a

scope :: String -> Either Note a -> Either Note a
scope s (Left (Note stack)) = Left (Note (s : stack))
scope _ e = e

scopeM :: Monad m => String -> m (Either Note a) -> m (Either Note a)
scopeM s = liftM (scope s)

scopeF :: Functor f => String -> f (Either Note a) -> f (Either Note a)
scopeF s = fmap (scope s)

instance Show Note where
  show (Note stack) = intercalate "\n" stack
