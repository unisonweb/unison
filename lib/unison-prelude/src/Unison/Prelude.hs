module Unison.Prelude
  ( module X,
    readUtf8,
    safeReadUtf8,
    safeReadUtf8StdIn,
    writeUtf8,
    uncurry4,
    reportBug,
    tShow,
    wundefined,

    -- * @Maybe@ control flow
    onNothing,
    onNothingM,
    whenNothing,
    whenNothingM,
    whenJust,
    whenJustM,
    eitherToMaybe,
    maybeToEither,
    altSum,
    altMap,

    -- * @Either@ control flow
    onLeft,
    onLeftM,
    whenLeft,
    whenLeftM,
    throwEitherM,
    throwEitherMWith,
    throwExceptT,
    throwExceptTWith,
  )
where

import Control.Applicative as X
import Control.Category as X ((>>>))
import Control.Exception as X (Exception, IOException, SomeException)
import Control.Monad as X
import Control.Monad.Extra as X (ifM, mapMaybeM, unlessM, whenM)
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Trans as X (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Control.Monad.Trans.Maybe as X (MaybeT (MaybeT, runMaybeT))
import Data.ByteString as X (ByteString)
import Data.Coerce as X (Coercible, coerce)
import Data.Either as X
import Data.Either.Combinators as X (mapLeft, maybeToRight)
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Data.Foldable as X (asum, fold, foldl', for_, toList, traverse_)
import Data.Function as X ((&))
import Data.Functor as X
import Data.Functor.Identity as X
import Data.Int as X
import Data.List as X (foldl1', sortOn)
import Data.Map as X (Map)
import Data.Maybe as X (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text
import Data.Traversable as X (for)
import Data.Typeable as X (Typeable)
import Data.Void as X (Void)
import Data.Word as X
import Debug.Trace as X
import GHC.Generics as X (Generic, Generic1)
import qualified GHC.IO.Handle as Handle
import GHC.Stack as X (HasCallStack)
import Safe as X (atMay, headMay, lastMay, readMay)
import qualified System.IO as IO
import Text.Read as X (readMaybe)
import UnliftIO as X (MonadUnliftIO (..), askRunInIO, askUnliftIO, try, withUnliftIO)
import qualified UnliftIO
import Witch as X (From (from), TryFrom (tryFrom), TryFromException (TryFromException), into, tryInto)
import Witherable as X (filterA, forMaybe, mapMaybe, wither, witherMap)

-- | Like 'fold' but for Alternative.
altSum :: (Alternative f, Foldable t) => t (f a) -> f a
altSum = foldl' (<|>) empty

-- | Like 'foldMap' but for Alternative.
altMap :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
altMap f = altSum . fmap f . toList

-- | E.g.
--
-- @@
-- onNothing (throwIO MissingPerson) $ mayThing
-- @@
onNothing :: (Applicative m) => m a -> Maybe a -> m a
onNothing m may = maybe m pure may

onNothingM :: (Monad m) => m a -> m (Maybe a) -> m a
onNothingM =
  flip whenNothingM

-- | E.g. @maybePerson `whenNothing` throwIO MissingPerson@
whenNothing :: (Applicative m) => Maybe a -> m a -> m a
whenNothing may m = maybe m pure may

whenNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
whenNothingM mx my =
  mx >>= maybe my pure

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust mx f =
  maybe (pure ()) f mx

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mx f = do
  mx >>= maybe (pure ()) f

onLeft :: (Applicative m) => (a -> m b) -> Either a b -> m b
onLeft =
  flip whenLeft

onLeftM :: (Monad m) => (a -> m b) -> m (Either a b) -> m b
onLeftM =
  flip whenLeftM

whenLeft :: (Applicative m) => Either a b -> (a -> m b) -> m b
whenLeft = \case
  Left a -> \f -> f a
  Right b -> \_ -> pure b

whenLeftM :: (Monad m) => m (Either a b) -> (a -> m b) -> m b
whenLeftM m f =
  m >>= \case
    Left x -> f x
    Right y -> pure y

throwExceptT :: (MonadIO m, Exception e) => ExceptT e m a -> m a
throwExceptT = throwExceptTWith id

throwExceptTWith :: (MonadIO m, Exception e') => (e -> e') -> ExceptT e m a -> m a
throwExceptTWith f action =
  runExceptT (withExceptT f action) >>= \case
    Left e -> liftIO . UnliftIO.throwIO $ e
    Right a -> pure a

throwEitherM :: forall e m a. (MonadIO m, Exception e) => m (Either e a) -> m a
throwEitherM = throwEitherMWith id

throwEitherMWith :: forall e e' m a. (MonadIO m, Exception e') => (e -> e') -> m (Either e a) -> m a
throwEitherMWith f action = throwExceptT . withExceptT f $ (ExceptT action)

tShow :: (Show a) => a -> Text
tShow = Text.pack . show

-- | Strictly read an entire file decoding UTF8.
-- Converts \r\n -> \n on windows.
readUtf8 :: FilePath -> IO Text
readUtf8 fileName =
  UnliftIO.withFile fileName UnliftIO.ReadMode readUtf8Handle

-- | Strictly read from a handle, decoding UTF8, or failing if not valid UTF8
-- Converts \r\n -> \n on windows.
safeReadUtf8 :: FilePath -> IO (Either IOException Text)
safeReadUtf8 p = try (readUtf8 p)

-- | Strictly read from a handle, decoding UTF8.
-- Note, this changes the newline-mode of the handle
-- to convert \r\n -> \n on windows.
readUtf8Handle :: IO.Handle -> IO Text
readUtf8Handle handle = do
  Handle.hSetEncoding handle IO.utf8
  Text.hGetContents handle

-- | Strictly read from stdin, decoding UTF8.
-- Converts \r\n -> \n on windows.
safeReadUtf8StdIn :: IO (Either IOException Text)
safeReadUtf8StdIn = do
  handle <- Handle.hDuplicate IO.stdin
  try $ readUtf8Handle handle

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) =
  f a b c d

-- | Write a file strictly assuming UTF8
-- Converts \n -> \r\n on windows.
writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 fileName txt = do
  UnliftIO.withFile fileName UnliftIO.WriteMode $ \handle -> do
    Handle.hSetEncoding handle IO.utf8
    Text.hPutStr handle txt

reportBug :: String -> String -> String
reportBug bugId msg =
  unlines
    [ "🐞",
      "",
      msg,
      "",
      "This is a Unison bug and you can report it here:",
      "",
      "https://github.com/unisonweb/unison/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+" <> bugId <> "+",
      "",
      "Bug reference: " <> bugId,
      "",
      "If there's already an issue with this reference, you can give a 👍",
      "on the issue to let the team know you encountered it, and you can add",
      "any additional details you know of to the issue."
    ]

{-# WARNING wundefined "You left this wundefined." #-}
wundefined :: (HasCallStack) => a
wundefined = undefined
