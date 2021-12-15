{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Unison.Prelude
  ( module X,
    readUtf8,
    safeReadUtf8,
    safeReadUtf8StdIn,
    writeUtf8,
    uncurry4,
    reportBug,
    tShow,

    -- * @Maybe@ control flow
    onNothing,

    -- * @Either@ control flow
    whenLeft,
  )
where

import Control.Applicative as X
import Control.Category as X ((>>>))
import Control.Exception as X (Exception, IOException, SomeException, try)
import Control.Monad as X
import Control.Monad.Extra as X (ifM, mapMaybeM, unlessM, whenM)
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Trans as X (MonadTrans (lift))
import Control.Monad.Trans.Maybe as X (MaybeT (MaybeT, runMaybeT))
import Data.ByteString as X (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce as X (Coercible, coerce)
import Data.Either as X
import Data.Either.Combinators as X (mapLeft, maybeToRight)
import Data.Foldable as X (asum, fold, foldl', for_, toList, traverse_)
import Data.Function as X ((&))
import Data.Functor as X
import Data.Int as X
import Data.List as X (foldl1', sortOn)
import Data.Map as X (Map)
import Data.Maybe as X (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
import Data.Traversable as X (for)
import Data.Typeable as X (Typeable)
import Data.Word as X
import Debug.Trace as X
import GHC.Generics as X (Generic, Generic1)
import GHC.Stack as X (HasCallStack)
import Safe as X (atMay, headMay, lastMay, readMay)
import Text.Read as X (readMaybe)

onNothing :: Applicative m => m a -> Maybe a -> m a
onNothing x =
  maybe x pure

whenLeft :: Applicative m => Either a b -> (a -> m b) -> m b
whenLeft = \case
  Left a -> \f -> f a
  Right b -> \_ -> pure b

tShow :: Show a => a -> Text
tShow = Text.pack . show

-- Read an entire file strictly assuming UTF8
readUtf8 :: FilePath -> IO Text
readUtf8 p = decodeUtf8 <$> BS.readFile p

safeReadUtf8 :: FilePath -> IO (Either IOException Text)
safeReadUtf8 p = try (readUtf8 p)

safeReadUtf8StdIn :: IO (Either IOException Text)
safeReadUtf8StdIn = try $ decodeUtf8 <$> BS.getContents

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) =
  f a b c d

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 p txt = BS.writeFile p (encodeUtf8 txt)

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
