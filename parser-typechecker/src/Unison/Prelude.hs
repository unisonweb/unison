module Unison.Prelude
  ( module X, readUtf8, writeUtf8
  ) where

import Control.Applicative as X
import Control.Exception as X (Exception, SomeException)
import Control.Monad as X
import Control.Monad.Extra as X (ifM, unlessM, whenM)
import Control.Monad.IO.Class as X (MonadIO(liftIO))
import Control.Monad.Trans as X (MonadTrans(lift))
import Control.Monad.Trans.Maybe as X (MaybeT(MaybeT, runMaybeT))
import Data.ByteString as X (ByteString)
import Data.Either as X
import Data.Either.Combinators as X (mapLeft, maybeToRight)
import Data.Foldable as X (asum, fold, foldl', for_, forM_, toList, traverse_)
import Data.Functor as X
import Data.Int as X
import Data.Map as X (Map)
import Data.Maybe as X (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.String as X (IsString, fromString)
import Data.Text as X (Text)
import Data.Text.Encoding as X (encodeUtf8, decodeUtf8)
import Data.Traversable as X (for)
import Data.Word as X
import Debug.Trace as X
import GHC.Generics as X (Generic, Generic1)
import Safe as X (atMay, headMay, lastMay, readMay)
import Text.Read as X (readMaybe)

import qualified Data.ByteString as BS

-- Read an entire file strictly assuming UTF8
readUtf8 :: FilePath -> IO Text
readUtf8 p = decodeUtf8 <$> BS.readFile p

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 p txt = BS.writeFile p (encodeUtf8 txt) 
