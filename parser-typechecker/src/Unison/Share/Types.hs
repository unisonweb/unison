{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Share.Types
  ( CodeserverURI (..),
    CodeserverId (..),
    Scheme (..),
    codeserverFromURI,
    codeserverIdFromURI,
    codeserverToURI,
    codeserverIdFromCodeserverURI,
    codeserverBaseURL,
  )
where

import Data.Aeson
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Text
import qualified Data.Text as Text
import Network.URI
import qualified Servant.Client as Servant
import Unison.Prelude

data Scheme = Http | Https
  deriving (Eq, Ord, Show)

-- | This type is expanded out into all of its fields because we require certain pieces
-- which are optional in a URI, and also to make it more typesafe to eventually convert into a
-- BaseURL for servant clients.
data CodeserverURI = CodeserverURI
  { codeserverScheme :: Scheme,
    codeserverUserInfo :: String,
    codeserverRegName :: String,
    codeserverPort :: Int,
    codeserverPath :: [String]
  }
  deriving stock (Eq, Ord)

instance Show CodeserverURI where
  show = show . codeserverToURI

codeserverToURI :: CodeserverURI -> URI
codeserverToURI cs@(CodeserverURI {..}) =
  let scheme = case codeserverScheme of
        Http -> "http:"
        Https -> "https:"
      authority = codeserverAuthority cs
   in URI
        { uriScheme = scheme,
          uriAuthority = Just authority,
          uriPath = case codeserverPath of
            [] -> ""
            segs -> "/" <> List.intercalate "/" segs,
          uriQuery = "",
          uriFragment = ""
        }

codeserverAuthority :: CodeserverURI -> URIAuth
codeserverAuthority (CodeserverURI {..}) =
  URIAuth
    { uriUserInfo = codeserverUserInfo,
      uriPort = ":" <> show codeserverPort,
      uriRegName = codeserverRegName
    }

codeserverFromURI :: URI -> Maybe CodeserverURI
codeserverFromURI URI {..} = do
  URIAuth {uriUserInfo, uriRegName, uriPort} <- uriAuthority
  scheme <- case uriScheme of
    "http:" -> Just Http
    "https:" -> Just Https
    _ -> Nothing
  port <- case uriPort of
    "" -> case scheme of
      Http -> Just 80
      Https -> Just 443
    (':' : p) -> readMaybe p
    _ -> Nothing
  pure $
    CodeserverURI
      { codeserverScheme = scheme,
        codeserverUserInfo = uriUserInfo,
        codeserverRegName = uriRegName,
        codeserverPort = port,
        codeserverPath = List.splitOn "/" . Prelude.dropWhile (== '/') $ uriPath
      }

-- | This is distinct from the codeserver URI in that we store credentials by a normalized ID, since it's
-- much easier to look up that way than from an arbitrary path.
-- We may wish to use explicitly named configurations in the future.
-- This currently uses a stringified uriAuthority.
newtype CodeserverId = CodeserverId {codeserverId :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Gets the part of the CodeserverURI that we use for identifying that codeserver in
-- credentials files.
--
-- >>> import Data.Maybe (fromJust)
-- >>> import Network.URI (parseURI)
-- >>> codeserverIdFromURI (CodeserverURI . fromJust $ parseURI "http://localhost:5424/api")
-- >>> codeserverIdFromURI (CodeserverURI . fromJust $ parseURI "https://share.unison-lang.org/api")
-- Right "localhost"
-- Right "share.unison-lang.org"
codeserverIdFromURI :: URI -> Either Text CodeserverId
codeserverIdFromURI uri =
  case uriAuthority uri of
    Nothing -> Left $ "No URI Authority for URI " <> tShow uri
    Just ua -> pure $ codeserverIdFromURIAuth ua

codeserverIdFromURIAuth :: URIAuth -> CodeserverId
codeserverIdFromURIAuth ua =
  (CodeserverId (Text.pack $ uriUserInfo ua <> uriRegName ua <> uriPort ua))

codeserverIdFromCodeserverURI :: CodeserverURI -> CodeserverId
codeserverIdFromCodeserverURI =
  codeserverIdFromURIAuth . codeserverAuthority

codeserverBaseURL :: CodeserverURI -> Servant.BaseUrl
codeserverBaseURL (CodeserverURI {..}) =
  let scheme = case codeserverScheme of
        Https -> Servant.Https
        Http -> Servant.Http
      host = codeserverUserInfo <> codeserverRegName <> ":" <> show codeserverPort
   in Servant.BaseUrl scheme host codeserverPort (List.intercalate "/" codeserverPath)
