{-# LANGUAGE DeriveGeneric #-}
module Unison.Server.Assorted where


import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as C8
import qualified Network.URI.Encode as URI

import Network.Wai.Handler.Warp
  ( Port,
  )


-- BaseUrl and helpers

data BaseUrl = BaseUrl
  { urlHost :: String,
    urlToken :: Strict.ByteString,
    urlPort :: Port
  }
instance Show BaseUrl where
  show url = urlHost url <> ":" <> show (urlPort url) <> "/" <> (URI.encode . C8.unpack . urlToken $ url)

data BaseUrlPath = UI | Api

urlFor :: BaseUrlPath -> BaseUrl -> String
urlFor path baseUrl =
  case path of
    UI -> show baseUrl <> "/ui"
    Api -> show baseUrl <> "/api"



data CodebaseServerOpts = CodebaseServerOpts
  { token :: Maybe String
  , host :: Maybe String
  , port :: Maybe Int
  , codebaseUIPath :: Maybe FilePath
  } deriving (Show, Eq)


ucmUIVar :: String
ucmUIVar = "UCM_WEB_UI"

ucmPortVar :: String
ucmPortVar = "UCM_PORT"

ucmHostVar :: String
ucmHostVar = "UCM_HOST"

ucmTokenVar :: String
ucmTokenVar = "UCM_TOKEN"
