module Unison.Runtime.Http where

import Data.Text
import Network.Curl (withCurlDo, CurlOption(..), curlGetString, CurlCode(..))

newtype HttpError = HttpError String deriving Show

get :: String -> IO (Either HttpError Text)
get url = withCurlDo $ do
  (code, response) <- curlGetString url [CurlFollowLocation True]
  pure $ case code of
    CurlOK -> Right $ pack response
    x -> Left . HttpError $ show x
