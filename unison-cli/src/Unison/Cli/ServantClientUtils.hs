-- | servant-client utilities
module Unison.Cli.ServantClientUtils
  ( ConnectionError (..),
    classifyConnectionError,
  )
where

import Control.Exception (fromException)
import Network.HTTP.Client qualified as HttpClient
import System.IO.Error (isDoesNotExistError)
import Unison.Prelude

data ConnectionError
  = ConnectionError'Offline
  | ConnectionError'SomethingElse HttpClient.HttpExceptionContent
  | ConnectionError'SomethingEntirelyUnexpected SomeException

-- | Given a 'SomeException' from a @servant-client@ 'ClientError', attempt to classify what happened.
classifyConnectionError :: SomeException -> ConnectionError
classifyConnectionError exception0 =
  case fromException exception0 of
    Just (HttpClient.HttpExceptionRequest _request content) ->
      fromMaybe (ConnectionError'SomethingElse content) do
        case content of
          HttpClient.ConnectionFailure exception1 -> do
            ioException <- fromException @IOException exception1
            if
              | -- This may not be 100% accurate... but if the initial `getAddrInfo` request fails it will indeed throw
                -- a "does not exist" error. It seems in order to *know* that `getAddrInfo` was the cause of this
                -- exception, we'd have to parse the `show` output, which is preposterous.
                isDoesNotExistError ioException ->
                  Just ConnectionError'Offline
              | otherwise -> Nothing
          _ -> Nothing
    _ -> ConnectionError'SomethingEntirelyUnexpected exception0
