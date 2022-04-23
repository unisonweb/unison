module Unison.Util.HTTP (addRequestMiddleware, setUserAgent, ucmUserAgent) where

import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import Unison.Codebase.Editor.Command (UCMVersion)
import Unison.Prelude

addRequestMiddleware :: (HTTP.Request -> IO HTTP.Request) -> HTTP.ManagerSettings -> HTTP.ManagerSettings
addRequestMiddleware f man =
  man {HTTP.managerModifyRequest = (HTTP.managerModifyRequest man) >=> f}

ucmUserAgent :: UCMVersion -> Text
ucmUserAgent ucmVersion = "UCM/" <> ucmVersion

setUserAgent :: Text -> HTTP.ManagerSettings -> HTTP.ManagerSettings
setUserAgent userAgent =
  let addUserAgent req = do
        pure $ req {HTTP.requestHeaders = ("User-Agent", Text.encodeUtf8 userAgent) : HTTP.requestHeaders req}
   in addRequestMiddleware addUserAgent
