{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.CommandLine.Server.API where

import Servant.API
import Unison.CommandLine.Server.Types

type CommandLineAPI =
  "run" :> ReqBody '[JSON] RunCommandRequest :> Post '[JSON] RunCommandResponse
