{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Errors where

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Servant (ServerError (..), err400, err404, err500)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Path as Path
import qualified Unison.Server.Backend as Backend
import Unison.Server.Types
  ( HashQualifiedName,
    munge,
    mungeShow,
    mungeString,
  )

badHQN :: HashQualifiedName -> ServerError
badHQN hqn =
  err400
    { errBody =
        Text.encodeUtf8 (Text.fromStrict hqn)
          <> " is not a well-formed name, hash, or hash-qualified name. "
          <> "I expected something like `foo`, `#abc123`, or `foo#abc123`."
    }

backendError :: Backend.BackendError -> ServerError
backendError = \case
  Backend.NoSuchNamespace n ->
    noSuchNamespace . Path.toText $ Path.unabsolute n
  Backend.BadRootBranch e -> rootBranchError e
  Backend.NoBranchForHash h ->
    noSuchNamespace . Text.toStrict . Text.pack $ show h

rootBranchError :: Codebase.GetRootBranchError -> ServerError
rootBranchError rbe =
  err500
    { errBody = case rbe of
        Codebase.NoRootBranch -> "Couldn't identify a root namespace."
        Codebase.CouldntLoadRootBranch h ->
          "Couldn't load root branch " <> mungeShow h
        Codebase.CouldntParseRootBranch h ->
          "Couldn't parse root branch head " <> mungeShow h
    }

badNamespace :: String -> String -> ServerError
badNamespace err namespace =
  err400
    { errBody =
        "Malformed namespace: "
          <> mungeString namespace
          <> ". "
          <> mungeString err
    }

noSuchNamespace :: HashQualifiedName -> ServerError
noSuchNamespace namespace =
  err404
    { errBody =
        "The namespace "
          <> munge namespace
          <> " does not exist."
    }
