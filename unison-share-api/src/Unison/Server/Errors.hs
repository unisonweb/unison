{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Errors where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Servant (ServerError (..), err400, err404, err409, err500)
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Reference as Reference
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
  Backend.BadNamespace err namespace -> badNamespace err namespace
  Backend.NoBranchForHash h ->
    noSuchNamespace . Text.toStrict . Text.pack $ show h
  Backend.CouldntLoadBranch h ->
    couldntLoadBranch h
  Backend.CouldntExpandBranchHash h ->
    noSuchNamespace . Text.toStrict . Text.pack $ show h
  Backend.AmbiguousBranchHash sbh hashes ->
    ambiguousNamespace (SBH.toText sbh) (Set.map SBH.toText hashes)
  Backend.MissingSignatureForTerm r -> missingSigForTerm $ Reference.toText r

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
  err404 {errBody = "The namespace " <> munge namespace <> " does not exist."}

couldntLoadBranch :: Causal.CausalHash -> ServerError
couldntLoadBranch h =
  err404
    { errBody =
        "The namespace "
          <> munge (Text.toStrict . Text.pack $ show h)
          <> " exists but couldn't be loaded."
    }

ambiguousNamespace :: HashQualifiedName -> Set HashQualifiedName -> ServerError
ambiguousNamespace name namespaces =
  err409
    { errBody =
        "Ambiguous namespace reference: "
          <> munge name
          <> ". It could refer to any of "
          <> mungeShow (Set.toList namespaces)
    }

missingSigForTerm :: HashQualifiedName -> ServerError
missingSigForTerm r =
  err500
    { errBody =
        "The type signature for reference "
          <> munge r
          <> " is missing! "
          <> "This means something might be wrong with the codebase, "
          <> "or the term was deleted just now. "
          <> "Try making the request again."
    }
