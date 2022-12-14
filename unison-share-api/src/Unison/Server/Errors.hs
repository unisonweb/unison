{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Errors where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Servant (ServerError (..), err400, err404, err409, err500)
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.ShortCausalHash as SCH
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Server.Backend as Backend
import Unison.Server.Types
  ( HashQualifiedName,
    munge,
    mungeShow,
    mungeString,
  )
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.HashQualified as HQ (toString)

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
  Backend.AmbiguousBranchHash sch hashes ->
    ambiguousNamespace (SCH.toText sch) (Set.map SCH.toText hashes)
  Backend.MissingSignatureForTerm r -> missingSigForTerm $ Reference.toText r
  Backend.NoSuchDefinition hqName -> noSuchDefinition hqName
  Backend.AmbiguousHashForDefinition shorthash -> ambiguousHashForDefinition shorthash

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

noSuchDefinition :: HQ.HashQualified Name -> ServerError
noSuchDefinition hqName =
  err404
    { errBody =
        "Couldn't find a definition for " <> BSC.pack (HQ.toString hqName)
    }

ambiguousHashForDefinition :: SH.ShortHash -> ServerError
ambiguousHashForDefinition shorthash =
  err400
    { errBody =
        "The hash prefix " <> BSC.pack (SH.toString shorthash) <> " is ambiguous"
    }
