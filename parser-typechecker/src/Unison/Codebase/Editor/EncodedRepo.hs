{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.EncodedRepo where

import Data.Text (Text)
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo(GitRepo))

-- Sort of struggling to define this model, but the basic idea is that we 
-- anticipate various styles of strings that would refer to Unison repos
-- hosted in different places.
-- 
-- Even given a particular Git webhosting service, there are multiple styles of 
-- URL that could refer to the same Unison namespace, so it's nice to have a decoded representation:
-- e.g.
--  https://github.com/unisonweb/base
--  https://github.com/unisonweb/base/
--  https://github.com/unisonweb/base.git
--  git@github.com:unisonweb/base.git
--  https://github.com/unisonweb/base/blob/master/.unison/v1/paths/t9hpnm3ra68lo1ct5n67jn6h5jrdsrdenu95vanq3vo86hhhdsi028evebclpq5ucultsm9ufofrlrknq2panfcuma8poii2u0bff9o.ub
--  https://github.com/unisonweb/base/blob/master/.unison/v1/paths/t9hpnm3ra68lo1ct5n67jn6h5jrdsrdenu95vanq3vo86hhhdsi028evebclpq5ucultsm9ufofrlrknq2panfcuma8poii2u0bff9o.ub?raw=true
--  https://github.com/unisonweb/base/raw/master/.unison/v1/paths/t9hpnm3ra68lo1ct5n67jn6h5jrdsrdenu95vanq3vo86hhhdsi028evebclpq5ucultsm9ufofrlrknq2panfcuma8poii2u0bff9o.ub
--  github-mac://openRepo/https://github.com/unisonweb/base?branch=master&filepath=.unison%2Fv1%2Fpaths%2Ft9hpnm3ra68lo1ct5n67jn6h5jrdsrdenu95vanq3vo86hhhdsi028evebclpq5ucultsm9ufofrlrknq2panfcuma8poii2u0bff9o.ub
--  https://api.github.com/repos/unisonweb/base/contents/.unison/v1/paths/t9hpnm3ra68lo1ct5n67jn6h5jrdsrdenu95vanq3vo86hhhdsi028evebclpq5ucultsm9ufofrlrknq2panfcuma8poii2u0bff9o.ub?ref=master
--  https://api.github.com/repos/unisonweb/base/git/blobs/cb6f7f679b801a15c59d19fac08de18fb8e942ab
--  https://raw.githubusercontent.com/unisonweb/base/master/.unison/v1/paths/t9hpnm3ra68lo1ct5n67jn6h5jrdsrdenu95vanq3vo86hhhdsi028evebclpq5ucultsm9ufofrlrknq2panfcuma8poii2u0bff9o.ub
--  github:unisonweb/base:#t9hpnm
data EncodedRepo
  = UserSlugRepo { service :: UserSlugService
                 , user :: Text
                 , repo :: Text
                 , commit :: Maybe Text }
  | OtherRepo { url :: Text, commit :: Maybe Text}
  deriving (Eq, Ord, Show)
  
data UserSlugService = Github | Gitlab | Bitbucket
  deriving (Eq, Ord, Show)

-- Unfortunately there are multiple, non-interchangeable ways of accessing a 
-- particular webhosted repo; namely https- and ssh-based methods.  Ssh mode 
-- requires that the user have an account with the specific git hosting service;
-- https mode is used mainly-but-not-exclusively for read-only access.
-- 
-- If the user isn't explicitly stating the protocol to use, we end up having to
-- guess, or read a preference out of .unisonConfig.
asSsh :: EncodedRepo -> RemoteRepo
asSsh = \case
  UserSlugRepo{..} -> 
    GitRepo ("git@" <> domain service <> ":" <> user <> "/" <> repo <> ".git")
            commit
  OtherRepo{..} -> GitRepo url commit
  
asHttps :: EncodedRepo -> RemoteRepo
asHttps = \case
  UserSlugRepo{..} -> 
    GitRepo ("https://" <> domain service 
                        <> "/" <> user 
                        <> "/" <> repo <> ".git") commit
  OtherRepo{..} -> GitRepo url commit

domain :: UserSlugService -> Text
domain = \case
  Github -> "github.com"
  Gitlab -> "gitlab.com"
  Bitbucket -> "bitbucket.com"