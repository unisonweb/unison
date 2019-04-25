{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Branch2 where

-- import qualified Unison.Codebase.Branch as Branch

import           Prelude                  hiding (head,subtract)

import           Control.Lens
-- import           Control.Monad            (join)
-- import           Data.GUID                (genText)
import           Data.List                (intercalate)
import qualified Data.Map                 as Map
import           Data.Map                 (Map)
import           Data.Text                (Text)
import qualified Data.Text as Text

import qualified Unison.Codebase.Causal2  as Causal
import           Unison.Codebase.Causal2   (Causal)
import           Unison.Codebase.TermEdit (TermEdit)
import           Unison.Codebase.TypeEdit (TypeEdit)
import           Unison.Codebase.Path     (NameSegment, Path(Path))
import qualified Unison.Codebase.Path as Path
import           Unison.Hash              (Hash)
import           Unison.Hashable (Hashable)
import qualified Unison.Hashable as H
import           Unison.Reference         (Reference)
import           Unison.Referent          (Referent)
import qualified Unison.Util.Relation     as R
import           Unison.Util.Relation     (Relation)

-- data RepoRef
--   = Local
--   | Github { username :: Text, repo :: Text, commit :: Text }
--   deriving (Eq, Ord, Show)
--
-- type EditGuid = Text
--
-- data RepoLink a = RepoLink RepoRef a
--   deriving (Eq, Ord, Show)
--
-- type Link = RepoLink Hash
-- type EditLink = RepoLink EditGuid
--
-- data UnisonRepo = UnisonRepo
--   { _rootNamespace :: Link
--   , _editMap :: EditMap
--   , _editNames :: Relation Text EditGuid
--   } deriving (Eq, Ord, Show)
--
-- data Edits = Edits
--   { _termEdits :: Relation Reference TermEdit
--   , _typeEdits :: Relation Reference TypeEdit
--   } deriving (Eq, Ord, Show)

-- newtype EditMap =
--   EditMap { toMap :: Map EditGuid (Causal Edits) }
--   deriving (Eq, Ord, Show)
--
-- type FriendlyEditNames = Relation Text EditGuid

-- newtype Branch = Branch
--   { _history :: Causal Identity Branch0 }
--
-- data Branch0 = Branch0
--   { _terms :: Relation NameSegment Referent
--   , _types :: Relation NameSegment Reference
--   , _children :: Map NameSegment Link
--   }

-- data Codebase' = Codebase'
--   { namespaceRoot :: Branch
--   , edits :: ???
--   }

newtype Branch m = Branch (Causal m (Branch0 m))

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

data Branch0 m = Branch0
  { _terms' :: Relation NameSegment Reference
  , _types' :: Relation NameSegment Reference
  , _children' :: Map NameSegment (Hash, m (Branch m))
  }

makeLenses ''Branch0

-- copy a path to another path
data ForkFailure = SrcNotFound | DestExists
fork
  :: Monad m => Branch m -> Path -> Path -> m (Either ForkFailure (Branch m))
fork root src dest = do
  -- descend from root to src to get a Branch srcBranch
  getAt root src >>= \case
    Nothing -> pure $ Left SrcNotFound
    Just src' -> setIfNotExists root dest src' >>= \case
      Nothing -> pure $ Left DestExists
      Just root' -> pure $ Right root'

setIfNotExists
  :: Monad m => Branch m -> Path -> Branch m -> m (Maybe (Branch m))
setIfNotExists root dest b =
  getAt root dest >>= \case
    Just _destExists -> pure Nothing
    Nothing -> Just <$> modifyAt root dest (const . pure $ b)

getAt :: Monad m
      => Branch m
      -> Path
      -> m (Maybe (Branch m))
-- todo: return Nothing if exists but is empty
getAt root path = case Path.toList path of
  [] -> pure $ Just root
  seg : path -> case Map.lookup seg (_children' . head $ root) of
    Nothing -> pure Nothing
    Just (_h, m) -> do
      root <- m
      getAt root (Path path)

empty :: Branch m
empty = Branch $ Causal.one (Branch0 mempty mempty mempty)

--
modifyAt :: Monad m
         => Branch m
         -> Path
         -> (Branch m -> m (Branch m))
         -> m (Branch m)
modifyAt root path f = case Path.toList path of
  [] -> f root
  seg : path ->
    let recurse root'@(Branch c) = do
          b <- modifyAt root' (Path path) f
          let h = Causal.hash (head b)
          pure . Branch . Causal.step
            (over children' $ Map.insert seg (h, pure b)) $ c
    in case Map.lookup seg (_children' . head $ root) of
      Nothing -> recurse empty
      Just (_h, m) -> m >>= recurse

instance Hashable (Branch0 m) where
  tokens b =
    [ H.accumulateToken . R.toList $ (_terms' b)
    , H.accumulateToken . R.toList $ (_types' b)
    , H.accumulateToken (fst <$> _children' b)
    ]

-- getLocalBranch :: Hash -> IO Branch
-- getGithubBranch :: RemotePath -> IO Branch
-- getLocalEdit :: GUID -> IO Edits

-- makeLenses ''Namespace
-- makeLenses ''Edits
-- makeLenses ''Branch
-- makeLenses ''Causal
