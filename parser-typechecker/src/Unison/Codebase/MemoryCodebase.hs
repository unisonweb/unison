{-# LANGUAGE TemplateHaskell     #-}

module Unison.Codebase.MemoryCodebase where

import Control.Lens
import Control.Lens.TH (makeLenses)
import Control.Monad.State.Class (MonadState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Unison.Codebase (Codebase(..), Decl, Term, Type)
import Unison.Codebase.Branch (Branch)
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.UnisonFile (WatchKind)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Data.Set as Set
import Data.Foldable (for_)
import qualified Unison.DataDeclaration as DD

data MemoryCodebase m v a = MemoryCodebase
  { _terms :: Map Reference.Id (Term v a, Type v a)
  , _decls :: Map Reference.Id (Decl v a)
  , _rootBranch :: Branch m
  , _dependents :: Map Reference (Set Reference.Id)
  , _watches'    :: Map (WatchKind, Reference.Id) (Term v a)
  }
makeLenses ''MemoryCodebase


codebase :: MonadState (MemoryCodebase m v a) m
         => Ord v
         => Codebase m v a
codebase = Codebase
  { getTerm            = \id -> fmap fst . Map.lookup id <$> use terms
  , getTypeOfTermImpl  = \id -> fmap snd . Map.lookup id <$> use terms
  , getTypeDeclaration = \id -> Map.lookup id <$> use decls
  , putTerm            = \id term typ -> do
      terms %= Map.insert id (term, typ)
      for_ (deleteComponent id (Term.dependencies term) <> Type.dependencies typ)
        $ \r -> dependents %= Map.insertWith (<>) r (Set.singleton id)
  , putTypeDeclaration = \id decl -> do
      decls %= Map.insert id decl
      for_ (deleteComponent id (DD.declDependencies decl))
        $ \r -> dependents %= Map.insertWith (<>) r (Set.singleton id)
  , getRootBranch      = use rootBranch
  , putRootBranch      = (rootBranch .=)
  , rootBranchUpdates  = pure (pure (), error "impossible to delay without IO")
  , dependentsImpl     = \r -> fromMaybe mempty . Map.lookup r <$> use dependents
  , syncFromDirectory  = error "move this out of this interface?"
  , syncToDirectory    = error "move this out of this interface?"
  , watches  =
      \k    -> fmap snd . filter (\(k',_r) -> k==k') . Map.keys <$> use watches'
  , getWatch =
      \k id -> Map.lookup (k, id) <$> use watches'
  , putWatch =
      \k id term -> watches' %= Map.insert (k, id) term
  , termsOfTypeImpl         = \r -> error "todo" r
  , termsMentioningTypeImpl = \r -> error "todo" r
  }

deleteComponent :: Reference.Id -> Set Reference -> Set Reference
deleteComponent r rs = Set.difference rs
  (Reference.members . Reference.componentFor . Reference.DerivedId $ r)
