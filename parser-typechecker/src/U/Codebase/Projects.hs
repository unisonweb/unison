module U.Codebase.Projects
  ( inferNamesRoot,
    inferDependencyMounts,
    libSegment,
  )
where

import Control.Lens (ifoldMap)
import qualified Control.Lens.Cons as Cons
import Control.Monad.Reader
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import U.Codebase.Branch
import qualified U.Codebase.Causal as Causal
import U.Codebase.HashTags (BranchHash (..))
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Unison.Util.Monoid (ifoldMapM)

libSegment :: NameSegment
libSegment = NameSegment "lib"

-- | Infers path to use for loading names.
-- Currently this means finding the closest parent with a "lib" child.
inferNamesRoot :: Path -> Branch Sqlite.Transaction -> Sqlite.Transaction (Maybe Path)
inferNamesRoot p _b | Just match <- specialCases p = pure $ Just match
  where
    specialCases :: Path -> Maybe Path
    specialCases ("public" Cons.:< "base" Cons.:< release Cons.:< _rest) = Just (Path.fromList ["public", "base", release])
    specialCases _ = Nothing
inferNamesRoot p b = getLast <$> execWriterT (runReaderT (go p b) Path.empty)
  where
    go :: Path -> Branch Sqlite.Transaction -> ReaderT Path (WriterT (Last Path) Sqlite.Transaction) ()
    go p b = do
      childMap <- lift . lift $ nonEmptyChildren b
      when (isJust $ Map.lookup libSegment childMap) $ ask >>= tell . Last . Just
      case p of
        Empty -> pure ()
        (nextChild Cons.:< pathRemainder) ->
          case Map.lookup (coerce nextChild) childMap of
            Nothing -> pure ()
            Just childCausal -> do
              childBranch <- lift . lift $ Causal.value childCausal
              local (Cons.|> nextChild) (go pathRemainder childBranch)

-- | Find all dependency mounts within a branch and the path to those mounts.
-- For a typical project this will return something like:
-- @[(lib.base, #abc), (lib.distributed, #def)]@
--
-- For a user codebase it will return something like:
-- @[(public.nested.namespace.lib.base, #abc), (public.other.namespace.lib.distributed, #def)]@
inferDependencyMounts :: Branch Sqlite.Transaction -> Sqlite.Transaction [(Path, BranchHash)]
inferDependencyMounts Branch {children} =
  do
    children
    & ifoldMapM \segment child -> do
      case segment of
        seg
          | seg == libSegment -> do
              Branch {children = deps} <- Causal.value child
              deps
                & ( ifoldMap \depName depBranch ->
                      [(Path.fromList [seg, depName], Causal.valueHash depBranch)]
                  )
                & pure
          | otherwise -> do
              childBranch <- Causal.value child
              inferDependencyMounts childBranch
                <&> map (first (Path.cons seg))
