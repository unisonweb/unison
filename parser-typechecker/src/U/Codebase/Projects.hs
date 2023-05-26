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
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import U.Codebase.Branch
import qualified U.Codebase.Causal as Causal
import U.Codebase.HashTags (BranchHash (..))
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Name (libSegment)
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import Unison.Util.Monoid (ifoldMapM)

-- | Infers path to use for loading names.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ (where .myproject.lib exists) or @.myproject.lib.base.List -> .myproject.lib.base@
inferNamesRoot :: Path -> Branch Sqlite.Transaction -> Sqlite.Transaction (Maybe Path)
inferNamesRoot p b
  | Just match <- findBaseProject p = pure $ Just match
  | Just depRoot <- findDepRoot p = pure $ Just depRoot
  | otherwise = getLast <$> execWriterT (runReaderT (go p b) Path.empty)
  where
    findBaseProject :: Path -> Maybe Path
    findBaseProject ("public" Cons.:< "base" Cons.:< release Cons.:< _rest) = Just (Path.fromList ["public", "base", release])
    findBaseProject _ = Nothing
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

-- | If the provided path is within a lib dir (or a transitive lib) find the dependency
-- we're in.
--
-- E.g. @.myproject.lib.base.List -> .myproject.lib.base@
-- E.g. @.myproject.lib.distributed.lib.base.List -> .myproject.lib.distributed.lib.base@
--
-- >>> findDepRoot (Path.fromList ["myproject", "lib", "base", "List"])
-- Just myproject.lib.base
--
-- >>> findDepRoot (Path.fromList ["myproject", "lib", "distributed", "lib", "base", "List"])
-- Just myproject.lib.distributed.lib.base
--
-- Just lib isn't inside a dependency.
-- >>> findDepRoot (Path.fromList ["myproject", "lib"])
-- Nothing
findDepRoot :: Path -> Maybe Path
findDepRoot (lib Cons.:< depRoot Cons.:< rest)
  | lib == libSegment =
      -- Keep looking to see if the full path is actually in a transitive dependency, otherwise
      -- fallback to this spot
      ((Path.fromList [lib, depRoot] <>) <$> findDepRoot rest)
        <|> Just (Path.fromList [lib, depRoot])
findDepRoot (other Cons.:< rest) = (other Cons.:<) <$> findDepRoot rest
findDepRoot _ = Nothing

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
