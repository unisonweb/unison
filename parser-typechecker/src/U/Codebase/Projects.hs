module U.Codebase.Projects where

import Control.Lens.Cons qualified as Cons
import Control.Monad.Reader
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import U.Codebase.Branch
import U.Codebase.Causal qualified as Causal
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Name (libSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

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
