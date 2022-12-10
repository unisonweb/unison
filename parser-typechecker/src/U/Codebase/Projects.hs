module U.Codebase.Projects where

import qualified Control.Lens.Cons as Cons
import Control.Monad.Reader
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import U.Codebase.Branch
import qualified U.Codebase.Branch.Type as Branch
import qualified U.Codebase.Causal as Causal
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite

libSegment :: Branch.NameSegment
libSegment = (Branch.NameSegment "lib")

-- | Infers the project root containing a given path.
-- Currently this means finding the closest parent with a "lib" child.
inferProjectRoot :: Path -> Branch Sqlite.Transaction -> Sqlite.Transaction (Maybe Path)
inferProjectRoot p _b | Just match <- specialCases p = pure $ Just match
  where
    specialCases :: Path -> Maybe Path
    specialCases ("public" Cons.:< "base" Cons.:< release Cons.:< _rest) = Just (Path.fromList ["public", "base", release])
    specialCases _ = Nothing
inferProjectRoot p b = getLast <$> execWriterT (runReaderT (go p b) Path.empty)
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
