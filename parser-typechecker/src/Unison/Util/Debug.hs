module Unison.Util.Debug where

import Data.Foldable (toList)
import qualified Data.Set as Set
import Debug.Trace

-- | e.g. traceIfShowMatch declIds ["#6qg3e"] ("when deserializing raw branch " ++ show h)
traceIfShowMatch :: (Applicative f, Foldable t, Ord a, Show a) => t a -> [String] -> String -> f ()
traceIfShowMatch as shows context =
  let filtered = Set.filter (\a -> show a `elem` shows) . Set.fromList $ toList as
  in if not (null filtered)
  then traceM $ "encountered " ++ show (toList filtered) ++ if not (null context) then " " ++ context else ""
  else pure ()
