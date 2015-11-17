{-# Language RecordWildCards #-}

module Unison.TermExplorer where

import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Reflex.Dom
import Unison.Metadata (Metadata)
import Unison.Node (Node,SearchResults,LocalInfo)
import Unison.Node (SearchResults)
import Unison.Node.MemNode (V)
import Unison.Paths (Target, Path)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as Map
import qualified Unison.Metadata as Metadata
import qualified Unison.Paths as Paths
import qualified Unison.Var as Var
import qualified Unison.Views as Views

data S =
  S { metadata :: Map Reference (Metadata V Reference)
    , lastResults :: Maybe (SearchResults V Reference (Term V))
    , id :: Int }

instance Monoid S where
  mempty = S Map.empty Nothing 0
  mappend (S md1 r1 id1) (S md2 r2 id2) = S (Map.unionWith const md2 md1) (if id2 > id1 then r2 else r1) (id1 `max` id2)

--explorer :: forall t m k s a z. (Reflex t, MonadWidget t m, Eq k, Semigroup s)
--         => Event t Int
--         -> (Dynamic t s -> Dynamic t (Maybe z) -> Dynamic t String -> Action m s k a)
--         -> Event t (m z) -- loaded asynchronously on open of explorer
--         -> Dynamic t s
--         -> m (Dynamic t s, Event t (Maybe a))
--data SearchResults v h t e =
--  SearchResults
--    { query :: Metadata.Query
--    , references :: [(h, Metadata v h)]
--    , matches :: ([e], Int)
--    , illTypedMatches :: ([e], Int)
--    , positionsExamined :: [Int] }

make :: (MonadWidget t m, Reflex t)
     => Event t Int
     -> Event t (LocalInfo (Term V) (Type V))
     -> Dynamic t S
     -> m (Dynamic t S, Event t (Maybe (Path -> Target V -> Target V)))
make keydown localInfo s = do
  localInfo <- holdDyn Nothing (Just <$> localInfo)
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol mds ref = maybe (Views.defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref mds)
  let lookupName mds ref = Var.name (lookupSymbol mds ref)
  let k localInfo (S {..}) = keyedResults (lookupName metadata) localInfo lastResults
  keyed <- combineDyn k localInfo s
  undefined

keyedResults :: (Reference -> Text)
             -> Maybe (LocalInfo (Term V) (Type V))
             -> Maybe (SearchResults V Reference (Term V))
             -> ([(String, m (Path -> Target V -> Target V))], [(String, m ())])
keyedResults name localInfo results =
  let
    replace e path ctx = Paths.modify (const (Paths.Term e)) path ctx
    go localInfo results = error "todo"
  in
    fromMaybe ([],[]) $ go <$> localInfo <*> results
