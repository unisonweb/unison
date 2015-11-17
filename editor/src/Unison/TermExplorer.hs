{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}

module Unison.TermExplorer where

import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Semigroup
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
import qualified Unison.Explorer as Explorer
import qualified Unison.Metadata as Metadata
import qualified Unison.Paths as Paths
import qualified Unison.Var as Var
import qualified Unison.Views as Views

data S =
  S { metadata :: Map Reference (Metadata V Reference)
    , lastResults :: Maybe (SearchResults V Reference (Term V))
    , overallTerm :: Target V
    , path :: Path -- path into `overallTerm`
    , id :: Int }

instance Semigroup S where
  (S md1 r1 t1 p1 id1) <> (S md2 r2 t2 p2 id2) =
    S (Map.unionWith const md2 md1)
      (if id2 > id1 then r2 else r1)
      (if id2 > id1 then t2 else t1)
      (if id2 > id1 then p2 else p1)
      (id1 `max` id2)

data Action
  = Replace Path (Term V)
  | Step Path
  | Eval Path

make :: (MonadWidget t m, Reflex t)
     => Event t Int
     -> Event t (m (LocalInfo (Term V) (Type V)))
     -> Dynamic t S
     -> m (Dynamic t S, Event t (Maybe Action))
make keydown localInfo s =
  let
    firstName (Metadata.Names (n:_)) = n
    lookupSymbol mds ref = maybe (Views.defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref mds)
    lookupName mds ref = Var.name (lookupSymbol mds ref)
    processQuery s localInfo txt = do
      let k localInfo (S {..}) = keyedResults (lookupName metadata) localInfo lastResults
      keyed <- combineDyn k localInfo s
      let trimEnd = reverse . dropWhile (== ' ') . reverse
      let f possible txt = let txt' = trimEnd txt in filter (isPrefixOf txt' . fst) possible
      filtered <- combineDyn f keyed txt
      -- todo parse txt and emit accept / cancel / accept-and-advance
      pure $ (\r -> Explorer.Results r 0) <$> updated filtered
  in
    Explorer.explorer keydown processQuery localInfo s

keyedResults :: (Reference -> Text)
             -> Maybe (LocalInfo (Term V) (Type V))
             -> Maybe (SearchResults V Reference (Term V))
             -> [(String, Either (m ()) (m Action))]
keyedResults name localInfo results =
  let
    replace e path ctx = Paths.modify (const (Paths.Term e)) path ctx
    go localInfo results = error "todo"
  in
    fromMaybe [] $ go <$> localInfo <*> results
