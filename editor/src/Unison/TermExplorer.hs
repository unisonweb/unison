{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
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
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Unison.Dimensions as Dimensions
import qualified Unison.Doc as Doc
import qualified Unison.DocView as DocView
import qualified Unison.Explorer as Explorer
import qualified Unison.Metadata as Metadata
import qualified Unison.Paths as Paths
import qualified Unison.Var as Var
import qualified Unison.Node as Node
import qualified Unison.View as View
import qualified Unison.Views as Views
import qualified Unison.Term as Term

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
      let k (S {..}) = formatSearch (lookupSymbol metadata) path lastResults
      searches <- mapDyn k s
      locals <- combineDyn (\S{..} info -> formatLocals (lookupSymbol metadata) path info) s localInfo
      -- todo - literals and other actions
      -- todo - figuring when need to make remote requests
      keyed <- combineDyn (++) locals searches
      let trimEnd = reverse . dropWhile (== ' ') . reverse
      let f possible txt = let txt' = trimEnd txt in filter (isPrefixOf txt' . fst) possible
      filtered <- combineDyn f keyed txt
      -- todo parse txt and emit accept / cancel / accept-and-advance
      pure $ (\r -> Explorer.Results r 0) <$> updated filtered
  in
    Explorer.explorer keydown processQuery localInfo s

formatResult :: MonadWidget t m
             => (Reference -> Symbol View.DFO) -> Path -> Term V -> a -> (m a -> b) -> (String, b)
formatResult name path e as w =
  let doc = Views.term name e
      txt = Text.unpack . Text.concat $ Doc.tokens "\n" (Doc.flow doc)
  in (txt, w (as <$ DocView.widget never (Dimensions.Width 300) doc))

formatLocals :: MonadWidget t m
             => (Reference -> Symbol View.DFO)
             -> Path
             -> Maybe (LocalInfo (Term V) (Type V))
             -> [(String, Either (m ()) (m Action))]
formatLocals name path results = fromMaybe [] $ go <$> results
  where
  view n = Term.var' "□" `Term.apps` replicate n Term.blank
  replace localTerm n = localTerm `Term.apps` replicate n Term.blank
  go (Node.LocalInfo {..}) =
    [ formatResult name path e (Replace path e) Right | e <- localVariableApplications ] ++
    [ formatResult name path (view n) (Replace path (replace localTerm n)) Right | n <- localOverapplications ]

formatSearch :: MonadWidget t m
             => (Reference -> Symbol View.DFO)
             -> Path
             -> Maybe (SearchResults V Reference (Term V))
             -> [(String, Either (m ()) (m Action))]
formatSearch name path results = fromMaybe [] $ go <$> results
  where
  go (Node.SearchResults {..}) =
    [ formatResult name path e () Left | e <- fst illTypedMatches ] ++
    [ formatResult name path e (Replace path e) Right | e <- fst matches ]

