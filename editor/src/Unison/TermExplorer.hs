{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module Unison.TermExplorer where

import Control.Monad.IO.Class
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Semigroup
import Reflex.Dom
import Unison.Metadata (Metadata,Query(..))
import Unison.Node (Node,SearchResults,LocalInfo)
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
import qualified Unison.LiteralParser as LiteralParser
import qualified Unison.Node as Node
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Paths as Paths
import qualified Unison.Signals as Signals
import qualified Unison.Term as Term
import qualified Unison.Typechecker as Typechecker
import qualified Unison.View as View
import qualified Unison.Views as Views

data S =
  S { metadata :: Map Reference (Metadata V Reference)
    , lastResults :: Maybe (SearchResults V Reference (Term V))
    , overallTerm :: Target V
    , nonce :: Int }

instance Semigroup S where
  (S md1 r1 t1 id1) <> (S md2 r2 t2 id2) =
    S (Map.unionWith const md2 md1)
      (if id2 > id1 then r2 else r1)
      (if id2 > id1 then t2 else t1)
      (id1 `max` id2)

type Advance = Bool

data Action
  = Replace Path (Term V)
  | Step Path
  | Eval Path

make :: forall t m . (MonadWidget t m, Reflex t)
     => Node IO V Reference (Type V) (Term V)
     -> Event t Int
     -> Event t (LocalInfo (Term V) (Type V))
     -> Dynamic t S
     -> Dynamic t Path
     -> m (Dynamic t S, Event t (Maybe (Action,Advance)))
make node keydown localInfo s path =
  let
    parse _ _ Nothing _ = []
    parse lookup path (Just (Node.LocalInfo{..})) txt = case Parser.run LiteralParser.term txt of
      Parser.Succeed tm n | all (== ' ') (drop n txt) -> do
        if isRight (Typechecker.check' tm localAdmissibleType)
          then [formatResult lookup tm (Replace path tm, False) Right]
          else [formatResult lookup tm () Left]
      _ -> []
    processQuery s localInfo txt selection = do
      let k path (S {..}) = formatSearch (Views.lookupSymbol metadata) path lastResults
      searches <- combineDyn k path s
      metadatas <- mapDyn metadata s
      lookupSymbols <- mapDyn Views.lookupSymbol metadatas
      locals <- Signals.combineDyn3 formatLocals lookupSymbols path localInfo
      literals <- Signals.combineDyn4 parse lookupSymbols path localInfo txt
      -- todo - other actions
      keyed <- mconcatDyn [locals, searches, literals]
      let trimEnd = reverse . dropWhile (== ' ') . reverse
      let f possible txt = let txt' = trimEnd txt in filter (isPrefixOf txt' . fst) possible
      filtered <- combineDyn f keyed txt
      pure $
        let
          p (txt, (_,_)) | any (== ';') txt = pure (Just Explorer.Cancel)
          p (txt, (_,_)) | isSuffixOf "  " txt = fmap k <$> sample selection
            where k (a,_) = Explorer.Accept (a,True) -- ending with two spaces is an accept+advance
          p (txt, (rs,textUpdate)) = do
            s <- sample (current s)
            currentPath <- sample (current path)
            req <- pure $ do
              info <- sample (current localInfo)
              case Paths.asTerm (overallTerm s) of
                Nothing -> pure s
                Just overallTerm -> do
                  lastResults@Node.SearchResults{..} <- liftIO . Note.run $
                    Node.search node
                      overallTerm
                      currentPath
                      10
                      (Query (Text.pack txt))
                      (Node.localAdmissibleType <$> info)
                  pure $ S (Map.fromList references) (Just lastResults) (Paths.Term overallTerm) (nonce s + 1)
            let finish rs n = if textUpdate then Just (Explorer.Request req rs) else Just (Explorer.Results rs n)
            pure $ case lastResults s of
              Nothing -> finish rs 0
              Just results ->
                if resultsComplete results && isPrefixOf (queryString $ Node.query results) txt
                then finish rs (additionalResults results)
                else Just (Explorer.Results rs (additionalResults results))
        in
        push p $ attachDyn txt (updated filtered `Signals.coincides` updated txt)
    formatLocalInfo (i@Node.LocalInfo{..}) = i <$ do
      name <- Views.lookupSymbol . metadata <$> sample (current s)
      let txt doc = text . Text.unpack . Text.concat . Doc.tokens "\n" . Doc.flow $ doc
      elClass "div" "explorer-local-info" $ do
        elClass "div" "localType" $ txt (Views.type' name localType)
        elClass "div" "localAdmissibleType" $ txt (Views.type' name localAdmissibleType)
        _ <- elClass "div" "localVariables" $
          traverse (elClass "div" "localVariable" . txt . Views.term name) localVariables
        pure ()
  in
    Explorer.explorer keydown processQuery (fmap formatLocalInfo localInfo) s

queryString :: Query -> String
queryString (Query s) = Text.unpack s

additionalResults :: Node.SearchResults v h e -> Int
additionalResults = snd . Node.matches

resultsComplete :: Node.SearchResults v h e -> Bool
resultsComplete = (==0) . additionalResults

formatResult :: MonadWidget t m
             => (Reference -> Symbol View.DFO) -> Term V -> a -> (m a -> b) -> (String, b)
formatResult name e as w =
  let doc = Views.term name e
      txt = Text.unpack . Text.concat $ Doc.tokens "\n" (Doc.flow doc)
  in (txt, w (as <$ DocView.widget never (const never) (Dimensions.Width 300) doc))

formatLocals :: MonadWidget t m
             => (Reference -> Symbol View.DFO)
             -> Path
             -> Maybe (LocalInfo (Term V) (Type V))
             -> [(String, Either (m ()) (m (Action,Advance)))]
formatLocals name path results = fromMaybe [] $ go <$> results
  where
  view n = Term.var' "□" `Term.apps` replicate n Term.blank
  replace localTerm n = localTerm `Term.apps` replicate n Term.blank
  go (Node.LocalInfo {..}) =
    [ formatResult name e ((Replace path e),False) Right | e <- localVariableApplications ] ++
    [ formatResult name (view n) (Replace path (replace localTerm n),False) Right | n <- localOverapplications ]

formatSearch :: MonadWidget t m
             => (Reference -> Symbol View.DFO)
             -> Path
             -> Maybe (SearchResults V Reference (Term V))
             -> [(String, Either (m ()) (m (Action,Advance)))]
formatSearch name path results = fromMaybe [] $ go <$> results
  where
  go (Node.SearchResults {..}) =
    [ formatResult name e () Left | e <- fst illTypedMatches ] ++
    [ formatResult name e (Replace path e,False) Right | e <- fst matches ]
