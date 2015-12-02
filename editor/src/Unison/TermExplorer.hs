{-# Language RecursiveDo #-}
{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module Unison.TermExplorer where

import Debug.Trace
import Data.Functor
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Semigroup
import Reflex.Dom
import Unison.Metadata (Metadata,Query(..))
import Unison.Node (Node,SearchResults,LocalInfo)
import Unison.Node.MemNode (V)
import Unison.Paths (Path)
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
import qualified Unison.Signals as Signals
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import qualified Unison.View as View
import qualified Unison.Views as Views

watch :: Show a => String -> a -> a
watch msg a = traceShow (msg,a) a

data S =
  S { metadata :: Map Reference (Metadata V Reference) }

instance Semigroup S where
  (S md1) <> (S md2) = S (Map.unionWith const md2 md1)

type Advance = Bool

data Action
  = Replace Path (Term V)
  | Step Path
  | Eval Path

make :: forall t m . (MonadWidget t m, Reflex t)
     => Node IO V Reference (Type V) (Term V)
     -> Event t Int
     -> Behavior t S
     -> Behavior t Path
     -> Behavior t (Term V)
     -> m (Event t S, Event t (Maybe (Action,Advance)))
make node keydown s paths terms =
  let
    formatLocalInfo Node.LocalInfo{..} = do
      name <- Views.lookupSymbol . metadata <$> sample s
      let width = Dimensions.Width 400
      elClass "div" "explorer-local-info" $ do
        id $
          if localAdmissibleType == Type.forall' ["a"] (Type.v' "a") then pure ()
          else void $ elClass "div" "localAdmissibleType" $ DocView.view width (Views.type' name localAdmissibleType)
        _ <- elClass "div" "localVariables" $
          traverse (elClass "div" "localVariable" . DocView.view width . Views.term name) localVariables
        pure ()
    parse _ _ Nothing _ = []
    parse lookup path (Just (Node.LocalInfo{..})) txt = case Parser.run LiteralParser.term txt of
      Parser.Succeed ts n | all (\c -> c == ' ' || c == ',') (drop n txt) ->
        ts >>= \tm ->
          if isRight (Typechecker.checkAdmissible' tm localAdmissibleType)
          then [formatResult lookup tm (Replace path tm, False) Right]
          else [formatResult lookup tm () Left]
      _ -> []
    processQuery localInfo s txt selection = do
      -- GHC type inference fail here
      localInfoB <- Signals.holdMaybe localInfo :: m (Behavior t (Maybe (LocalInfo (Term V) (Type V))))
      let lookupSymbols = Views.lookupSymbol . metadata <$> s
      let locals = formatLocals <$> lookupSymbols <*> paths <*> localInfoB
      literals <- hold [] $
        let go txt = parse <$> sample lookupSymbols <*> sample paths <*> sample localInfoB <*> pure txt
        in pushAlways go (updated txt)
      mdo
        searchResultE <- id $
          let
            f txt = do
              term <- sample terms; path <- sample paths; info <- sample localInfoB
              let g info = Node.search node term path 10 (Query (Text.pack txt)) (Just (Node.localAdmissibleType info))
              pure $ g <$> info
            searchEvents = push f triggeringTxt
          in Signals.evaluate Note.run searchEvents
        searchResultB <- Signals.holdMaybe searchResultE
        searches <- pure $ formatSearch <$> lookupSymbols <*> paths <*> searchResultB
        -- text which triggers a refinement to an existing search
        searchOutstanding <- toggle False (leftmost [void searchResultE])
        searchTrigger <- id $
          let
            ok txt' = do
              lastResults <- sample searchResultB
              complete <- fromMaybe False . fmap resultsComplete <$> pure lastResults
              old <- sample (current txt)
              alreadyRunning <- sample (current searchOutstanding)
              pure $ if {-alreadyRunning ||-} complete && isPrefixOf old txt' then Nothing
                     else (Just ())
          in do tick <- Signals.after localInfo; pure $ leftmost [tick, push ok (updated txt)]
        let triggeringTxt = tagDyn txt searchTrigger
        -- todo - other actions
        keyed <- pure $ (\a b c -> a ++ b ++ c) <$> locals <*> searches <*> literals
        let trimEnd = reverse . dropWhile (== ' ') . reverse
        let f possible txt = let txt' = trimEnd txt in filter (isPrefixOf txt' . fst) possible
        filtered <- pure $ f <$> keyed <*> current txt
        let outputS = S . Map.fromList . Node.references <$> searchResultE
        _ <- widgetHold (pure ()) (formatLocalInfo <$> localInfo)
        ticks <- Signals.guard $ leftmost [void localInfo, void $ updated txt, void searchResultE]
        pure $
          let
            advance (a, _) = (a, True)
            render _ txt _   | any (== ';') txt = Explorer.Cancel
            render _ txt sel | isSuffixOf "  " txt = maybe Explorer.Cancel (Explorer.Accept . advance) sel
            render rs _ _ = Explorer.Results rs 0 -- todo - indicate additional
            explorerEvents = pushAlways
              (\_ -> render <$> sample filtered <*> sample (current txt) <*> sample selection)
              ticks
          in (outputS, explorerEvents)
  in
  do
    localInfo <- do
      p <- sample paths; t <- sample terms
      Signals.later (Note.run (Node.localInfo node t p))
    Explorer.explorer keydown (processQuery localInfo) s

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
  in (txt, w (as <$ DocView.view (Dimensions.Width 300) doc))

formatLocals :: MonadWidget t m
             => (Reference -> Symbol View.DFO)
             -> Path
             -> Maybe (LocalInfo (Term V) (Type V))
             -> [(String, Either (m ()) (m (Action,Advance)))]
formatLocals name path results = fromMaybe [] $ go <$> results
  where
  view localType 0 = Term.var' "□" `Term.ann` localType
  view _ n = Term.var' "□" `Term.apps` replicate n Term.blank
  replace localTerm n = localTerm `Term.apps` replicate n Term.blank
  go (Node.LocalInfo {..}) =
    [ formatResult name e ((Replace path e),False) Right | e <- localVariableApplications ] ++
    [ formatResult name (view localType n) (Replace path (replace localTerm n),False) Right | n <- localOverapplications ]

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
