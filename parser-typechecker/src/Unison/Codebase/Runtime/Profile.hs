{-# LANGUAGE ExistentialQuantification #-}

module Unison.Codebase.Runtime.Profile
  ( ProfTrie (..),
    Profile (..),
    SomeProfile (..),
    ProfileSpec (..),
    emptyProfile,
    singlePath,
    addPath,
    aggregatePruned,
    fullProfile,
    miniProfile,
    foldedProfile,
  )
where

import Data.Bifunctor (second)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.String
import Numeric
import Unison.PrettyPrintEnv
import Unison.Reference
import Unison.Referent
import Unison.Syntax.NamePrinter
import Unison.Util.Pretty as P

-- A `ProfTrie` maps non-empty paths to counts of occurrences of that
-- path. This allows tracking relative frequency of call stacks. The
-- path elements are parameterized to allow for more efficient `Map` keys
-- to be used during construction, and can be paired with a decoding to
-- `Reference` as below.
newtype ProfTrie k a = ProfT (Map k (a, ProfTrie k a))
  deriving (Functor)

trimEmpty :: (Ord k, Eq a, Num a) => ProfTrie k a -> ProfTrie k a
trimEmpty (ProfT m) = case M.traverseMaybeWithKey f m of
  Identity m -> ProfT m
  where
    f _ (a, trimEmpty -> sub@(ProfT m))
      | a == 0, null m = pure Nothing
      | otherwise = pure $ Just (a, sub)

demux ::
  (Ord k, Eq a, Eq b, Num a, Num b) =>
  ProfTrie k (a, b) ->
  (ProfTrie k a, ProfTrie k b)
demux tr = (trimEmpty $ fst <$> tr, trimEmpty $ snd <$> tr)

-- A profile pairs the above arbitrary key based profile trie with a
-- decoding of the integers to references and a total sample count.
data Profile k =
  Prof !(Int, Int) !(ProfTrie k (Int, Int)) !(Map k Reference)

-- Abstracts over the exact key type used in a profile.
data SomeProfile = forall k. (Ord k) => SomeProf (Profile k)

data ProfileSpec = NoProf | MiniProf | FullProf String
  deriving (Eq, Ord, Show)

emptyProfile :: Profile k
emptyProfile = Prof zero (ProfT M.empty) M.empty

zero :: (Int, Int)
zero = (0, 0)

inc :: Bool -> (Int, Int) -> (Int, Int)
inc b (m, n) = pair (m+1) (if b then n+1 else n)
  where
    pair !x !y = (x, y)

-- Creates a singleton profile trie from a path.
singlePath ::
  (Ord k) =>
  Bool ->
  [k] ->
  ((Int, Int), ProfTrie k (Int, Int))
singlePath b [] = (inc b zero, ProfT M.empty)
singlePath b (i : is) =
  (zero,) . ProfT $! M.singleton i (singlePath b is)

addPath0 ::
  (Ord k) =>
  Bool ->
  [k] ->
  ((Int, Int), ProfTrie k (Int, Int)) ->
  ((Int, Int), ProfTrie k (Int, Int))
addPath0 b [] (t, p) = (,p) $! inc b t
addPath0 b (i : is) (m, ProfT p) = (m,) . ProfT $! M.alter f i p
  where
    f Nothing = Just $ singlePath b is
    f (Just q) = Just $ addPath0 b is q

-- Adds a path to a profile trie, incrementing the count for the given
-- path.
addPath ::
  (Ord k) =>
  Bool ->
  [k] ->
  ProfTrie k (Int, Int) ->
  ProfTrie k (Int, Int)
addPath _ [] p = p
addPath b (i : is) (ProfT m) = ProfT $ M.alter f i m
  where
    f Nothing = Just $ singlePath b is
    f (Just q) = Just $ addPath0 b is q

data AggInfo k = Ag
  { -- inherited sample count
    inherited :: Int,
    -- total sample for all occurrences of a key
    allOccs :: Map k Int
  }

instance (Ord k) => Semigroup (AggInfo k) where
  Ag il al <> Ag ir ar =
    Ag (il + ir) (M.unionWith (+) al ar)

instance (Ord k) => Monoid (AggInfo k) where
  mempty = Ag 0 M.empty

aggregateWith ::
  (Ord k) =>
  (AggInfo k -> Int -> r) ->
  k ->
  (Int, ProfTrie k Int) ->
  (AggInfo k, (r, ProfTrie k r))
aggregateWith f k (m, ProfT t) =
  case M.traverseWithKey (aggregateWith f) t of
    (ag, t) -> (ag', (f ag' m, ProfT t))
      where
        ag' = ag <> Ag m (M.singleton k m)

prune0 ::
  (Ord k) =>
  Set k ->
  k ->
  (a, ProfTrie k a) ->
  Identity (Maybe (a, ProfTrie k a))
prune0 keep k (a, ProfT sub) =
  case M.traverseMaybeWithKey (prune0 keep) sub of
    Identity sub
      | null sub, k `S.notMember` keep -> pure Nothing
      | otherwise -> pure $ Just (a, ProfT sub)

prune :: (Ord k) => Set k -> ProfTrie k a -> ProfTrie k a
prune keep (ProfT m) = case M.traverseMaybeWithKey (prune0 keep) m of
  Identity sub -> ProfT sub

topN :: (Ord k) => Int -> Map k Int -> [(k, Int)]
topN n0 = M.foldlWithKey (ins n0) []
  where
    ins _ pss _ 0 = pss
    ins 0 _ _ _ = []
    ins _ [] k i = [(k, i)]
    ins n pss@((k1, j) : ps) k0 i
      | i > j = (k0, i) : pop (n - 1) pss
      | otherwise = (k1, j) : ins (n - 1) ps k0 i

    pop 0 _ = []
    pop _ [] = []
    pop n (p : ps) = p : pop (n - 1) ps

fraction :: Int -> Int -> Double
fraction n d = fromIntegral n / fromIntegral d

fractions :: Int -> AggInfo k -> Int -> (Double, Double)
fractions total ag lo =
  (fraction (inherited ag) total, fraction lo total)

topNum :: Int
topNum = 25

-- Given a total count and a profile trie, calculates local and inherited
-- cost fractions of the positions in the trie, and prunes it to the
-- hottest spots.
aggregatePruned ::
  (Ord k) => Int -> ProfTrie k Int -> ProfTrie k (Double, Double)
aggregatePruned total (ProfT t) =
  case M.traverseWithKey (aggregateWith (fractions total)) t of
    (ag, t)
      | top <- topN topNum (allOccs ag) ->
          prune (S.fromList $ fst <$> top) $ ProfT t

aggregate ::
  (Ord k) =>
  Int ->
  ProfTrie k Int ->
  ([(k, Double)], ProfTrie k (Double, Double))
aggregate total (ProfT t) =
  case M.traverseWithKey (aggregateWith (fractions total)) t of
    (ag, t) ->
      ( second (flip fraction total) <$> topN topNum (allOccs ag),
        ProfT t
      )

-- Folds over a profile trie. The mapping function receives a reversed
-- path to the node, which can be used e.g. to see the node's key and to
-- calculate the depth in the trie.
foldMapTrie :: (Monoid m) => ((k, [k]) -> v -> m) -> ProfTrie k v -> m
foldMapTrie f = descend []
  where
    descend ks (ProfT m) =
      M.foldMapWithKey
        (\k (v, sub) -> f (k, ks) v <> descend (k : ks) sub)
        m

showPercent :: Double -> String
showPercent d = pad $ showFFloat (Just 2) (100 * d) "%"
  where
    pad s = replicate (7 - length s) ' ' <> s

dispProfEntry ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Map k Reference ->
  (k, [k]) ->
  (Double, Double) ->
  Pretty ColorText
dispProfEntry ppe misc refs (k, ks) (inh, self) =
  mconcat
    [ P.indentN 2 . fromString $ showPercent inh,
      P.indentN 4 . fromString $ showPercent self,
      P.indentN (2 * ind + 4) $ dispKey ppe misc refs k,
      "\n"
    ]
  where
    ind = fromIntegral $ length ks

dispFunc ::
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Reference ->
  Pretty ColorText
dispFunc ppe misc r
  | Just pr <- M.lookup r misc = pr
  | otherwise =
      syntaxToColor . prettyHashQualified . termName ppe $ Ref r

dispKey ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Map k Reference ->
  k ->
  Pretty ColorText
dispKey ppe misc refs k = case M.lookup k refs of
  Just r -> dispFunc ppe misc r
  Nothing -> "<unknown>"

dispProfTrie ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Map k Reference ->
  ProfTrie k (Double, Double) ->
  Pretty ColorText
dispProfTrie ppe misc refs ag =
  foldMapTrie (dispProfEntry ppe misc refs) ag

dispTopEntry ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Map k Reference ->
  (k, Double) ->
  Pretty ColorText
dispTopEntry ppe misc refs (k, frac) =
  mconcat
    [ P.indentN 3 . fromString $ showPercent frac,
      P.indentN 4 dr,
      "\n"
    ]
  where
    dr :: Pretty ColorText
    dr
      | Just r <- M.lookup k refs = dispFunc ppe misc r
      | otherwise = "<unknown>"

dispTop ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Map k Reference ->
  [(k, Double)] ->
  Pretty ColorText
dispTop ppe misc refs = foldMap (dispTopEntry ppe misc refs)

overallHeader :: Pretty ColorText -> Int -> Pretty ColorText
overallHeader label samps = label <> ": " <> dsamps <> newline
  where
    dsamps
      | samps == 1 = "1 sample"
      | otherwise = fromString (show samps) <> " samples"

profileTopHeader :: Pretty ColorText
profileTopHeader =
  "Hot Spots\n" <> " Total Cost    Function\n"

profileTreeHeader :: Pretty ColorText
profileTreeHeader =
  P.lines
    [ P.indentN 9 "Costs",
      "Inherited      Local    Function Call Tree",
      ""
    ]

miniProfile ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Profile k ->
  Pretty ColorText
miniProfile ppe misc (Prof (total, wtotal) tr refs) =
  P.lines
    [ overallHeader "Complete Profile" total,
      profileTreeHeader <> dispProfTrie ppe misc refs ag,
      "",
      if wtotal > 0
      then
        overallHeader "Post-wakeup Profile" wtotal <> newline
          <> profileTreeHeader
          <> dispProfTrie ppe misc refs agw
      else "Threads never missed ticks"
    ]
  where
    (full, wait) = demux tr
    ag = aggregatePruned total full
    agw = aggregatePruned wtotal wait

fullProfile ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Profile k ->
  (Pretty ColorText, Pretty ColorText)
fullProfile ppe misc (Prof (total, wtotal) tr0 refs) =
  ( make "Complete Profile" total comp,
    make "Post-wakeup Profile" wtotal wait
  )
  where
    (comp, wait) = demux tr0

    make label tot tr =
      overallHeader label tot <> newline
        <> profileTopHeader
        <> dispTop ppe misc refs top
        <> newline <> newline
        <> profileTreeHeader
        <> dispProfTrie ppe misc refs ag
      where
        (top, ag) = aggregate tot tr

foldedProfile ::
  (Ord k) =>
  PrettyPrintEnv ->
  Map Reference (Pretty ColorText) ->
  Profile k ->
  (String, String)
foldedProfile ppe misc (Prof _ tr refs) =
  ( toPlain 0 $ foldMapTrie f comp,
    toPlain 0 $ foldMapTrie f wake
  )
  where
    dk = dispKey ppe misc refs

    (comp, wake) = demux tr

    f (k, ks) n =
      mconcat
        [ foldl (\tx k -> dk k <> ";" <> tx) (dk k) ks,
          " ",
          fromString $ show n,
          "\n"
        ]
