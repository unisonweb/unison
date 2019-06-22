-- {-# OPTIONS_GHC -Wwarn #-} -- todo: remove me later
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Branch2 where

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
import qualified Control.Monad                 as Monad
--import           Control.Monad.Extra            ( whenM )
import           Data.Bifunctor                 ( second )
-- import           Data.GUID                (genText)
import qualified Data.Foldable as Foldable
import           Data.List                      ( foldl'
    -- , intercalate
    )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           Data.Tuple                     (swap)
--import qualified Data.Text                     as Text
import           Data.Foldable                  ( for_, toList )
import           Data.Traversable               ( for )
import qualified Unison.Codebase.Patch         as Patch
import           Unison.Codebase.Patch          ( Patch )
import qualified Unison.Codebase.Causal2       as Causal
import           Unison.Codebase.Causal2        ( Causal
                                                , pattern RawOne
                                                , pattern RawCons
                                                , pattern RawMerge
                                                )
import           Unison.Codebase.SearchResult (SearchResult)
import qualified Unison.Codebase.SearchResult as SR
import           Unison.Codebase.TermEdit       ( TermEdit )
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import           Unison.Codebase.Path           ( Path(..) )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.NameSegment    ( NameSegment )
import qualified Unison.Codebase.NameSegment   as NameSegment
import qualified Unison.Codebase.Metadata      as Metadata
import qualified Unison.Hash                   as Hash
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified          as HQ
import qualified Unison.ShortHash as SH


import           Unison.Name                    ( Name(..) )
import qualified Unison.Name                   as Name
import           Unison.Names2                  ( Names'(Names), Names, Names0 )
import qualified Unison.Names2                 as Names
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent(Con,Ref) )
import qualified Unison.Referent              as Referent
import qualified Unison.Reference             as Reference

import qualified Unison.Util.Relation         as R
import           Unison.Util.Relation           ( Relation )
import qualified Unison.Util.Star3             as Star3
import           Unison.Util.Star3              ( Star3 )
import qualified Unison.Util.List              as List


-- type EditGuid = Text

newtype Branch m = Branch { _history :: Causal m Raw (Branch0 m) }
  deriving (Eq, Ord)

type Hash = Causal.RawHash Raw
type EditHash = Hash.Hash

-- Star3 r n Metadata.Type (Metadata.Type, Metadata.Value)
type Star r n = Metadata.Star r n

data Branch0 m = Branch0
  { _terms :: Star Referent NameSegment
  , _types :: Star Reference NameSegment
  , _children:: Map NameSegment (Hash, Branch m) --todo: can we get rid of this hash
  , _edits :: Map NameSegment (EditHash, m Patch)
  , toNamesSeg :: Names.NamesSeg
  , toNames0 :: Names.Names0
  , deepReferents :: Set Referent
  , deepTypeReferences :: Set Reference
  , deepTerms :: Star Referent Name
  , deepTypes :: Star Reference Name
  }

-- The raw Branch
data Raw = Raw
  { _termsR :: Star Referent NameSegment
  , _typesR :: Star Reference NameSegment
  , _childrenR :: Map NameSegment Hash
  , _editsR :: Map NameSegment EditHash
  }

makeLenses ''Branch
makeLensesFor [("_edits", "edits")] ''Branch0
makeLenses ''Raw

terms :: Lens' (Branch0 m) (Star Referent NameSegment)
terms = lens _terms (\Branch0{..} x -> branch0 x _types _children _edits)
types :: Lens' (Branch0 m) (Star Reference NameSegment)
types = lens _types (\Branch0{..} x -> branch0 _terms x _children _edits)
children :: Lens' (Branch0 m) (Map NameSegment (Hash, Branch m))
children = lens _children (\Branch0{..} x -> branch0 _terms _types x _edits)

-- creates a Branch0 from the primary fields and derives the others.
branch0 :: Metadata.Star Referent NameSegment
        -> Metadata.Star Reference NameSegment
        -> Map NameSegment (Hash, Branch m)
        -> Map NameSegment (EditHash, m Patch)
        -> Branch0 m
branch0 terms types children edits =
  Branch0 terms types children edits namesSeg names0 deepRefts deepTypeRefs deepTerms' deepTypes'
  where
  deepTerms' =
    Star3.mapD1 nameSegToName terms <> foldMap go (Map.toList (snd <$> children))
    where
    go (nameSegToName -> n, b) = Star3.mapD1 (Name.joinDot n) (deepTerms $ head b)
  deepTypes' =
    Star3.mapD1 nameSegToName types <> foldMap go (Map.toList (snd <$> children))
    where
    go (nameSegToName -> n, b) = Star3.mapD1 (Name.joinDot n) (deepTypes $ head b)
  termsr = R.swap $ Star3.d1 terms
  typesr = R.swap $ Star3.d1 types
  namesSeg = toNamesSegImpl termsr typesr
  names0 = foldMap toNames0Impl (Map.toList (fmap snd children))
            <> Names (R.mapDom nameSegToName termsr)
                     (R.mapDom nameSegToName typesr)
  deepRefts = foldMap deepReferents childrenBranch0
  deepTypeRefs = foldMap deepTypeReferences childrenBranch0
  childrenBranch0 = fmap (head . snd) . Foldable.toList $ children

  toNames0Impl :: (NameSegment, Branch m) -> Names0
  toNames0Impl (nameSegToName -> n, head -> b0) = Names.prefix0 n (toNames0 b0)
  toNamesSegImpl :: Relation NameSegment Referent
                 -> Relation NameSegment Reference
                 -> Names' (HQ.HashQualified' NameSegment)
  toNamesSegImpl terms types = Names terms' types' where
    terms' = R.map (\(n, r) -> (Names.hqTermName names n r, r)) terms
    types' = R.map (\(n, r) -> (Names.hqTypeName names n r, r)) types
    names :: Names' NameSegment
    names = Names terms types
  nameSegToName = Name . NameSegment.toText

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> Hash
headHash (Branch c) = Causal.currentHash c

merge :: Monad m => Branch m -> Branch m -> m (Branch m)
merge (Branch x) (Branch y) = Branch <$> Causal.mergeWithM merge0 x y

-- todo: use 3-way merge for terms, types, and edits
merge0 :: forall m. Monad m => Branch0 m -> Branch0 m -> m (Branch0 m)
merge0 b1 b2 = do
  c3 <- unionWithM f (_children b1) (_children b2)
  e3 <- unionWithM g (_edits b1) (_edits b2)
  pure $
    Branch0 (_terms b1 <> _terms b2)
            (_types b1 <> _types b2)
            c3
            e3
            (toNamesSeg b1 <> toNamesSeg b2)
            (toNames0 b1 <> toNames0 b2)
            (deepReferents b1 <> deepReferents b2)
            (deepTypeReferences b1 <> deepTypeReferences b2)
            (deepTerms b1 <> deepTerms b2)
            (deepTypes b1 <> deepTypes b2)
  where
  f :: (h1, Branch m) -> (h2, Branch m) -> m (Hash, Branch m)
  f (_h1, b1) (_h2, b2) = do b <- merge b1 b2; pure (headHash b, b)

  g :: (EditHash, m Patch) -> (EditHash, m Patch) -> m (EditHash, m Patch)
  g (h1, m1) (h2, _) | h1 == h2 = pure (h1, m1)
  g (_, m1) (_, m2) = do
    e1 <- m1
    e2 <- m2
    let e3 = e1 <> e2
    pure (H.accumulate' e3, pure e3)


unionWithM :: forall m k a.
  (Monad m, Ord k) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f m1 m2 = Monad.foldM go m1 $ Map.toList m2 where
  go :: Map k a -> (k, a) -> m (Map k a)
  go m1 (k, a2) = case Map.lookup k m1 of
    Just a1 -> do a <- f a1 a2; pure $ Map.insert k a m1
    Nothing -> pure $ Map.insert k a2 m1

pattern Hash h = Causal.RawHash h

toList0 :: Branch0 m -> [(Path, Branch0 m)]
toList0 = go Path.empty where
  go p b = (p, b) : (Map.toList (_children b) >>= (\(seg, (_h, cb)) ->
    go (Path.snoc p seg) (head cb) ))

printDebugPaths :: Branch m -> String
printDebugPaths = unlines . map show . Set.toList . debugPaths

debugPaths :: Branch m -> Set (Path, Hash)
debugPaths = go Path.empty where
  go p b = Set.insert (p, headHash b) . Set.unions $
    [ go (Path.snoc p seg) b | (seg, (_,b)) <- Map.toList $ _children (head b) ]

data Target = TargetType | TargetTerm | TargetBranch
  deriving (Eq, Ord, Show)

instance Eq (Branch0 m) where
  a == b = view terms a == view terms b
    && view types a == view types b
    && view children a == view children b
    && (fmap fst . view edits) a == (fmap fst . view edits) b

data ForkFailure = SrcNotFound | DestExists

-- consider delegating to Names.numHashChars when ready to implement?
-- are those enough?
-- could move this to a read-only field in Branch0
-- could move a Names0 to a read-only field in Branch0 until it gets too big
numHashChars :: Branch m -> Int
numHashChars _b = 3

-- todo: Can this be made parametric on Causal2?
-- todo: Can it still quit once `missing` is empty?
findRefsInHistory :: forall m.
  Monad m => Set Reference -> Branch m -> m Names0
findRefsInHistory refs (Branch c) = go refs mempty [c] [] where
  -- double-ended queue, used to go fairly / breadth first through multiple tails.
  -- unsure as to whether I need to be passing a Names0 as an accumulator
  go :: Set Reference -> Set Hash -> [Causal m Raw (Branch0 m)] -> [Causal m Raw (Branch0 m)] -> m Names0
  go (toList -> []) _ _ _ = pure mempty
  go _missing _seen _deq@[] _enq@[] = pure mempty
  go missing seen [] enqueue = go missing seen (reverse enqueue) []
  go missing seen (c:rest) enqueue =
    if Set.member (Causal.currentHash c) seen then go missing seen rest enqueue
    else (getNames missing (Causal.head c) <>) <$> case c of
      Causal.One h _ -> go missing (Set.insert h seen) rest enqueue
      Causal.Cons h _ (_, mt) -> do
        t <- mt
        go missing (Set.insert h seen) rest (t : enqueue)
      Causal.Merge h _ mts -> do
        ts <- sequence $ toList mts
        go missing (Set.insert h seen) rest (ts ++ enqueue)
  getNames :: Set Reference -> Branch0 m -> Names0
  getNames rs b = Names terms' types' where
    Names terms types = toNames0 b
    terms' = terms R.|> Set.map Referent.Ref rs
    types' = types R.|> rs


-- Question: How does Deserialize throw a not-found error?
-- Question: What is the previous question?
read
  :: forall m
   . Monad m
  => Causal.Deserialize m Raw Raw
  -> (EditHash -> m Patch)
  -> Hash
  -> m (Branch m)
read deserializeRaw deserializeEdits h = Branch <$> Causal.read d h
 where
  fromRaw :: Raw -> m (Branch0 m)
  fromRaw Raw {..} = do
    children <- traverse go _childrenR
    edits <- for _editsR $ \hash -> (hash,) . pure <$> deserializeEdits hash
    pure $ branch0 _termsR _typesR children edits
  go h = (h, ) <$> read deserializeRaw deserializeEdits h
  d :: Causal.Deserialize m Raw (Branch0 m)
  d h = deserializeRaw h >>= \case
    RawOne raw      -> RawOne <$> fromRaw raw
    RawCons  raw h  -> flip RawCons h <$> fromRaw raw
    RawMerge raw hs -> flip RawMerge hs <$> fromRaw raw


-- serialize a `Branch m` indexed by the hash of its corresponding Raw
sync :: forall m. Monad m
     => (Hash -> m Bool)
     -> Causal.Serialize m Raw Raw
     -> (EditHash -> m Patch -> m ())
     -> Branch m
     -> m ()
sync exists serializeRaw serializeEdits b = do
  for_ (view children (head b)) (sync exists serializeRaw serializeEdits . snd)
  for_ (view edits (head b)) (uncurry serializeEdits)
  Causal.sync exists serialize0 (view history b)
  where
  toRaw :: Branch0 m -> Raw
  toRaw Branch0{..} = Raw _terms _types (fst <$> _children) (fst <$> _edits)
  serialize0 :: Causal.Serialize m Raw (Branch0 m)
  serialize0 h = \case
    RawOne b0 -> serializeRaw h $ RawOne (toRaw b0)
    RawCons b0 ht -> serializeRaw h $ RawCons (toRaw b0) ht
    RawMerge b0 hs -> serializeRaw h $ RawMerge (toRaw b0) hs

  -- this has to serialize the branch0 and its descendants in the tree,
  -- and then serialize the rest of the history of the branch as well

-- copy a path to another path
fork
  :: Applicative m
  => Path
  -> Path
  -> Branch m
  -> Either ForkFailure (Branch m)
fork src dest root = case getAt src root of
  Nothing -> Left SrcNotFound
  Just src' -> case setIfNotExists dest src' root of
    Nothing -> Left DestExists
    Just root' -> Right root'

-- Move the node at src to dest.
-- It's okay if `dest` is inside `src`, just create empty levels.
-- Try not to `step` more than once at each node.
move :: Applicative m
     => Path
     -> Path
     -> Branch m
     -> Either ForkFailure (Branch m)
move src dest root = case getAt src root of
  Nothing -> Left SrcNotFound
  Just src' ->
    -- make sure dest doesn't already exist
    case getAt dest root of
      Just _destExists -> Left DestExists
      Nothing ->
      -- find and update common ancestor of `src` and `dest`:
        Right $ modifyAt ancestor go root
        where
        (ancestor, relSrc, relDest) = Path.relativeToAncestor src dest
        go = deleteAt relSrc . setAt relDest src'

setIfNotExists
  :: Applicative m => Path -> Branch m -> Branch m -> Maybe (Branch m)
setIfNotExists dest b root = case getAt dest root of
  Just _destExists -> Nothing
  Nothing -> Just $ setAt dest b root

setAt :: Applicative m => Path -> Branch m -> Branch m -> Branch m
setAt path b = modifyAt path (const b)

deleteAt :: Applicative m => Path -> Branch m -> Branch m
deleteAt path = setAt path empty

-- returns `Nothing` if no Branch at `path`
getAt :: Path
      -> Branch m
      -> Maybe (Branch m)
getAt path root = case Path.toList path of
  [] -> if isEmpty root then Nothing else Just root
  seg : path -> case Map.lookup seg (_children $ head root) of
    Just (_h, b) -> getAt (Path.fromList path) b
    Nothing -> Nothing

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

getAt0 :: Path -> Branch0 m -> Branch0 m
getAt0 p b = case Path.toList p of
  [] -> b
  seg : path -> case Map.lookup seg (_children b) of
    Just (_h, c) -> getAt0 (Path.fromList path) (head c)
    Nothing -> empty0

empty :: Branch m
empty = Branch $ Causal.one empty0

one :: Branch0 m -> Branch m
one = Branch . Causal.one

empty0 :: Branch0 m
empty0 = Branch0 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

isEmpty0 :: Branch0 m -> Bool
isEmpty0 = (== empty0)

isEmpty :: Branch m -> Bool
isEmpty = (== empty)

step :: Applicative m => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = over history (Causal.stepDistinct f)

stepM :: Monad m => (Branch0 m -> m (Branch0 m)) -> Branch m -> m (Branch m)
stepM f = mapMOf history (Causal.stepDistinctM f)

cons :: Applicative m => Branch0 m -> Branch m -> Branch m
cons = step . const

isOne :: Branch m -> Bool
isOne (Branch Causal.One{}) = True
isOne _ = False

uncons :: Applicative m => Branch m -> m (Maybe (Branch0 m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b where
  go = over (_Just . _2) Branch

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAt :: forall m. Applicative m
       => Path
       -> (Branch0 m -> Branch0 m)
       -> Branch m -> Branch m
stepAt p f = modifyAt p g where
  g :: Branch m -> Branch m
  g (Branch b) = Branch . Causal.consDistinct (f (Causal.head b)) $ b

stepManyAt :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m) -> Branch m -> Branch m
stepManyAt actions = step (stepManyAt0 actions)

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAtM :: forall n m. (Functor n, Applicative m)
        => Path -> (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepAtM p f = modifyAtM p g where
  g :: Branch m -> n (Branch m)
  g (Branch b) = do
    b0' <- f (Causal.head b)
    pure $ Branch . Causal.consDistinct b0' $ b

stepManyAtM :: (Monad m, Foldable f)
            => f (Path, Branch0 m -> m (Branch0 m)) -> Branch m -> m (Branch m)
stepManyAtM actions = stepM (stepManyAt0M actions)

-- starting at the leaves, apply `f` to every level of the branch.
stepAll :: Applicative m => (Branch0 m -> Branch0 m) -> (Branch0 m -> Branch0 m)
stepAll f Branch0{..} = f (branch0 _terms _types children _edits) where
  children = fmap (second (step f)) _children

-- Creates a function to fix up the children field._1
-- If the action emptied a child, then remove the mapping,
-- otherwise update it.
-- Todo: Fix this in hashing & serialization instead of here?
getChildBranch :: NameSegment -> Branch0 m -> Branch m
getChildBranch seg b = maybe empty snd $ Map.lookup seg (_children b)

setChildBranch :: NameSegment -> Branch m -> Branch0 m -> Branch0 m
setChildBranch seg b = over children (updateChildren seg b)

getPatch :: Applicative m => NameSegment -> Branch0 m -> m Patch
getPatch seg b = case Map.lookup seg (_edits b) of
  Nothing -> pure Patch.empty
  Just (_, p) -> p

modifyEdits :: Monad m => NameSegment -> (Patch -> Patch) -> Branch0 m -> m (Branch0 m)
modifyEdits seg f = mapMOf edits update where
  update m = do
    p' <- case Map.lookup seg m of
      Nothing -> pure $ f Patch.empty
      Just (_, p) -> f <$> p
    let h = H.accumulate' p'
    pure $ Map.insert seg (h, pure p') m

updateChildren ::NameSegment
               -> Branch m
               -> Map NameSegment (Hash, Branch m)
               -> Map NameSegment (Hash, Branch m)
updateChildren seg updatedChild =
  if isEmpty updatedChild
  then Map.delete seg
  else Map.insert seg (headHash updatedChild, updatedChild)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAt :: Applicative m
  => Path -> (Branch m -> Branch m) -> Branch m -> Branch m
modifyAt path f = runIdentity . modifyAtM path (pure . f)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAtM
  :: forall n m
   . Functor n
  => Applicative m -- because `Causal.cons` uses `pure`
  => Path
  -> (Branch m -> n (Branch m))
  -> Branch m
  -> n (Branch m)
modifyAtM path f b = case Path.toList path of
  [] -> f b
  seg : path -> do -- Functor
    let child = getChildBranch seg (head b)
    child' <- modifyAtM (Path.fromList path) f child
    -- step the branch by updating its children according to fixup
    pure $ step (setChildBranch seg child') b

stepAt0 :: Applicative m => Path
                         -> (Branch0 m -> Branch0 m)
                         -> Branch0 m -> Branch0 m
stepAt0 p f = runIdentity . stepAt0M p (pure . f)

-- stepManyAt consolidates several changes into a single step,
-- by starting at the leaves and working up to the root
-- use Unison.Util.List.groupBy to merge the Endos at each Path
-- todo: reimplement this using step, not stepAt, to preserve the property
-- that each path is only stepped once.
stepManyAt0 :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m)
           -> Branch0 m -> Branch0 m
stepManyAt0 actions b = let
  -- paths are ordered lexicographically, so parents will appear before their children
  -- we reverse this so children are stepped before their parents
  actions' = reverse . Map.toList $ combine <$> List.multimap actions
  combine = foldl' (flip (.)) id
  in foldl' (\b (p, f) -> stepAt0 p f b) b actions'

-- todo: reimplement this using stepM, not stepAtM, to preserve the property
-- that each path is only stepped once.
stepManyAt0M :: (Monad m, Foldable f)
             => f (Path, Branch0 m -> m (Branch0 m))
             -> Branch0 m -> m (Branch0 m)
stepManyAt0M actions b = let
  -- paths are ordered lexicographically, so parents will appear before their children
  -- we reverse this so children are stepped before their parents
  actions' = reverse . Map.toList $ combine <$> List.multimap actions
  combine = foldl' (\f g x -> f x >>= g) pure
  in Monad.foldM (\b (p, f) -> stepAt0M p f b) b actions'

stepAt0M :: forall n m. (Functor n, Applicative m)
         => Path
         -> (Branch0 m -> n (Branch0 m))
         -> Branch0 m -> n (Branch0 m)
stepAt0M p f b = case Path.uncons p of
  Nothing -> f b
  Just (seg, path) -> do
    let child = getChildBranch seg b
    child0' <- stepAt0M path f (head child)
    pure $ setChildBranch seg (cons child0' child) b

instance Hashable (Branch0 m) where
  tokens b =
    [ H.accumulateToken (_terms b)
    , H.accumulateToken (_types b)
    , H.accumulateToken (fst <$> _children b)
    ]

-- getLocalBranch :: Hash -> IO Branch
-- getGithubBranch :: RemotePath -> IO Branch
-- getLocalEdit :: GUID -> IO Patch

-- todo: consider inlining these into Actions2
addTermName :: Referent -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTermName r new md =
  over terms (Metadata.insertWithMetadata (r,md) . Star3.insertD1 (r,new))

addTypeName :: Reference -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTypeName r new md =
  over types (Metadata.insertWithMetadata (r,md) . Star3.insertD1 (r,new))

-- addTermNameAt :: Path.Split -> Referent -> Branch0 m -> Branch0 m
-- addTypeNameAt :: Path.Split -> Reference -> Branch0 m -> Branch0 m

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b | Star3.memberD1 (r,n) (view terms b)
                     = over terms (Star3.deletePrimaryD1 (r,n)) b
deleteTermName _ _ b = b

deleteTypeName :: Reference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b | Star3.memberD1 (r,n) (view types b)
                     = over types (Star3.deletePrimaryD1 (r,n)) b
deleteTypeName _ _ b = b

data RefCollisions =
  RefCollisions { termCollisions :: Relation Name Name
                , typeCollisions :: Relation Name Name
                } deriving (Eq, Show)

instance Semigroup RefCollisions where
  (<>) = mappend
instance Monoid RefCollisions where
  mempty = RefCollisions mempty mempty
  mappend r1 r2 = RefCollisions (termCollisions r1 <> termCollisions r2)
                                (typeCollisions r1 <> typeCollisions r2)
