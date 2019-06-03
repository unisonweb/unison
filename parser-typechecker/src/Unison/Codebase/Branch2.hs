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

-- import qualified Unison.Codebase.Branch as Branch

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
import qualified Control.Monad                 as Monad
--import           Control.Monad.Extra            ( whenM )
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
import           Unison.Codebase.Path           ( NameSegment
                                                , Path(Path)
                                                )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Hash                   as Hash
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified          as HQ
import qualified Unison.ShortHash as SH


import           Unison.Name                    ( Name )
import           Unison.Names2                  ( Names'(Names), Names, Names0 )
import qualified Unison.Names2                 as Names
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent(Con,Ref) )
import qualified Unison.Referent              as Referent
import qualified Unison.Reference             as Reference

import qualified Unison.Util.Relation          as R
import           Unison.Util.Relation           ( Relation )
import qualified Unison.Util.List              as List


-- type EditGuid = Text

newtype Branch m = Branch { _history :: Causal m Raw (Branch0 m) }
  deriving (Eq, Ord)

-- used by `fold`
data BranchEntry
  = TermEntry Referent
  | TypeEntry Reference
  deriving (Eq,Ord,Show)

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> Hash
headHash (Branch c) = Causal.currentHash c

merge :: Monad m => Branch m -> Branch m -> m (Branch m)
merge (Branch x) (Branch y) = Branch <$> Causal.mergeWithM merge0 x y

merge0 :: forall m. Monad m => Branch0 m -> Branch0 m -> m (Branch0 m)
merge0 b1 b2 = unionWithM f (_children b1) (_children b2)
  <&> \c ->
    Branch0 (_terms b1 <> _terms b2)
            (_types b1 <> _types b2)
            c
            (error "todo: merge edits")
            (toNamesSeg b1 <> toNamesSeg b2)
            (deepReferents b1 <> deepReferents b2)
            (deepTypeReferences b1 <> deepTypeReferences b2)
  where
  f :: (h1, Branch m) -> (h2, Branch m) -> m (Hash, Branch m)
  f (_h1, b1) (_h2, b2) = do b <- merge b1 b2; pure (headHash b, b)

unionWithM :: forall m k a.
  (Monad m, Ord k) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f m1 m2 = Monad.foldM go m1 $ Map.toList m2 where
  go :: Map k a -> (k, a) -> m (Map k a)
  go m1 (k, a2) = case Map.lookup k m1 of
    Just a1 -> do a <- f a1 a2; pure $ Map.insert k a m1
    Nothing -> pure $ Map.insert k a2 m1

type Hash = Causal.RawHash Raw
type EditHash = Hash.Hash

data Branch0 m = Branch0
  { _terms :: Relation NameSegment Referent
  , _types :: Relation NameSegment Reference
  , _children :: Map NameSegment (Hash, Branch m) --todo: can we get rid of this hash
  , _edits :: Map NameSegment (EditHash, m Edits)
  , toNamesSeg :: Names.NamesSeg
  , deepReferents :: Set Referent
  , deepTypeReferences :: Set Reference
  }

data Target = TargetType | TargetTerm | TargetBranch
  deriving (Eq, Ord, Show)

-- The raw Branch
data Raw = Raw
  { _termsR :: Relation NameSegment Referent
  , _typesR :: Relation NameSegment Reference
  , _childrenR :: Map NameSegment Hash
  , _editsR :: Map NameSegment EditHash
  }

-- todo: move Edits to its own module?
data Edits = Edits
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  } deriving (Eq, Ord)

makeLenses ''Raw
makeLenses ''Branch0
makeLenses ''Branch
makeLenses ''Edits

instance Eq (Branch0 m) where
  a == b = view terms a == view terms b
    && view types a == view types b
    && view children a == view children b
    && (fmap fst . view edits) a == (fmap fst . view edits) b

data ForkFailure = SrcNotFound | DestExists

fold :: forall m a. (a -> Name -> BranchEntry -> a) -> a -> Branch0 m -> a
fold f a b = go Path.empty b a where
  doTerm p a (seg, r) = f a (Path.toName (p `Path.snoc` seg)) (TermEntry r)
  doType p a (seg, r) = f a (Path.toName (p `Path.snoc` seg)) (TypeEntry r)
  doChild p a (seg, (_hash, head -> b)) = go (p `Path.snoc` seg) b a
  go :: Path -> Branch0 m -> a -> a
  go p b a = let
    a1 = foldl' (doTerm p) a (R.toList . view terms $ b)
    a2 = foldl' (doType p) a1 (R.toList . view types $ b)
    in foldl' (doChild p) a2 (Map.toList . view children $ b)

foldM :: forall m a. Monad m
      => (a -> Name -> BranchEntry -> m a) -> a -> Branch0 m -> m a
foldM f a b = go Path.empty b a where
  doTerm p a (seg, r) = f a (Path.toName (p `Path.snoc` seg)) (TermEntry r)
  doType p a (seg, r) = f a (Path.toName (p `Path.snoc` seg)) (TypeEntry r)
  doChild p a (seg, (_hash, head -> b)) = go (p `Path.snoc` seg) b a
  go :: Path -> Branch0 m -> a -> m a
  go p b a = do
    a1 <- Monad.foldM (doTerm p) a (R.toList . view terms $ b)
    a2 <- Monad.foldM (doType p) a1 (R.toList . view types $ b)
    Monad.foldM (doChild p) a2 (Map.toList . view children $ b)

-- consider delegating to Names.numHashChars when ready to implement?
-- are those enough?
-- could move this to a read-only field in Branch0
-- could move a Names0 to a read-only field in Branch0 until it gets too big
numHashChars :: Branch m -> Int
numHashChars _b = 3

toNames :: Branch0 m -> Names
toNames b = Names hqTerms hqTypes where
  names0 = toNames0 b
  hqTerms = R.fromList [ (Names.hqTermName names0 n r, r)
                       | (n, r) <- R.toList (Names.terms names0) ]
  hqTypes = R.fromList [ (Names.hqTypeName names0 n r, r)
                       | (n, r) <- R.toList (Names.types names0) ]

toNames0 :: Branch0 m -> Names0
toNames0 b = fold go mempty b where
  go names name (TermEntry r) = names <> Names.fromTerms [(name, r)]
  go names name (TypeEntry r) = names <> Names.fromTypes [(name, r)]

allEntries :: Branch0 m -> [(Name, BranchEntry)]
allEntries = reverse . fold (\l n e -> (n, e) : l) []

-- asSearchResults :: Branch m -> [SearchResult]

-- Question: How does Deserialize throw a not-found error?
-- Question: What is the previous question?
read
  :: forall m
   . Monad m
  => Causal.Deserialize m Raw Raw
  -> (EditHash -> m Edits)
  -> Hash
  -> m (Branch m)
read deserializeRaw deserializeEdits h = Branch <$> Causal.read d h
 where
  fromRaw :: Raw -> m (Branch0 m)
  fromRaw Raw {..} = do
    children <- traverse go _childrenR
    let namesSeg = toNamesSegImpl _termsR _typesR
        childrenBranch0 = fmap (head . snd) . Foldable.toList $ children
        deepReferents' = foldMap deepReferents childrenBranch0
        deepTypeReferences' = foldMap deepTypeReferences childrenBranch0
    edits <- for _editsR $ \hash -> (hash,) . pure <$> deserializeEdits hash
    pure $ Branch0 _termsR _typesR children edits
                    namesSeg
                    deepReferents'
                    deepTypeReferences'
  go h = (h, ) <$> read deserializeRaw deserializeEdits h
  d :: Causal.Deserialize m Raw (Branch0 m)
  d h = deserializeRaw h >>= \case
    RawOne raw      -> RawOne <$> fromRaw raw
    RawCons  raw h  -> flip RawCons h <$> fromRaw raw
    RawMerge raw hs -> flip RawMerge hs <$> fromRaw raw
  toNamesSegImpl :: Relation NameSegment Referent
                 -> Relation NameSegment Reference
                 -> Names' (HQ.HashQualified' NameSegment)
  toNamesSegImpl terms types = Names terms' types' where
    terms' = R.map (\(n, r) -> (Names.hqTermName names n r, r)) terms
    types' = R.map (\(n, r) -> (Names.hqTypeName names n r, r)) types
    names :: Names' NameSegment
    names = Names terms types


-- serialize a `Branch m` indexed by the hash of its corresponding Raw
sync :: forall m. Monad m
     => (Hash -> m Bool)
     -> Causal.Serialize m Raw Raw
     -> (EditHash -> m Edits -> m ())
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
    RawCons b0 h -> serializeRaw h $ RawCons (toRaw b0) h
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
  [] -> if isEmpty (head root) then Nothing else Just root
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

empty0 :: Branch0 m
empty0 = Branch0 mempty mempty mempty mempty mempty mempty mempty

isEmpty :: Branch0 m -> Bool
isEmpty = (== empty0)

step :: Applicative m => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = over history (Causal.stepDistinct f)

cons :: Applicative m => Branch0 m -> Branch m -> Branch m
cons = step . const

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAt :: forall m. Applicative m
       => Path
       -> (Branch0 m -> Branch0 m)
       -> Branch m -> Branch m
stepAt p f b = modifyAt p g b where
  g :: Branch m -> Branch m
  g (Branch b) = Branch . Causal.consDistinct (f (Causal.head b)) $ b

stepManyAt :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m) -> Branch m -> Branch m
stepManyAt actions = step (stepManyAt0 actions)

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAtM :: forall n m. (Functor n, Applicative m)
        => Path -> (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepAtM p f b = modifyAtM p g b where
  g :: Branch m -> n (Branch m)
  g (Branch b) = do
    b0' <- f (Causal.head b)
    pure $ Branch . Causal.consDistinct b0' $ b

-- Creates a function to fix up the children field._1
-- If the action emptied a child, then remove the mapping,
-- otherwise update it.
-- Todo: Fix this in hashing & serialization instead of here?
getChildBranch :: NameSegment -> Branch0 m -> Branch m
getChildBranch seg b = maybe empty snd $ Map.lookup seg (_children b)

setChildBranch :: NameSegment -> Branch m -> Branch0 m -> Branch0 m
setChildBranch seg b = over children (updateChildren seg b)

updateChildren ::
  NameSegment -> Branch m -> Map NameSegment (Hash, Branch m)
                          -> Map NameSegment (Hash, Branch m)
updateChildren seg updatedChild =
  if isEmpty (head updatedChild)
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
stepManyAt0 :: (Applicative m, Foldable f)
           => f (Path, Branch0 m -> Branch0 m)
           -> Branch0 m -> Branch0 m
stepManyAt0 = error "todo"

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
    [ H.accumulateToken . R.toList $ (_terms b)
    , H.accumulateToken . R.toList $ (_types b)
    , H.accumulateToken (fst <$> _children b)
    ]

-- getLocalBranch :: Hash -> IO Branch
-- getGithubBranch :: RemotePath -> IO Branch
-- getLocalEdit :: GUID -> IO Edits

-- makeLenses ''Namespace
-- makeLenses ''Edits

-- todo: consider inlining these into Actions2
addTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
addTermName r new = over terms (R.insert new r)

addTypeName :: Reference -> NameSegment -> Branch0 m -> Branch0 m
addTypeName r new = over types (R.insert new r)

-- addTermNameAt :: Path.Split -> Referent -> Branch0 m -> Branch0 m
-- addTypeNameAt :: Path.Split -> Reference -> Branch0 m -> Branch0 m

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b | R.member n r (view terms b)
                     = over terms (R.insert n r) $ b
deleteTermName _ _ b = b

deleteTypeName :: Reference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b | R.member n r (view types b)
                     = over types (R.insert n r) $ b
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
