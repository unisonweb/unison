-- {-# OPTIONS_GHC -Wwarn #-} -- todo: remove me later
{-# OPTIONS_GHC -Wno-unused-imports #-}

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

import           Control.Lens            hiding ( children, transform )
import qualified Control.Monad                 as Monad
--import           Control.Monad.Extra            ( whenM )
-- import           Data.GUID                (genText)
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
import           Data.Foldable                  ( for_ )
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
--import           Unison.Hash                    ( Hash )
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
  <&> Branch0 (_terms b1 <> _terms b2) (_types b1 <> _types b2)
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

data Branch0 m = Branch0
  { _terms :: Relation NameSegment Referent
  , _types :: Relation NameSegment Reference
--  , _edits :: Relation NameSegment Edits
  -- Q: How will we handle merges and conflicts for `children`?
  --    Should this be a relation?
  --    What is the UX to resolve conflicts?
  -- The hash we use to identify branches is the hash of their Causal node.
  , _children :: Map NameSegment (Hash, Branch m)
  }

-- The raw Branch
data Raw = Raw
  { _termsR :: Relation NameSegment Referent
  , _typesR :: Relation NameSegment Reference
  , _childrenR :: Map NameSegment Hash
  }

-- todo: move Edits to its own module?
data Edits = Edits
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  }

makeLenses ''Raw
makeLenses ''Branch0
makeLenses ''Branch
makeLenses ''Edits

instance Eq (Branch0 m) where
  a == b = view terms a == view terms b
    && view types a == view types b
    && view children a == view children b

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
  -> Hash
  -> m (Branch m)
read deserializeRaw h = Branch <$> Causal.read d h
 where
  fromRaw :: Raw -> m (Branch0 m)
  fromRaw Raw {..} = Branch0 _termsR _typesR <$> (traverse go _childrenR)
  go h = (h, ) <$> read deserializeRaw h
  d :: Causal.Deserialize m Raw (Branch0 m)
  d h = deserializeRaw h >>= \case
    RawOne raw      -> RawOne <$> fromRaw raw
    RawCons  raw h  -> flip RawCons h <$> fromRaw raw
    RawMerge raw hs -> flip RawMerge hs <$> fromRaw raw

-- serialize a `Branch m` indexed by the hash of its corresponding Raw
sync :: forall m. Monad m
     => (Hash -> m Bool)
     -> Causal.Serialize m Raw Raw
     -> Branch m
     -> m ()
sync exists serializeRaw b = do
  for_ (view children (head b)) (sync exists serializeRaw . snd)
  Causal.sync exists serialize0 (view history b)
  where
  toRaw :: Branch0 m -> Raw
  toRaw Branch0{..} = Raw _terms _types (fst <$> _children)
  serialize0 :: Causal.Serialize m Raw (Branch0 m)
  serialize0 h = \case
    RawOne b0 -> serializeRaw h $ RawOne (toRaw b0)
    RawCons b0 h -> serializeRaw h $ RawCons (toRaw b0) h
    RawMerge b0 hs -> serializeRaw h $ RawMerge (toRaw b0) hs

  -- this has to serialize the branch0 and its descendants in the tree,
  -- and then serialize the rest of the history of the branch as well

-- copy a path to another path
fork
  :: Monad m
  => Path
  -> Path
  -> Branch m
  -> m (Either ForkFailure (Branch m))
fork src dest root = case getAt src root of
  Nothing -> pure $ Left SrcNotFound
  Just src' -> setIfNotExists dest src' root >>= \case
    Nothing -> pure $ Left DestExists
    Just root' -> pure $ Right root'

-- Move the node at src to dest.
-- It's okay if `dest` is inside `src`, just create empty levels.
-- Try not to `step` more than once at each node.
move :: Monad m
     => Path
     -> Path
     -> Branch m
     -> m (Either ForkFailure (Branch m))
move src dest root = case getAt src root of
  Nothing -> pure $ Left SrcNotFound
  Just src' ->
    -- make sure dest doesn't already exist
    case getAt dest root of
      Just _destExists -> pure $ Left DestExists
      Nothing ->
      -- find and update common ancestor of `src` and `dest`:
        Right <$> modifyAtM ancestor go root
        where
        (ancestor, relSrc, relDest) = Path.relativeToAncestor src dest
        go b = do
-- todo: can we combine these into one update, eliminating Monad constraint
          b <- setAt relDest src' b
          deleteAt relSrc b

setIfNotExists
  :: Monad m => Path -> Branch m -> Branch m -> m (Maybe (Branch m))
setIfNotExists dest b root = case getAt dest root of
  Just _destExists -> pure Nothing
  Nothing -> Just <$> setAt dest b root

setAt :: Monad m => Path -> Branch m -> Branch m -> m (Branch m)
setAt path b = modifyAt path (const b)

deleteAt :: Monad m => Path -> Branch m -> m (Branch m)
deleteAt path = setAt path empty

transform :: Functor m => (forall a . m a -> n a) -> Branch m -> Branch n
transform f b = case _history b of
  causal -> Branch . Causal.transform f $ transformB0s f causal
  where
  transformB0 :: Functor m => (forall a . m a -> n a) -> Branch0 m -> Branch0 n
  transformB0 f = over (children.mapped._2) (transform f)

  transformB0s :: Functor m => (forall a . m a -> n a)
               -> Causal m Raw (Branch0 m)
               -> Causal m Raw (Branch0 n)
  transformB0s f = Causal.unsafeMapHashPreserving (transformB0 f)

-- returns `Nothing` if no Branch at `path`
getAt :: Path
      -> Branch m
      -> Maybe (Branch m)
-- todo: return Nothing if exists but is empty
getAt path root = case Path.toList path of
  [] -> Just root
  seg : path -> case Map.lookup seg (_children $ head root) of
    Just (_h, b) -> getAt (Path.fromList path) b
    Nothing -> Nothing

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

empty :: Branch m
empty = Branch $ Causal.one empty0

empty0 :: Branch0 m
empty0 = Branch0 mempty mempty mempty

isEmpty :: Branch0 m -> Bool
isEmpty = (== empty0)

step :: Applicative m => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = over history (Causal.stepDistinct f)

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
-- todo: consider adding logic somewhere to skip the cons if `f` is a no-op.
stepAt :: Monad m
       => Path
       -> (Branch0 m -> Branch0 m)
       -> Branch m
       -> m (Branch m)
stepAt path f = stepAtM path (pure . f)

-- Modify the branch0 at the head of at `path` with `f`,
-- after creating it if necessary.  Preserves history.
stepAtM
  :: Monad m => Path -> (Branch0 m -> m (Branch0 m)) -> Branch m -> m (Branch m)
stepAtM path f =
  modifyAtM path (fmap Branch . Causal.stepDistinctM f . view history)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAt :: Monad m
  => Path -> (Branch m -> Branch m) -> Branch m -> m (Branch m)
modifyAt path f = modifyAtM path (pure . f)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAtM
  :: forall n m
   . Monad n
  => Applicative m -- because `Causal.cons` uses `pure`
  => Path
  -> (Branch m -> n (Branch m))
  -> Branch m
  -> n (Branch m)
modifyAtM path f b = case Path.toList path of
  [] -> f b
  seg : path -> let
    -- fixup :: ChildMap -> ChildMap
    fixup seg b =
      if isEmpty (head b)
      then Map.delete seg
      else Map.insert seg (headHash b, b)
    child = case Map.lookup seg (_children $ head b) of
      Nothing -> empty
      Just (_h, b) -> b
    in do
      child' <- modifyAtM (Path.fromList path) f child
      pure $ step (over children (fixup seg child')) b


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
