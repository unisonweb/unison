{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Branch where

-- import Debug.Trace
import           Control.Lens
import           Control.Monad            (join)
import           Data.Bifunctor           (bimap)
import           Data.Foldable
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Prelude                  hiding (head,subtract)
import           Unison.Codebase.Causal   (Causal)
import qualified Unison.Codebase.Causal   as Causal
import           Unison.Codebase.TermEdit (TermEdit, Typing)
import qualified Unison.Codebase.TermEdit as TermEdit
import           Unison.Codebase.TypeEdit (TypeEdit)
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Unison.DataDeclaration as DD
import           Unison.Hash              (Hash)
import           Unison.Hashable          (Hashable)
import qualified Unison.Hashable          as H
import           Unison.Names             (Name, Names (..))
import qualified Unison.Names             as Names
import           Unison.Reference         (Reference)
import qualified Unison.Reference         as Reference
import           Unison.Referent          (Referent)
import qualified Unison.Referent          as Referent
import qualified Unison.UnisonFile        as UF
import           Unison.Util.Relation     (Relation)
import qualified Unison.Util.Relation     as R
import           Unison.Util.TransitiveClosure (transitiveClosure)
import           Unison.PrettyPrintEnv    (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnv    as PPE
import           Unison.Var               (Var)
import qualified Unison.Var               as Var

-- todo:
-- probably should refactor Reference to include info about whether it
-- is a term reference, a type decl reference, or an effect decl reference
-- (maybe combine last two)
--
-- While we're at it, should add a `Cycle Int [Reference]` for referring to
-- an element of a cycle of references.
--
-- If we do that, can implement various operations safely since we'll know
-- if we are referring to a term or a type (and can prevent adding a type
-- reference to the term namespace, say)

-- A `Branch`, `b` should likely maintain that:
--
--  * If `r : Reference` is in `codebase b` or one of its
--    transitive dependencies then `b` should have a `Name` for `r`.
--
-- This implies that if you depend on some code, you pick names for that
-- code. The editing tool will likely pick names based on some convention.
-- (like if you import and use `Runar.foo` in a function you write, it will
--  republished under `dependencies.Runar`. Could also potentially put
--  deps alongside the namespace...)
--
-- Thought was that basically don't need `Release`, it's just that
-- some branches are unconflicted and we might indicate that in some way
-- in the UI.
--
-- To "delete" a definition, just remove it from the map.
--
-- Operations around making transitive updates, resolving conflicts...
-- determining remaining work before one branch "covers" another...

data Namespace =
  Namespace { _terms :: Relation Name Referent
            , _types :: Relation Name Reference
            } deriving (Eq, Show)

makeLenses ''Namespace

data Branch0 =
  Branch0 { _namespaceL :: Namespace
          -- oldNamespace contains historic names for hashes
          , _oldNamespaceL :: Namespace
          , _editedTermsL :: Relation Reference TermEdit
          , _editedTypesL :: Relation Reference TypeEdit
          } deriving (Eq, Show)

makeLenses ''Branch0

namespace, oldNamespace :: Branch0 -> Namespace
namespace = view namespaceL
oldNamespace = view oldNamespaceL

newtype Branch = Branch { unbranch :: Causal Branch0 } deriving (Eq, Show)

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

termNamespace :: Branch0 -> Relation Name Referent
termNamespace = view $ namespaceL . terms

typeNamespace :: Branch0 -> Relation Name Reference
typeNamespace = view $ namespaceL . types

allTerms :: Branch0 -> Set Referent
allTerms = R.ran . termNamespace

allTypes :: Branch0 -> Set Reference
allTypes = R.ran . typeNamespace

intersectNames :: Namespace -> Namespace -> Namespace
intersectNames n1 n2 = Namespace terms types
 where
  -- `set R.<| rel` filters `rel` to contain tuples whose first elem is in `set`
  terms =
    Set.intersection (R.dom $ _terms n1) (R.dom $ _terms n2)
      R.<| _terms n1
  types =
    Set.intersection (R.dom $ _types n1) (R.dom $ _types n2)
      R.<| _types n1

intersectRefs :: Namespace -> Namespace -> Namespace
intersectRefs n1 n2 = Namespace terms types
 where
  terms =
    (    _terms n1
    R.|> (Set.intersection (R.ran $ _terms n1)
                           (R.ran $ _terms n2)
         )
    )
  types =
    (    _types n1
    R.|> (Set.intersection (R.ran $ _types n1)
                           (R.ran $ _types n2)
         )
    )

oldTermNamespace :: Branch0 -> Relation Name Referent
oldTermNamespace = view (oldNamespaceL . terms)

oldTypeNamespace :: Branch0 -> Relation Name Reference
oldTypeNamespace = view (oldNamespaceL . types)

instance Semigroup Namespace where
  Namespace terms1 types1 <> Namespace terms2 types2 =
    Namespace (terms1 <> terms2) (types1 <> types2)

instance Monoid Namespace where
  mempty = Namespace R.empty R.empty

editedTerms :: Branch0 -> Relation Reference TermEdit
editedTerms = view editedTermsL

editedTypes :: Branch0 -> Relation Reference TypeEdit
editedTypes = view editedTypesL

one :: Branch0 -> Branch
one = Branch . Causal.one

allNamedReferences :: Branch0 -> Set Reference
allNamedReferences b = let
  termRefs = Set.map Referent.toReference . R.ran $ termNamespace b
  typeRefs = R.ran $ typeNamespace b
  in termRefs <> typeRefs

allNamedTypes :: Branch0 -> Set Reference
allNamedTypes = R.ran . typeNamespace

data Diff = Diff { ours :: Branch0, theirs :: Branch0 }

fromNames :: Names -> Branch0
fromNames names = Branch0 (Namespace terms types) mempty R.empty R.empty
 where
  terms = R.fromList . Map.toList $ Names.termNames names
  types = R.fromList . Map.toList $ Names.typeNames names

fromDeclaration :: Var v
  => v -> Reference -> Either (DD.EffectDeclaration' v a) (DD.DataDeclaration' v a)
  -> Branch0
fromDeclaration v r e = fromNames $ case e of
  Left e -> DD.effectDeclToNames v r e
  Right d -> DD.dataDeclToNames v r d

contains :: Branch0 -> Reference -> Bool
contains b r =
  R.memberRan (Referent.Ref r) (termNamespace b)
    || R.memberRan r (typeNamespace b)

subtract :: Branch0 -> Branch0 -> Branch0
subtract b1 b2 = ours $ diff' b1 b2

diff :: Branch -> Branch -> Diff
diff ours theirs =
  uncurry diff' $ join bimap (Causal.head . unbranch) (ours, theirs)

diff' :: Branch0 -> Branch0 -> Diff
diff' ours theirs =
  let to :: (Ord a, Ord b) => Set (a, b) -> Relation a b
      to = R.fromSet
      fro :: (Ord a, Ord b) => Relation a b -> Set (a, b)
      fro = R.toSet
      diffSet f =
        ( to (fro (f ours) `Set.difference` fro (f theirs))
        , to (fro (f theirs) `Set.difference` fro (f ours))
        )
      (ourTerms    , theirTerms    ) = diffSet termNamespace
      (ourTypes    , theirTypes    ) = diffSet typeNamespace
      (ourOldTerms , theirOldTerms ) = diffSet oldTermNamespace
      (ourOldTypes , theirOldTypes ) = diffSet oldTypeNamespace
      (ourTermEdits, theirTermEdits) = diffSet editedTerms
      (ourTypeEdits, theirTypeEdits) = diffSet editedTypes
  in  Diff
        (Branch0 (Namespace ourTerms ourTypes)
                 (Namespace ourOldTerms ourOldTypes)
                 ourTermEdits
                 ourTypeEdits
        )
        (Branch0 (Namespace theirTerms theirTypes)
                 (Namespace theirOldTerms theirOldTypes)
                 theirTermEdits
                 theirTypeEdits
        )

-- When adding a Reference `r` to a namespace as `n`:
--   * add names for all of its transitive dependencies to `backupNames`.
--   * cache its transitive dependencies in `transitiveDependencies`
--   * (q1) do we add r,n to backupNames? no
-- When removing a Reference `r` from a namespace:
--   * get its transitive dependencies `ds`
--   * remove `r` from dom(transitiveDependencies)
--   * for each `d <- ds`, if `d` isn't in ran(transitiveDependencies),
--                         then delete `d` from backupNames
--   * (q2) When renaming, do we need to update `backupNames`? no

instance Semigroup Branch0 where
  Branch0 n1 o1 e1 et1 <> Branch0 n2 o2 e2 et2 = Branch0
    (n1 <> n2)
    (o1 <> o2)
    (R.union e1 e2)
    (R.union et1 et2)

instance Monoid Branch0 where
  mempty = Branch0 mempty mempty R.empty R.empty
  mappend = (<>)

allNames :: Branch0 -> Set Name
allNames = Set.union <$> allTermNames <*> allTypeNames

allTermNames :: Branch0 -> Set Name
allTermNames = R.dom . termNamespace

allTypeNames :: Branch0 -> Set Name
allTypeNames b0 = R.dom (typeNamespace b0)

hasTermNamed :: Name -> Branch0 -> Bool
hasTermNamed n b = not . null $ termsNamed n b

hasTypeNamed :: Name -> Branch0 -> Bool
hasTypeNamed n b = not . null $ typesNamed n b

termsNamed :: Name -> Branch0 -> Set Referent
termsNamed name = R.lookupDom name . termNamespace

constructorsNamed :: Name -> Branch0 -> Set Referent
constructorsNamed n b = Set.filter Referent.isConstructor (termsNamed n b)

typesNamed :: Name -> Branch0 -> Set Reference
typesNamed name = R.lookupDom name . typeNamespace

namesForTerm :: Referent -> Branch0 -> Set Name
namesForTerm ref = R.lookupRan ref . termNamespace

namesForType :: Reference -> Branch0 -> Set Name
namesForType ref = R.lookupRan ref . typeNamespace

oldNamesForTerm :: Int -> Referent -> Branch0 -> Set Name
oldNamesForTerm numHashChars ref
  = Set.map (<> Text.pack (Referent.showShort numHashChars ref))
  . R.lookupRan ref
  . (view $ oldNamespaceL . terms)

oldNamesForType :: Int -> Reference -> Branch0 -> Set Name
oldNamesForType numHashChars ref
  = Set.map (<> Text.pack (Reference.showShort numHashChars ref))
  . R.lookupRan ref
  . (view $ oldNamespaceL . types)

prettyPrintEnv1 :: Branch0 -> PrettyPrintEnv
prettyPrintEnv1 b = PPE.PrettyPrintEnv terms types where
  numHashChars = 5 -- todo: compute from the branch0
  or :: Set a -> Set a -> Set a
  or s1 s2 = if Set.null s1 then s2 else s1
  terms r = multiset $ namesForTerm r b `or` oldNamesForTerm numHashChars r b
  types r = multiset $ namesForType r b `or` oldNamesForType numHashChars r b
  multiset ks = Map.fromList [ (k, 1) | k <- Set.toList ks ]

prettyPrintEnv :: [Branch0] -> PrettyPrintEnv
prettyPrintEnv = foldMap prettyPrintEnv1

before :: Branch -> Branch -> Bool
before b b2 = unbranch b `Causal.before` unbranch b2

-- Use e.g. by `conflicts termNamespace branch`
-- conflicts :: (Ord a, Ord b) => (Branch0 -> Relation a b) -> Branch0 -> Map a (Set b)
-- conflicts f = R.domain . conflicts'' . f

conflicts'' :: (Ord a, Ord b) => Relation a b -> Relation a b
conflicts'' r = R.filterDom ((>1). length . flip R.lookupDom r) r

conflicts' :: Branch0 -> Branch0
conflicts' b = Branch0 (Namespace (c termNamespace) (c typeNamespace))
                       mempty
                       (c editedTerms)
                       (c editedTypes)
  where c f = conflicts'' . f $ b


-- Use as `resolved editedTerms branch`
resolved :: Ord a => (Branch0 -> Relation a b) -> Branch0 -> Map a b
resolved f = resolved' . f where
  resolved' :: Ord a => Relation a b -> Map a b
  resolved' r = foldl' go Map.empty (R.dom r) where
    go m a =
      let bs = R.lookupDom a r
      in if Set.size bs == 1 then Map.insert a (Set.findMin bs) m else m


-- count of remaining work, including:
-- * conflicted thingies
-- * among unconflicted thingies:
--    * definitions depending on definitions that have been updated
--       * terms and types depending on updated types
--       * terms depending on updated terms
data RemainingWork
  = TermNameConflict Name (Set Referent)
  | TypeNameConflict Name (Set Reference)
  | TermEditConflict Reference (Set TermEdit)
  | TypeEditConflict Reference (Set TypeEdit)
  -- ObsoleteTerm r [(old,new)]: r depended on old, which has been updated to new
  | ObsoleteTerm Reference (Set (Reference, Either TermEdit TypeEdit))
  | ObsoleteType Reference (Set (Reference, TypeEdit))
  deriving (Eq, Ord, Show)

empty :: Branch
empty = Branch (Causal.one mempty)

merge :: Branch -> Branch -> Branch
merge (Branch b) (Branch b2) = Branch (Causal.merge b b2)

head :: Branch -> Branch0
head (Branch b) = Causal.head b

-- Returns the subset of `b0` whose names collide with elements of `b`
nameCollisions :: Branch0 -> Branch0 -> Branch0
nameCollisions b1 b2 =
  Branch0 (intersectNames (namespace b1) (namespace b2))
          (intersectNames (oldNamespace b1) (oldNamespace b2))
          R.empty
          R.empty

-- Returns names occurring in both branches that also have the same referent.
duplicates :: Branch0 -> Branch0 -> Branch0
duplicates b1 b2 =
  Branch0
    (Namespace (R.fromSet . Set.intersection (terms b1) $ terms b2)
               (R.fromSet . Set.intersection (types b1) $ types b2))
    (Namespace (R.fromSet . Set.intersection (oldTerms b1) $ oldTerms b2)
               (R.fromSet . Set.intersection (oldTypes b1) $ oldTypes b2))
    R.empty
    R.empty
 where
  terms    = R.toSet . termNamespace
  types    = R.toSet . typeNamespace
  oldTerms = R.toSet . oldTermNamespace
  oldTypes = R.toSet . oldTypeNamespace

-- Returns the subset of `b0` whose names collide with elements of `b`
-- (and don't have the same referent).
collisions :: Branch0 -> Branch0 -> Branch0
collisions b0 b = nameCollisions b0 b `subtract` duplicates b0 b

-- Like `collisions` but removes anything that's in a conflicted state
unconflictedCollisions :: Branch0 -> Branch0 -> Branch0
unconflictedCollisions b1 b2 =
  collisions (b1 `subtract` conflicts' b1)
             (b2 `subtract` conflicts' b2)

-- Returns the references that have different names in `a` vs `b`
differentNames :: Branch0 -> Branch0 -> RefCollisions
differentNames a b = RefCollisions collTerms collTypes
 where
  colls f b =
    R.fromMultimap
      . fmap
          (Set.unions . toList . Set.map
            (\n -> Set.fromList . toList . R.lookupRan n $ f b)
          )
      . R.domain
      . f
  collTerms = colls termNamespace b a
  collTypes = colls typeNamespace b a

-- Returns the subset of `b0` whose referents collide with elements of `b`
refCollisions :: Branch0 -> Branch0 -> Branch0
refCollisions b0 b = go b0 b `subtract` duplicates b0 b
 where
  -- `set R.<| rel` filters `rel` to contain tuples whose first elem is in `set`
  go b1 b2 = Branch0 (intersectRefs (namespace b1) (namespace b2))
                     (intersectRefs (oldNamespace b1) (oldNamespace b2))
                     R.empty
                     R.empty

-- todo: treat name collisions as edits to a branch
-- editsFromNameCollisions :: Codebase -> Branch0 -> Branch -> Branch

-- Promote a typechecked file to a `Branch0` which can be added to a `Branch`
fromTypecheckedFile
  :: forall v a . Var v => UF.TypecheckedUnisonFile v a -> Branch0
fromTypecheckedFile file =
  let
    toName      = Var.name
    hashedTerms = UF.hashTerms file
    ctors :: [(v, Referent)]
    ctors = Map.toList $ UF.hashConstructors file
    conNamespace =
      R.fromList [ (toName v, r) | (v, r@(Referent.Con _ _)) <- ctors ]
    reqNamespace =
      R.fromList [ (toName v, r) | (v, r@(Referent.Req _ _)) <- ctors ]
    termNamespace1 = R.fromList
      [ (toName v, Referent.Ref r) | (v, (r, _, _)) <- Map.toList hashedTerms ]
    typeNamespace1 = R.fromList
      [ (toName v, r) | (v, (r, _)) <- Map.toList (UF.dataDeclarations' file) ]
    typeNamespace2 = R.fromList
      [ (toName v, r)
      | (v, (r, _)) <- Map.toList (UF.effectDeclarations' file)
      ]
  in
    Branch0
      (Namespace
        (termNamespace1 `R.union` conNamespace `R.union` reqNamespace)
        (typeNamespace1 `R.union` typeNamespace2)
      )
      mempty
      R.empty
      R.empty

-- | Returns the types and terms, respectively, whose names occur in both
-- the branch and the file.
intersectWithFile
  :: forall v a
   . Var v
  => Branch0
  -> UF.TypecheckedUnisonFile v a
  -> (Set v, Set v)
intersectWithFile branch file =
  ( Set.union
    (Map.keysSet (UF.dataDeclarations' file) `Set.intersection` typeNames)
    (Map.keysSet (UF.effectDeclarations' file) `Set.intersection` typeNames)
  , Set.fromList
    $   UF.topLevelComponents file
    >>= (>>= (\(v, _, _) -> if Set.member v termNames then [v] else []))
  )
 where
  typeNames = Set.map (Var.named) $ allTypeNames branch
  termNames = Set.map (Var.named) $ allTermNames branch


modify :: (Branch0 -> Branch0) -> Branch -> Branch
modify f b@(Branch causal) = let
  b0 = head b
  b1 = f b0
  in if b1 == b0 then b
     else Branch $ Causal.cons b1 causal

append :: Branch0 -> Branch -> Branch
append b0 = modify (<> b0)

cons :: Branch0 -> Branch -> Branch
cons b0 = modify (const b0)

instance Semigroup Branch where
  (<>) = mappend

instance Monoid Branch where
  mempty = empty
  mappend = merge

data ReferenceOps m = ReferenceOps
  { isTerm       :: Reference -> m Bool
  , isType       :: Reference -> m Bool
  , dependencies :: Reference -> m (Set Reference)
  , dependents   :: Reference -> m (Set Reference)
  }

-- 0. bar depends on foo
-- 1. replace foo with foo'
-- 2. replace bar with bar' which depends on foo'
-- 3. replace foo' with foo''
-- "foo" points to foo''
-- "bar" points to bar'
--
-- foo -> Replace foo'
-- foo' -> Replace foo''
-- bar -> Replace bar'
--
-- foo -> Replace foo''
-- foo' -> Replace foo''
-- bar -> Replace bar'
--
-- foo -> Replace foo''
-- bar -> Replace bar''
-- foo' -> Replace foo'' *optional
-- bar' -> Replace bar'' *optional

replaceType :: Reference -> Reference -> Branch0 -> Branch0
replaceType old new b | old == new = b
replaceType old new b
  = over editedTypesL (R.insert old (TypeEdit.Replace new))
  . over (namespaceL . types)    (R.replaceRan old new)
  . over (namespaceL . terms)    (R.filterRan (not . isMatch))
  . over (oldNamespaceL . types) (R.union (typeNamespace b R.|> [old]))
  . over (oldNamespaceL . terms) (R.union (R.filterRan isMatch (termNamespace b)))
  $ b
  where
    isMatch r = case r of
      Referent.Con r _ -> r == old
      Referent.Req r _ -> r == old
      _ -> False

-- insertNames :: Monad m
--             => ReferenceOps m
--             -> Relation Reference Name
--             -> Reference -> m (Relation Reference Name)
-- insertNames ops m r = foldl' (flip $ R.insert r) m <$> name ops r

replaceTerm :: Reference -> Reference -> Typing -> Branch0 -> Branch0
replaceTerm old new _typ b | old == new = b
replaceTerm old new typ b =
 over editedTermsL (R.insert old (TermEdit.Replace new typ))
   . over (namespaceL . terms)    (R.replaceRan old' new')
   . over (oldNamespaceL . terms) (R.union (termNamespace b R.|> [old']))
   $ b
 where
  old' = Referent.Ref old
  new' = Referent.Ref new

-- This resolves term edit conflicts for `old`, restoring it to an unconflicted
-- state such that:
-- 1. There is only one edit of `old` (it's changed to `new`).
-- 2. Every hash that `old` was changed to is now changed to `new`.
resolveTermConflict :: Reference -> TermEdit -> Branch0 -> Branch0
resolveTermConflict old new b = set
  editedTermsL
  (R.insertManyDom (toList . TermEdit.toReference =<< updates) new stripped)
  b
 where
  updates  = Set.toList . R.lookupDom old $ editedTerms b
  stripped = R.insert old new . R.deleteDom old $ editedTerms b

-- Like `resolveTermConflict`, but for types.
resolveTypeConflict :: Reference -> TypeEdit -> Branch0 -> Branch0
resolveTypeConflict old new b = set
  editedTypesL
  (R.insertManyDom (toList . TypeEdit.toReference =<< updates) new stripped)
  b
 where
  updates  = Set.toList . R.lookupDom old $ editedTypes b
  stripped = R.insert old new . R.deleteDom old $ editedTypes b

-- Does both `unreplaceTerm` and `unreplaceType`. This is fine since
-- term and type references are guaranteed to be distinct.
unreplace :: Reference -> Reference -> Branch0 -> Branch0
unreplace r1 r2 = unreplaceTerm r1 r2 . unreplaceType r1 r2

-- `unreplaceTerm old new` removes the term edit `old -> new`
-- from the branch, or noops if there's no such edit.
--
-- To do this doesn't require a scan of the full relation,
-- just a tweak to the range for `old`.
unreplaceTerm :: Reference -> Reference -> Branch0 -> Branch0
unreplaceTerm old new = over editedTermsL $ R.deleteRanWhere p old
 where
  p (TermEdit.Replace r _) = r == new
  p _                      = False

-- `unreplaceType old new` removes the type edit `old -> new`
-- from the branch, or noops if there's no such edit.
-- To do this doesn't require a scan of the full relation,
-- just a tweak to the range for `old`.
unreplaceType :: Reference -> Reference -> Branch0 -> Branch0
unreplaceType old new = over editedTypesL $ R.deleteRanWhere p old
 where
  p (TypeEdit.Replace r) = r == new
  p _                    = False

-- If any `as` aren't in `b`, then delete them from `c` as well.  Kind of sad.
deleteOrphans
  :: (Ord a, Ord c) => Set a -> Relation a b -> Relation a c -> Relation a c
deleteOrphans as b c =
  foldl' (\c a -> if R.memberDom a b then c else R.deleteDom a c) c as

-- Collect all the term/type references mentioned in this branch.
codebase :: Monad m => ReferenceOps m -> Branch -> m (Set Reference)
codebase ops (Branch (Causal.head -> b@Branch0 {..})) =
  let initial = Set.fromList $
        (Referent.toReference . snd <$> R.toList (termNamespace b)) ++
        (snd <$> R.toList (typeNamespace b)) ++
        ((map snd (R.toList $ editedTerms b) >>= TermEdit.referents)) ++
        ((map snd (R.toList $ editedTypes b) >>= TypeEdit.references))
  in transitiveClosure (dependencies ops) initial

deprecateTerm :: Reference -> Branch -> Branch
deprecateTerm old (Branch b) = Branch $ Causal.step go b
 where
  old' = Referent.Ref old
  go b =
    over editedTermsL (R.insert old TermEdit.Deprecate)
      . over (namespaceL . terms)    (R.deleteRan old')
      . over (oldNamespaceL . terms) (R.union $ termNamespace b R.|> [old'])
      $ b

deprecateType :: Reference -> Branch -> Branch
deprecateType old (Branch b) = Branch $ Causal.step go b
 where
  go b =
    over editedTypesL (R.insert old TypeEdit.Deprecate)
      . over (namespaceL . types)    (R.deleteRan old)
      . over (oldNamespaceL . types) (R.union $ typeNamespace b R.|> [old])
      $ b

instance (Hashable a, Hashable b) => Hashable (Relation a b) where
  tokens r = H.tokens (R.toList r)

instance Hashable Branch0 where
  tokens b =
    H.tokens (termNamespace b) ++ H.tokens (typeNamespace b) ++
    H.tokens (editedTerms b) ++ H.tokens (editedTypes b)

resolveTerm :: Name -> Branch -> Set Referent
resolveTerm n (Branch (Causal.head -> b)) = R.lookupDom n (termNamespace b)

resolveTermUniquely :: Name -> Branch -> Maybe Referent
resolveTermUniquely n b =
  case resolveTerm n b of
    s | Set.size s == 1 -> Set.lookupMin s
    _                   -> Nothing

termOrTypeOp :: Monad m => ReferenceOps m -> Reference
             -> m b -> m b -> m b
termOrTypeOp ops r ifTerm ifType = do
  isTerm <- isTerm ops r
  isType <- isType ops r
  if isTerm then ifTerm
  else if isType then ifType
  else fail $ "neither term nor type: " ++ show r

addTermName :: Referent -> Name -> Branch0 -> Branch0
addTermName r new = over (namespaceL . terms) $ R.insert new r

addTypeName :: Reference -> Name -> Branch0 -> Branch0
addTypeName r new = over (namespaceL . types) $ R.insert new r

renameType :: Name -> Name -> Branch0 -> Branch0
renameType old new = over (namespaceL . types) $ R.replaceDom old new

renameTerm :: Name -> Name -> Branch0 -> Branch0
renameTerm old new = over (namespaceL . terms) $ R.replaceDom old new

deleteTermName :: Referent -> Name -> Branch0 -> Branch0
deleteTermName r name = over (namespaceL . terms) $ R.delete name r

deleteTypeName :: Reference -> Name -> Branch0 -> Branch0
deleteTypeName r name = over (namespaceL . types) $ R.delete name r

deleteTermsNamed :: Name -> Branch0 -> Branch0
deleteTermsNamed name = over (namespaceL . terms) $ R.deleteDom name

deleteTypesNamed :: Name -> Branch0 -> Branch0
deleteTypesNamed name = over (namespaceL . types) $ R.deleteDom name

toHash :: Branch -> Hash
toHash = Causal.currentHash . unbranch

toNames :: Branch -> Names
toNames b' = Names terms types
 where
  b        = head b'
  termRefs = Map.fromList . R.toList $ termNamespace b
  types    = Map.fromList . R.toList $ typeNamespace b
  terms    = termRefs
