-- | Three-way merge for namespace metadata marks — the @##Builtin.Given@
-- sentinel on terms (see 'Unison.Codebase.Givens') and the
-- @##Builtin.Class@ sentinel on types (see 'Unison.Codebase.Classes').
--
-- The merge logic is identical for both, differing only in the kind of
-- reference marked ('Referent' for givens, 'TypeReference' for classes),
-- so the core ('Marks', 'mergeMarks', 'applyMarks') is polymorphic over
-- the reference type, with given- and class-specialised synonyms.
--
-- During a three-way namespace merge, four distinct cases must be
-- resolved when the marks differ across LCA, Alice, and Bob:
--
--   * (a) Both branches mark the same hash - no-op; the merged
--     namespace keeps the mark.
--   * (b) One branch marks, the other does not - mark wins by default,
--     user-confirmable in interactive mode. Non-interactive merges
--     (CI) default to the mark surviving.
--   * (c) Both branches rename the same definition to different names -
--     the existing rename-conflict path drives name resolution;
--     whichever name the user picks, the resulting hash retains the tag.
--   * (d) One branch marks, the other deletes the definition - the
--     existing delete-conflict path drives the resolution. If the
--     surviving definition exists, it inherits the mark.
--
-- The merge pipeline wires this in via
-- 'Unison.Codebase.Editor.HandleInput.Givens.mergeGivenMarksInto' /
-- 'mergeClassMarksInto', which extract the marks per branch from their
-- 'Branch0', pass them to 'mergeMarks' (in 'NonInteractive' mode), and
-- project the result back onto the merged branch with 'applyMarks'.
-- This is necessary because the merged namespace is assembled from
-- unconflicted definitions, which carry no metadata — so without it a
-- merge would silently drop every @given@ / @class@ mark.
--
-- Cases (c) and (d) require no new conflict category in the merge
-- itself - they layer atop the existing rename-conflict and
-- delete-conflict paths. The single new conflict category introduced
-- here is case (b)'s metadata-only conflict, exposed to callers via
-- 'MarkConflict' for optional confirmation.
module Unison.Merge.GivenSet
  ( -- * Per-branch marks (generic)
    Marks,
    emptyMarks,
    isMarked,
    insertMark,
    fromList,
    toMarkList,

    -- * Merge mode
    MergeMode (..),

    -- * Three-way merge (generic)
    MarksMergeOutcome (..),
    MarkConflict (..),
    MarkSide (..),
    mergeMarks,
    applyMarks,

    -- * Given-mark specialisation
    GivenMarks,
    emptyGivenMarks,
    GivenSetMergeOutcome,
    GivenSetConflict,
    GivenSetSide,
    mergeGivenSets,
    SurvivorMap,
    applyGivenSet,

    -- * Class-mark specialisation
    ClassMarks,
    emptyClassMarks,
    mergeClassSets,
    applyClassSet,

    -- * Sentinel
    sentinel,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Codebase.Givens qualified as Givens
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (TermReference, TypeReference)
import Unison.Referent (Referent)

-- | The givenness sentinel reference, re-exported from
-- 'Unison.Codebase.Givens' so this module is the single import in the
-- merge pipeline. The merge code never compares against a hardcoded
-- string; it always references this definition.
sentinel :: TermReference
sentinel = Givens.givenSentinel

-- | A snapshot of one branch's marks. Keyed by 'Name' since a mark
-- applies at a particular name; the @r@ is the definition being marked
-- ('Referent' for givens, 'TypeReference' for classes). Marking is
-- conceptually per-name, but V1 'MdValues' is keyed on the reference,
-- so the same reference at multiple names is marked at all of them. We
-- carry the reference here so case (c) (rename) and case (d) (delete)
-- can match marks against the merged namespace.
newtype Marks r = Marks (Map Name r)
  deriving stock (Eq, Generic, Show)

emptyMarks :: Marks r
emptyMarks = Marks Map.empty

isMarked :: Name -> Marks r -> Bool
isMarked name (Marks m) = Map.member name m

-- | Insert a mark for the supplied @(name, reference)@.
insertMark :: Name -> r -> Marks r -> Marks r
insertMark name ref (Marks m) = Marks (Map.insert name ref m)

fromList :: [(Name, r)] -> Marks r
fromList = Marks . Map.fromList

toMarkList :: Marks r -> [(Name, r)]
toMarkList (Marks m) = Map.toList m

-- | Whether to prompt the user for case (b) ambiguities. Non-interactive
-- (CI) merges default to keeping the mark; 'NonInteractive' captures
-- that. 'Interactive' mode reports prompts in the outcome so the caller
-- (UCM) can present them.
data MergeMode
  = -- | Surface case (b) ambiguities as prompts; the caller decides.
    Interactive
  | -- | Apply the default ("mark wins") to every case (b) without
    -- prompting. This is the CI default.
    NonInteractive
  deriving stock (Eq, Show)

-- | Which side mark differs from the other in a case (b).
data MarkSide
  = AliceMarked
  | BobMarked
  deriving stock (Eq, Show)

-- | A user-visible conflict where one branch marks a name and the other
-- does not. Surfaced by 'mergeMarks' in 'Interactive' mode; in
-- 'NonInteractive' mode these are auto-resolved and not returned. The
-- 'def' is the reference at the conflict's 'name' on the side that holds
-- the mark.
data MarkConflict r = MarkConflict
  { name :: Name,
    def :: r,
    side :: MarkSide
  }
  deriving stock (Eq, Generic, Show)

-- | The outcome of merging three mark snapshots.
data MarksMergeOutcome r = MarksMergeOutcome
  { -- | Marks that should appear in the merged namespace, before
    -- pruning by 'applyMarks'. In 'NonInteractive' mode this is the
    -- final answer; in 'Interactive' mode it reflects the default
    -- ("mark wins") and is what to apply if the user accepts every
    -- prompt.
    merged :: Marks r,
    -- | Case (b) prompts. Empty in 'NonInteractive' mode.
    prompts :: [MarkConflict r]
  }
  deriving stock (Eq, Generic, Show)

-- | Three-way merge of mark snapshots.
--
-- The four cases are resolved as follows:
--
--   * Case (a) (both mark, same name): the mark survives unchanged.
--   * Case (b) (one marks, one doesn't): the mark survives. In
--     'Interactive' mode a 'MarkConflict' is emitted for caller
--     confirmation; in 'NonInteractive' mode the resolution is silent.
--   * Cases (c) and (d): handled downstream by 'applyMarks', which
--     prunes marks for names that don't survive the rename-conflict /
--     delete-conflict path. Their marks ride along here regardless;
--     'applyMarks' filters.
mergeMarks ::
  forall r.
  MergeMode ->
  ThreeWay (Marks r) ->
  MarksMergeOutcome r
mergeMarks mode ThreeWay {lca, alice, bob} =
  let Marks lcaM = lca
      Marks aliceM = alice
      Marks bobM = bob

      -- Names that appear marked anywhere across the three branches.
      -- Including the LCA's keys ensures we still see names where both
      -- sides removed the mark - those resolve silently to "no mark."
      allMarkedNames :: Set Name
      allMarkedNames =
        Map.keysSet lcaM
          <> Map.keysSet aliceM
          <> Map.keysSet bobM

      step ::
        (Map Name r, [MarkConflict r]) ->
        Name ->
        (Map Name r, [MarkConflict r])
      step (acc, conflicts) name =
        case (Map.lookup name aliceM, Map.lookup name bobM) of
          -- Case (a): both branches still mark this name. The mark
          -- survives unchanged - no prompt. We use
          -- Alice's reference as the canonical witness; if the
          -- underlying definition diverges, the existing update-update
          -- conflict path will surface it separately.
          (Just refA, Just _) ->
            (Map.insert name refA acc, conflicts)
          -- Case (b): only Alice marks. Mark wins by default; in
          -- Interactive mode emit a prompt for caller confirmation.
          -- This fires whether the LCA was marked or not: a merge that
          -- drops a deliberate mark is more surprising than one that
          -- keeps it.
          (Just refA, Nothing) ->
            ( Map.insert name refA acc,
              case mode of
                NonInteractive -> conflicts
                Interactive ->
                  MarkConflict {name, def = refA, side = AliceMarked} : conflicts
            )
          -- Case (b): only Bob marks. Symmetric to the Alice case.
          (Nothing, Just refB) ->
            ( Map.insert name refB acc,
              case mode of
                NonInteractive -> conflicts
                Interactive ->
                  MarkConflict {name, def = refB, side = BobMarked} : conflicts
            )
          -- Neither branch marks. If the LCA had a mark and both
          -- sides explicitly removed it, that's not a conflict - both
          -- agreed - so the result has no mark. (Ditto if the LCA
          -- never had it.)
          (Nothing, Nothing) -> (acc, conflicts)

      (mergedMap, prompts0) =
        foldl' step (Map.empty, []) (Set.toList allMarkedNames)
   in MarksMergeOutcome
        { merged = Marks mergedMap,
          prompts = reverse prompts0
        }

-- | Project a merged mark-set onto the surviving namespace. Any mark
-- whose name no longer exists in the merged namespace is dropped
-- (case (d), delete won). Any mark whose name now refers to a
-- different reference than was originally marked is kept iff that name
-- is the survivor of a rename - in which case the rename-conflict
-- path already resolved which name maps to the marked hash, and the
-- mark travels with the hash (case (c)).
--
-- The semantics: a mark @(n, r)@ survives if either
--
--   * @n@ maps to @r@ in the survivor map (the obvious case), or
--   * the survivor map contains @r@ under any name (the rename case),
--     in which case we relocate the mark to @r@'s new name.
applyMarks :: forall r. (Ord r) => Map Name r -> Marks r -> Marks r
applyMarks survivors (Marks marks) =
  let survivorByRef :: Map r Name
      survivorByRef =
        Map.foldlWithKey' (\acc n r -> Map.insert r n acc) Map.empty survivors

      step :: Map Name r -> Name -> r -> Map Name r
      step acc n r =
        case Map.lookup n survivors of
          Just r' | r' == r -> Map.insert n r acc
          _ ->
            -- Either the name was deleted (Map.lookup returned Nothing)
            -- or the name now refers to a different reference (rename
            -- handled below). In the rename case, find the new home
            -- of @r@ in the survivor map.
            case Map.lookup r survivorByRef of
              Just n' -> Map.insert n' r acc
              Nothing -> acc -- definitively deleted
   in Marks (Map.foldlWithKey' step Map.empty marks)

------------------------------------------------------------------------------
-- Given-mark specialisation
------------------------------------------------------------------------------

-- | One branch's @given@-marks: a 'Marks' over term 'Referent's.
type GivenMarks = Marks Referent

emptyGivenMarks :: GivenMarks
emptyGivenMarks = emptyMarks

type GivenSetMergeOutcome = MarksMergeOutcome Referent

type GivenSetConflict = MarkConflict Referent

type GivenSetSide = MarkSide

-- | 'mergeMarks' specialised to @given@-marks.
mergeGivenSets :: MergeMode -> ThreeWay GivenMarks -> GivenSetMergeOutcome
mergeGivenSets = mergeMarks

-- | A map describing which @(name, referent)@ pairs survive in the
-- merged namespace, used by 'applyGivenSet'.
type SurvivorMap = Map Name Referent

-- | 'applyMarks' specialised to @given@-marks.
applyGivenSet :: SurvivorMap -> GivenMarks -> GivenMarks
applyGivenSet = applyMarks

------------------------------------------------------------------------------
-- Class-mark specialisation
------------------------------------------------------------------------------

-- | One branch's @class@-marks: a 'Marks' over 'TypeReference's.
type ClassMarks = Marks TypeReference

emptyClassMarks :: ClassMarks
emptyClassMarks = emptyMarks

-- | 'mergeMarks' specialised to @class@-marks.
mergeClassSets :: MergeMode -> ThreeWay ClassMarks -> MarksMergeOutcome TypeReference
mergeClassSets = mergeMarks

-- | 'applyMarks' specialised to @class@-marks.
applyClassSet :: Map Name TypeReference -> ClassMarks -> ClassMarks
applyClassSet = applyMarks
