-- | Three-way merge for namespace given-sets.
--
-- A /given/ is a name marked with the @##Builtin.Given@ sentinel
-- reference inside the namespace's 'MdValues' (see 'Unison.Codebase.Givens').
-- During a three-way namespace merge, four distinct cases must be
-- resolved when the given marks differ across LCA, Alice, and Bob:
--
--   * (a) Both branches mark the same hash given - no-op; the merged
--     namespace keeps the mark.
--   * (b) One branch marks, the other does not - mark wins by default,
--     user-confirmable in interactive mode. Non-interactive merges
--     (CI) default to the mark surviving.
--   * (c) Both branches rename the same given to different names - the
--     existing rename-conflict path drives name resolution; whichever
--     name the user picks, the resulting hash retains the given tag.
--   * (d) One branch marks, the other deletes the definition - the
--     existing delete-conflict path drives the resolution. If the
--     surviving definition exists, it inherits the mark.
--
-- The merge pipeline wires this in via
-- 'Unison.Codebase.Editor.HandleInput.Givens.mergeGivenMarksInto', which
-- extracts the given-marks per branch from their 'Branch0' (using
-- 'Unison.Codebase.Givens'), passes them to 'mergeGivenSets' (in
-- 'NonInteractive' mode) alongside the existing merge inputs, and
-- projects the result back onto the merged branch with 'applyGivenSet'.
-- This is necessary because the merged namespace is assembled from
-- unconflicted definitions, which carry no metadata — so without it a
-- merge would silently drop every @given@ mark.
--
-- Cases (c) and (d) require no new conflict category in the merge
-- itself - they layer atop the existing rename-conflict and
-- delete-conflict paths. The single new conflict category introduced
-- here is case (b)'s metadata-only conflict, exposed to callers via
-- 'GivenSetConflict' for optional confirmation.
module Unison.Merge.GivenSet
  ( -- * Per-branch given-marks
    GivenMarks,
    emptyGivenMarks,
    isMarked,
    insertMark,
    fromList,
    toMarkList,

    -- * Merge mode
    MergeMode (..),

    -- * Three-way merge
    GivenSetMergeOutcome (..),
    GivenSetConflict (..),
    GivenSetSide (..),
    mergeGivenSets,

    -- * Applying the merge result
    SurvivorMap,
    applyGivenSet,

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
import Unison.Reference (TermReference)
import Unison.Referent (Referent)

-- | The givenness sentinel reference, re-exported from
-- 'Unison.Codebase.Givens' so this module is the single import in the
-- merge pipeline. The merge code never compares against a hardcoded
-- string; it always references this definition.
sentinel :: TermReference
sentinel = Givens.givenSentinel

-- | A snapshot of one branch's given-marks. Keyed by 'Name' since a
-- mark applies at a particular name; the 'Referent' is the term being
-- marked. Marking is conceptually per-name, but V1 'MdValues' is keyed
-- on referent, so the same referent at multiple names is marked at all
-- of them. We carry the 'Referent' here so case (c) (rename) and case
-- (d) (delete) can match marks against the merged namespace.
newtype GivenMarks = GivenMarks (Map Name Referent)
  deriving stock (Eq, Generic, Show)

emptyGivenMarks :: GivenMarks
emptyGivenMarks = GivenMarks Map.empty

isMarked :: Name -> GivenMarks -> Bool
isMarked name (GivenMarks m) = Map.member name m

-- | Insert a given-mark for the supplied @(name, referent)@.
insertMark :: Name -> Referent -> GivenMarks -> GivenMarks
insertMark name ref (GivenMarks m) = GivenMarks (Map.insert name ref m)

fromList :: [(Name, Referent)] -> GivenMarks
fromList = GivenMarks . Map.fromList

toMarkList :: GivenMarks -> [(Name, Referent)]
toMarkList (GivenMarks m) = Map.toList m

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
data GivenSetSide
  = AliceMarked
  | BobMarked
  deriving stock (Eq, Show)

-- | A user-visible conflict where one branch marks a name as a given
-- and the other does not. Surfaced by 'mergeGivenSets' in 'Interactive'
-- mode; in 'NonInteractive' mode these are auto-resolved and not
-- returned. The 'def' is the referent at the conflict's 'name' on the
-- side that holds the mark.
data GivenSetConflict = GivenSetConflict
  { name :: Name,
    def :: Referent,
    side :: GivenSetSide
  }
  deriving stock (Eq, Generic, Show)

-- | The outcome of merging three given-set snapshots.
data GivenSetMergeOutcome = GivenSetMergeOutcome
  { -- | Marks that should appear in the merged namespace, before
    -- pruning by 'applyGivenSet'. In 'NonInteractive' mode this is the
    -- final answer; in 'Interactive' mode it reflects the default
    -- ("mark wins") and is what to apply if the user accepts every
    -- prompt.
    merged :: GivenMarks,
    -- | Case (b) prompts. Empty in 'NonInteractive' mode.
    prompts :: [GivenSetConflict]
  }
  deriving stock (Eq, Generic, Show)

-- | Three-way merge of given-mark snapshots.
--
-- The four cases are resolved as follows:
--
--   * Case (a) (both mark, same name): the mark survives unchanged.
--   * Case (b) (one marks, one doesn't): the mark survives. In
--     'Interactive' mode a 'GivenSetConflict' is emitted for caller
--     confirmation; in 'NonInteractive' mode the resolution is silent.
--   * Cases (c) and (d): handled downstream by 'applyGivenSet', which
--     prunes marks for names that don't survive the rename-conflict /
--     delete-conflict path. Their marks ride along here regardless;
--     'applyGivenSet' filters.
mergeGivenSets ::
  MergeMode ->
  ThreeWay GivenMarks ->
  GivenSetMergeOutcome
mergeGivenSets mode ThreeWay {lca, alice, bob} =
  let GivenMarks lcaM = lca
      GivenMarks aliceM = alice
      GivenMarks bobM = bob

      -- Names that appear marked anywhere across the three branches.
      -- Including the LCA's keys ensures we still see names where both
      -- sides removed the mark - those resolve silently to "no mark."
      allMarkedNames :: Set Name
      allMarkedNames =
        Map.keysSet lcaM
          <> Map.keysSet aliceM
          <> Map.keysSet bobM

      step ::
        (Map Name Referent, [GivenSetConflict]) ->
        Name ->
        (Map Name Referent, [GivenSetConflict])
      step (acc, conflicts) name =
        case (Map.lookup name aliceM, Map.lookup name bobM) of
          -- Case (a): both branches still mark this name. The mark
          -- survives unchanged - no prompt. We use
          -- Alice's referent as the canonical witness; if the
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
                  GivenSetConflict {name, def = refA, side = AliceMarked} : conflicts
            )
          -- Case (b): only Bob marks. Symmetric to the Alice case.
          (Nothing, Just refB) ->
            ( Map.insert name refB acc,
              case mode of
                NonInteractive -> conflicts
                Interactive ->
                  GivenSetConflict {name, def = refB, side = BobMarked} : conflicts
            )
          -- Neither branch marks. If the LCA had a mark and both
          -- sides explicitly removed it, that's not a conflict - both
          -- agreed - so the result has no mark. (Ditto if the LCA
          -- never had it.)
          (Nothing, Nothing) -> (acc, conflicts)

      (mergedMap, prompts0) =
        foldl' step (Map.empty, []) (Set.toList allMarkedNames)
   in GivenSetMergeOutcome
        { merged = GivenMarks mergedMap,
          prompts = reverse prompts0
        }

-- | A map describing which @(name, referent)@ pairs survive in the
-- merged namespace, used by 'applyGivenSet' to prune marks for names
-- that lost the rename-conflict or delete-conflict path.
type SurvivorMap = Map Name Referent

-- | Project a merged given-set onto the surviving namespace. Any mark
-- whose name no longer exists in the merged namespace is dropped
-- (case (d), delete won). Any mark whose name now refers to a
-- different referent than was originally marked is kept iff that name
-- is the survivor of a rename - in which case the rename-conflict
-- path already resolved which name maps to the marked hash, and the
-- mark travels with the hash (case (c)).
--
-- The semantics: a mark @(n, r)@ survives if either
--
--   * @n@ maps to @r@ in the survivor map (the obvious case), or
--   * the survivor map contains @r@ under any name (the rename case),
--     in which case we relocate the mark to @r@'s new name.
applyGivenSet :: SurvivorMap -> GivenMarks -> GivenMarks
applyGivenSet survivors (GivenMarks marks) =
  let survivorByRef :: Map Referent Name
      survivorByRef =
        Map.foldlWithKey' (\acc n r -> Map.insert r n acc) Map.empty survivors

      step :: Map Name Referent -> Name -> Referent -> Map Name Referent
      step acc n r =
        case Map.lookup n survivors of
          Just r' | r' == r -> Map.insert n r acc
          _ ->
            -- Either the name was deleted (Map.lookup returned Nothing)
            -- or the name now refers to a different referent (rename
            -- handled below). In the rename case, find the new home
            -- of @r@ in the survivor map.
            case Map.lookup r survivorByRef of
              Just n' -> Map.insert n' r acc
              Nothing -> acc -- definitively deleted
   in GivenMarks (Map.foldlWithKey' step Map.empty marks)
