{-# LANGUAGE RecordWildCards #-}
module Unison.Codebase.Editor.TodoOutput where

import qualified Unison.Names3 as Names
import qualified Unison.Type as Type
import qualified Unison.Util.Relation as R
import qualified Unison.Codebase.Patch as Patch
import qualified Data.Set as Set
import qualified Unison.DataDeclaration as DD
import qualified Unison.HashQualified' as HQ'
import Unison.Reference (Reference)
import Unison.Names3 (Names0)
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Editor.DisplayThing (DisplayThing(RegularThing))
import Unison.Type (Type)
import Unison.DataDeclaration (Decl)
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Unison.LabeledDependency as LD
import Unison.LabeledDependency (LabeledDependency)

type Score = Int

data TodoOutput v a = TodoOutput
  { todoScore :: Score
  , todoFrontier ::
        ( [(HQ'.HashQualified, Reference, Maybe (Type v a))]
        , [(HQ'.HashQualified, Reference, DisplayThing (Decl v a))])
  , todoFrontierDependents ::
        ( [(Score, HQ'.HashQualified, Reference, Maybe (Type v a))]
        , [(Score, HQ'.HashQualified, Reference, DisplayThing (Decl v a))])
  , nameConflicts :: Names0
  , editConflicts :: Patch
  } deriving (Show)

labeledDependencies :: Ord v => TodoOutput v a -> Set LabeledDependency
labeledDependencies TodoOutput{..} = Set.fromList (
  -- term refs
  [LD.termRef r | (_, r, _) <- fst todoFrontier] <>
  [LD.termRef r | (_, _, r, _) <- fst todoFrontierDependents] <>
  [LD.typeRef r | (_, r, _) <- snd todoFrontier] <>
  [LD.typeRef r | (_, _, r, _) <- snd todoFrontierDependents] <>
  -- types of term refs
  [LD.typeRef r | (_, _, Just t) <- fst todoFrontier
            , r <- toList (Type.dependencies t)] <>
  [LD.typeRef r | (_, _, _, Just t) <- fst todoFrontierDependents
            , r <- toList (Type.dependencies t)] <>
  -- and decls of type refs
  [LD.typeRef r | (_, _, RegularThing d) <- snd todoFrontier
           , r <- toList (DD.declDependencies d)] <>
  [LD.typeRef r | (_, _, _, RegularThing d) <- snd todoFrontierDependents
           , r <- toList (DD.declDependencies d)]) <>
  -- name conflicts
  Set.map LD.referent (R.ran (Names.terms0 nameConflicts)) <>
  Set.map LD.typeRef (R.ran (Names.types0 nameConflicts)) <>
  Patch.labeledDependencies editConflicts
