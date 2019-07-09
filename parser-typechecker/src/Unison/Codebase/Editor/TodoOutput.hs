{-# LANGUAGE RecordWildCards #-}
module Unison.Codebase.Editor.TodoOutput where

import qualified Unison.Names3 as Names
import qualified Unison.Referent as Referent
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
import Unison.Type (AnnotatedType)
import Unison.DataDeclaration (Decl)
import Data.Foldable (toList)
import Unison.Referent (Referent)
import Data.Set (Set)

type Score = Int
type Type v a = AnnotatedType v a

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

labeledDependencies :: Ord v => TodoOutput v a -> Set (Either Reference Referent)
labeledDependencies TodoOutput{..} = Set.fromList (
  -- term refs
  [tmRef r | (_, r, _) <- fst todoFrontier] <>
  [tmRef r | (_, _, r, _) <- fst todoFrontierDependents] <>
  [tyRef r | (_, r, _) <- snd todoFrontier] <>
  [tyRef r | (_, _, r, _) <- snd todoFrontierDependents] <>
  -- types of term refs
  [tyRef r | (_, _, Just t) <- fst todoFrontier
            , r <- toList (Type.dependencies t)] <>
  [tyRef r | (_, _, _, Just t) <- fst todoFrontierDependents
            , r <- toList (Type.dependencies t)] <>
  -- and decls of type refs
  [tyRef r | (_, _, RegularThing d) <- snd todoFrontier
           , r <- toList (DD.declDependencies d)] <>
  [tyRef r | (_, _, _, RegularThing d) <- snd todoFrontierDependents
           , r <- toList (DD.declDependencies d)]) <>
  -- name conflicts
  Set.map tmRef' (R.ran (Names.terms0 nameConflicts)) <>
  Set.map tyRef (R.ran (Names.types0 nameConflicts)) <>
  Patch.labeledDependencies editConflicts
  where
  tmRef = Right . Referent.Ref
  tmRef' = Right
  tyRef = Left
