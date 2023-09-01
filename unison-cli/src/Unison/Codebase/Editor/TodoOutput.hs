{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.TodoOutput where

import Data.Set qualified as Set
import Unison.Codebase.Editor.DisplayObject (DisplayObject (UserObject))
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Patch qualified as Patch
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (Reference (DerivedId))
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.Relation qualified as R

type Score = Int

data TodoOutput v a = TodoOutput
  { todoScore :: Score,
    todoFrontier ::
      ( [(Reference, Maybe (Type v a))],
        [(Reference, DisplayObject () (Decl v a))]
      ),
    todoFrontierDependents ::
      ( [(Score, Reference, Maybe (Type v a))],
        [(Score, Reference, DisplayObject () (Decl v a))]
      ),
    nameConflicts :: Names,
    editConflicts :: Patch
  }

labeledDependencies :: (Ord v) => TodoOutput v a -> Set LabeledDependency
labeledDependencies TodoOutput {..} =
  Set.fromList
    ( -- term refs
      [LD.termRef r | (r, _) <- fst todoFrontier]
        <> [LD.termRef r | (_, r, _) <- fst todoFrontierDependents]
        <> [LD.typeRef r | (r, _) <- snd todoFrontier]
        <> [LD.typeRef r | (_, r, _) <- snd todoFrontierDependents]
        <>
        -- types of term refs
        [ LD.typeRef r | (_, Just t) <- fst todoFrontier, r <- toList (Type.dependencies t)
        ]
        <> [ LD.typeRef r | (_, _, Just t) <- fst todoFrontierDependents, r <- toList (Type.dependencies t)
           ]
        <>
        -- and decls of type refs
        [ labeledDep | (DerivedId declRef, UserObject d) <- snd todoFrontier, labeledDep <- toList (DD.labeledDeclDependenciesIncludingSelf declRef d)
        ]
        <> [ labeledDep | (_, DerivedId declRef, UserObject d) <- snd todoFrontierDependents, labeledDep <- toList (DD.labeledDeclDependenciesIncludingSelf declRef d)
           ]
    )
    <>
    -- name conflicts
    Set.map LD.referent (R.ran (Names.terms nameConflicts))
    <> Set.map LD.typeRef (R.ran (Names.types nameConflicts))
    <> Patch.labeledDependencies editConflicts

noConflicts :: TodoOutput v a -> Bool
noConflicts todo =
  nameConflicts todo == mempty && editConflicts todo == Patch.empty

noEdits :: TodoOutput v a -> Bool
noEdits todo =
  todoScore todo == 0
