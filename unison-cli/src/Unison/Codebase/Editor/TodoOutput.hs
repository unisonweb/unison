{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.TodoOutput where

import qualified Data.Set as Set
import Unison.Codebase.Editor.DisplayObject (DisplayObject (UserObject))
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Patch as Patch
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Relation as R

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

labeledDependencies :: Ord v => TodoOutput v a -> Set LabeledDependency
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
        [ LD.typeRef r | (_, UserObject d) <- snd todoFrontier, r <- toList (DD.declDependencies d)
        ]
        <> [ LD.typeRef r | (_, _, UserObject d) <- snd todoFrontierDependents, r <- toList (DD.declDependencies d)
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
