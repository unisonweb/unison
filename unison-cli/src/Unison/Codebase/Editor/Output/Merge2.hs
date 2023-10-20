module Unison.Codebase.Editor.Output.Merge2 where

import Data.Map qualified as Map
import Unison.ABT (Var)
import Unison.Cli.Pretty qualified as P
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as V1
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Merge2 (Conflict (..), ConflictOrGood (Conflict, Good), MergeOutput, ScratchDefn (SdDecl, SdTerm), definitions)
import Unison.Merge2 qualified as NT
import Unison.Name (Name)
import Unison.Prelude
import Unison.Project (ProjectBranchName)
import Unison.Syntax.NamePrinter qualified as NamePrinter
import Unison.Term (Term)
import Unison.Util.Pretty qualified as P

-----------------------------------------------------------------------------------------------------------------------
-- Pretty-print definitions

type Pretty = P.Pretty P.ColorText

pseudoOutput :: forall v a. (Var v, Ord a) => (Name -> ScratchDefn v a -> Pretty) -> MergeOutput v a -> Pretty
pseudoOutput printDefn merge = prettyConflicts <> newline <> prettyTransitiveDeps
  where
    prettyConflicts :: Pretty =
      foldMap prettyConflict (Map.toList $ fmap (fmap SdDecl) conflictedDecls)
        <> foldMap prettyConflict (Map.toList $ fmap (fmap SdTerm) conflictedTerms)
    prettyTransitiveDeps :: Pretty =
      foldMap (prettyTransitiveDep . second SdDecl) (Map.toList okDecls)
        <> foldMap (prettyTransitiveDep . second SdTerm) (Map.toList okTerms)
    conflictedTerms :: Map Name (Conflict ProjectBranchName (Term v a))
    conflictedDecls :: Map Name (Conflict ProjectBranchName (Decl v a))
    okTerms :: Map Name (Term v a)
    okDecls :: Map Name (Decl v a)
    (conflictedTerms, okTerms) = foldl' (partitionConflicts) mempty (Map.toList . NT.terms $ definitions merge)
    (conflictedDecls, okDecls) = foldl' (partitionConflicts) mempty (Map.toList . NT.types $ definitions merge)
    partitionConflicts ::
      forall a.
      Ord a =>
      (Map Name (Conflict ProjectBranchName a), Map Name a) ->
      (Name, ConflictOrGood a) ->
      (Map Name (Conflict ProjectBranchName a), Map Name a)
    partitionConflicts (conflicted, transitiveDeps) (name, cog) = case cog of
      Conflict conflict -> (Map.insert name conflict conflicted, transitiveDeps)
      Good unconflicted -> (conflicted, Map.insert name unconflicted transitiveDeps)
    prettyTransitiveDep = uncurry printDefn
    prettyConflict :: (Name, Conflict ProjectBranchName (ScratchDefn v a)) -> Pretty
    prettyConflict = \case
      (name, ConflictUnknown b1 b2 d1 d2) ->
        P.lines
          [ "-- from " <> P.prettyProjectBranchName b1,
            printDefn name d1,
            "-- from " <> P.prettyProjectBranchName b2,
            printDefn name d2
          ]
    printDeletedDefn :: Name -> ScratchDefn v a -> Pretty
    printDeletedDefn name = \case
      SdTerm {} -> NamePrinter.prettyName name <> " = " <> "<<<deleted>>>"
      SdDecl (Left (V1.EffectDeclaration ed)) -> case V1.Decl.modifier ed of
        V1.Decl.Structural -> "structural ability " <> NamePrinter.prettyName name <> " where " <> "<<<deleted>>>"
        V1.Decl.Unique {} -> "unique ability " <> NamePrinter.prettyName name <> " where " <> "<<<deleted>>>"
      SdDecl (Right dd) -> case V1.Decl.modifier dd of
        V1.Decl.Structural -> "structural type " <> NamePrinter.prettyName name <> " = " <> "<<<deleted>>>"
        V1.Decl.Unique {} -> "unique type " <> NamePrinter.prettyName name <> " = " <> "<<<deleted>>>"
    newline = "\n"

-----------------------------------------------------------------------------------------------------------------------
