module Unison.Merge.PreconditionViolation
  ( PreconditionViolation (..),
  )
where

import U.Codebase.Reference (TypeReference)
import U.Codebase.Referent (Referent)
import Unison.Core.Project (ProjectBranchName)
import Unison.Name (Name)
import Unison.Prelude

-- | A reason that a merge could not be performed.
data PreconditionViolation
  = -- | @ConflictedAliases branch foo bar@: in project branch @branch@, @foo@ and @bar@ refer to different things,
    -- but at one time (in the LCA of another branch, in fact) they referred to the same thing.
    ConflictedAliases !ProjectBranchName !Name !Name
  | -- | @ConflictedTermName name refs@: @name@ refers to 2+ referents @refs@.
    ConflictedTermName !Name !(Set Referent)
  | -- | @ConflictedTypeName name refs@: @name@ refers to 2+ type references @refs@.
    ConflictedTypeName !Name !(Set TypeReference)
  | -- | @ConflictInvolvingBuiltin name@: @name@ is involved in a conflict, but it refers to a builtin (on at least one
    -- side). Since we can't put a builtin in a scratch file, we bomb in these cases.
    ConflictInvolvingBuiltin !Name
  | -- | A second naming of a constructor was discovered underneath a decl's name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo#0
    --   Foo.Some.Other.Name.For.Bar#Foo#0
    ConstructorAlias !Name !Name -- first name we found, second name we found
  | -- | There were some definitions at the top level of lib.*, which we don't like
    DefnsInLib
  | MissingConstructorName !Name
  | NestedDeclAlias !Name
  | NoConstructorNames !Name
  | StrayConstructor !Name
  deriving stock (Show)
