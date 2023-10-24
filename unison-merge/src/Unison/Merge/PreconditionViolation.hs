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
  = ConflictedAliases !ProjectBranchName !Name !Name
  | -- | A name refers to two different terms
    ConflictedTermName !(Set Referent)
  | -- A name refers to two different terms
    ConflictedTypeName !(Set TypeReference)
  | -- We can't put a builtin in a scratch file, so we bomb in situations where we'd have to
    ConflictInvolvingBuiltin
  | -- A second naming of a constructor was discovered underneath a decl's name, e.g.
    --
    --   Foo#Foo
    --   Foo.Bar#Foo#0
    --   Foo.Some.Other.Name.For.Bar#Foo#0
    ConstructorAlias !Name !Name -- first name we found, second name we found
  | -- There were some definitions at the top level of lib.*, which we don't like
    DefnsInLib
  | MissingConstructorName !Name
  | NestedDeclAlias !Name
  | NoConstructorNames !Name
  | StrayConstructor !Name
  deriving stock (Show)
