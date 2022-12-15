-- | Description: This module exports:
--
--   * Data types with 'ContentAddressable' instances that correspond to v2 of the Unison hash function.
--   * Miscellaneous helper functions related to hashing.
module Unison.Hashing.V2
  ( Branch (..),
    Causal (..),
    DataDeclaration (..),
    Decl,
    EffectDeclaration (..),
    Kind (..),
    MatchCase (..),
    MdValues (..),
    Modifier (..),
    NameSegment (..),
    Patch (..),
    Pattern (..),
    Reference (..),
    pattern ReferenceDerived,
    ReferenceId (..),
    Referent (..),
    SeqOp (..),
    Term,
    TermEdit (..),
    TermF (..),
    Type,
    TypeEdit (..),
    TypeF (..),
    hashClosedTerm,
    hashDecls,
    hashTermComponents,
    hashTermComponentsWithoutTypes,
    typeToReference,
    typeToReferenceMentions,
  )
where

import Unison.Hashing.V2.Branch (Branch (..), MdValues (..))
import Unison.Hashing.V2.Causal (Causal (..))
import Unison.Hashing.V2.DataDeclaration (DataDeclaration (..), Decl, EffectDeclaration (..), Modifier (..), hashDecls)
import Unison.Hashing.V2.Kind (Kind (..))
import Unison.Hashing.V2.NameSegment (NameSegment (..))
import Unison.Hashing.V2.Patch (Patch (..))
import Unison.Hashing.V2.Pattern (Pattern (..), SeqOp (..))
import Unison.Hashing.V2.Reference (Reference (..), ReferenceId (..), pattern ReferenceDerived)
import Unison.Hashing.V2.Referent (Referent (..))
import Unison.Hashing.V2.Term (MatchCase (..), Term, TermF (..), hashClosedTerm, hashTermComponents, hashTermComponentsWithoutTypes)
import Unison.Hashing.V2.TermEdit (TermEdit (..))
import Unison.Hashing.V2.Type (Type, TypeF (..), typeToReference, typeToReferenceMentions)
import Unison.Hashing.V2.TypeEdit (TypeEdit (..))
