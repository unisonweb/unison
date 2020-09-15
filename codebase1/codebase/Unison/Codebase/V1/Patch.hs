{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.V1.Patch where

import Control.Lens hiding (children, cons, transform)
import Data.Foldable (Foldable (toList))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified U.Util.Relation as R
import U.Util.Relation (Relation)
import qualified Unison.Codebase.V1.LabeledDependency as LD
import Unison.Codebase.V1.LabeledDependency (LabeledDependency)
import Unison.Codebase.V1.Patch.TermEdit (TermEdit)
import qualified Unison.Codebase.V1.Patch.TermEdit as TermEdit
import Unison.Codebase.V1.Patch.TypeEdit (TypeEdit)
import qualified Unison.Codebase.V1.Patch.TypeEdit as TypeEdit
import Unison.Codebase.V1.Reference (Reference)

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit,
    _typeEdits :: Relation Reference TypeEdit
  }
  deriving (Eq, Ord, Show)

makeLenses ''Patch

labeledDependencies :: Patch -> Set LabeledDependency
labeledDependencies Patch {..} =
  Set.map LD.termRef (R.dom _termEdits)
    <> Set.fromList
      (fmap LD.termRef $ TermEdit.references =<< toList (R.ran _termEdits))
    <> Set.map LD.typeRef (R.dom _typeEdits)
    <> Set.fromList
      (fmap LD.typeRef $ TypeEdit.references =<< toList (R.ran _typeEdits))

allReferences :: Patch -> Set Reference
allReferences p = typeReferences p <> termReferences p
  where
    typeReferences p =
      Set.fromList
        [ r | (old, TypeEdit.Replace new) <- R.toList (_typeEdits p), r <- [old, new]
        ]
    termReferences p =
      Set.fromList
        [ r | (old, TermEdit.Replace new _) <- R.toList (_termEdits p), r <- [old, new]
        ]

-- | Returns the set of references which are the target of an arrow in the patch
allReferenceTargets :: Patch -> Set Reference
allReferenceTargets p = typeReferences p <> termReferences p
  where
    typeReferences p =
      Set.fromList
        [new | (_, TypeEdit.Replace new) <- R.toList (_typeEdits p)]
    termReferences p =
      Set.fromList
        [new | (_, TermEdit.Replace new _) <- R.toList (_termEdits p)]
