{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Unison.Codebase.Editor.Slurp where

import Unison.Hash (Hash)
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import Unison.Var (Var)

data SlurpStatus = New | Updated | Duplicate | Alias

data SlurpOp = Add | Update

data SlurpErr
  = TermCtorCollision
  | CtorTermCollision
  | RequiresUpdate

data SlurpComponent v = SlurpComponent {types :: Set v, terms :: Set v}
  deriving (Eq, Ord, Show)

data DefinitionNotes v = DefinitionNotes
  { status :: SlurpStatus,
    errs :: Set SlurpErr
  }

data ComponentNotes v = ComponentNotes
  { deps :: Set ComponentHash,
    definitions :: Map v (DefinitionNotes v)
  }

data SlurpResult v = SlurpResult
  { componentNotes :: Map ComponentHash (Map v (DefinitionNotes v)),
    varToComponent :: Map v ComponentHash
  }

type ComponentHash = Hash

data Components v = Components
  { termComponents :: Map Hash (Set v),
    typeComponents :: Map Hash (Set v)
  }

collectComponents :: UF.TypecheckedUnisonFile v Ann -> Components v
collectComponents _uf = Components {termComponents, typeComponents}
  where
    termComponents = undefined
    typeComponents = undefined

computeComponentDependencies :: Components v -> Map Hash (Set Hash)
computeComponentDependencies = undefined

analyzeTypecheckedUnisonFile ::
  forall v.
  Var v =>
  UF.TypecheckedUnisonFile v Ann ->
  Names ->
  SlurpResult v
analyzeTypecheckedUnisonFile uf _codebaseNames = undefined
  where
    fileNames :: Names
    fileNames = UF.typecheckedToNames uf

slurpOp :: SlurpOp -> Maybe (Set v) -> SlurpResult v -> Either (Set SlurpErr) (SlurpComponent v)
slurpOp = undefined
