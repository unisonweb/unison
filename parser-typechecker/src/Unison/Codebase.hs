{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase where

import           Data.Set               (Set)
import           Unison.Codebase.Branch (Branch)
import           Unison.Codebase.Name   (Name)
import qualified Unison.DataDeclaration as DD
import           Unison.Reference       (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Term            as Term
import qualified Unison.Type            as Type

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data Codebase m v a =
  Codebase { getTerm            :: Reference.Id -> m (Maybe (Term v a))
           , getTypeOfTerm      :: Reference -> m (Maybe (Type v a))
           , putTerm            :: Reference.Id -> Term v a -> Type v a -> m ()

           , getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))
           , putTypeDeclaration :: Reference.Id -> Decl v a -> m ()

           , branches           :: m [Name]
           , getBranch          :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch        :: Name -> Branch -> m Branch
           , branchUpdates      :: m (m (), m (Set Name))
           }

data Err = InvalidBranchFile FilePath String deriving Show

branchExists :: Functor m => Codebase m v a -> Name -> m Bool
branchExists codebase name = elem name <$> branches codebase

