{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase where

import           Data.Text              (Text)
import           Unison.Codebase.Branch (Branch)
import           Unison.Codebase.Name   (Name)
import qualified Unison.DataDeclaration as DD
import           Unison.Hash            (Hash)
import           Unison.Reference       (Reference)
import           Unison.Result          (Note, Result)
import qualified Unison.Term            as Term
import qualified Unison.Type            as Type
import           Unison.UnisonFile      (UnisonFile')

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data Codebase m v a =
  Codebase { getTerm            :: Hash -> m (Maybe (Term v a))
           , getTypeOfTerm      :: Reference -> m (Maybe (Type v a))
           , putTerm            :: Hash -> Term v a -> Type v a -> m ()

           , getTypeDeclaration :: Hash -> m (Maybe (Decl v a))
           , putTypeDeclaration :: Hash -> Decl v a -> m ()

           , branches           :: m [Name]
           , getBranch          :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch        :: Name -> Branch -> m Branch
           -- , branchUpdates :: m (m (), m (Name, Branch))
           }

data Session m v a
  = Session { branch :: m Name
            , switchBranch :: Name -> m ()
            -- Await new .ubf files
            , watchBranches :: FilePath -> m (FilePath, Branch)
            -- Await the next .u file change in the given directory,
            -- and return the path of the thing that changed, its contents,
            -- and the results of parsing / typechecking.
            , watch :: FilePath -> m (FilePath, Text, Result (Note v a) (UnisonFile' v a)) }

data Err = InvalidBranchFile FilePath String deriving Show

branchExists :: Functor m => Codebase m v a -> Name -> m Bool
branchExists codebase name = elem name <$> branches codebase
