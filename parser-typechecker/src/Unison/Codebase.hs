{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase where

import Data.String (fromString)
import Control.Monad (forM)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.List
import qualified Data.Map as Map
import qualified Data.Relation as R
import           Data.Set               (Set)
import qualified Data.Text as Text
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import           Unison.Codebase.Branch (Branch,Branch0(..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.DataDeclaration as DD
import qualified Unison.PrettyPrintEnv  as PPE
import           Unison.Reference       (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Term            as Term
import qualified Unison.TermPrinter     as TermPrinter
import qualified Unison.Type            as Type
import Unison.Util.PrettyPrint (PrettyPrint)
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.AnnotatedText (AnnotatedText)
import           Unison.Util.ColorText     (Color)
import qualified Unison.Var             as Var
import qualified Unison.ABT             as ABT
import Unison.Names (Names(..), Name)

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

prettyBinding :: (Var.Var v, Monad m)
  => Codebase m v a -> Name -> Reference -> Branch -> m (Maybe (PrettyPrint String))
prettyBinding _ _ (Reference.Builtin _) _ = pure Nothing
prettyBinding cb name r0@(Reference.DerivedId r) b = go =<< getTerm cb r where
  go Nothing = pure Nothing
  go (Just tm) = let
    -- We boost the `(r0,name)` association since if this is a recursive
    -- fn whose body also mentions `r`, want name to be the same as the binding.
    ppEnv = Branch.prettyPrintEnv [b] `mappend`
            PPE.scale 10 (PPE.withTermNames [(r0, name)])
    in case tm of
      Term.Ann' _ _ -> pure $ Just (TermPrinter.prettyBinding ppEnv (Var.named name) tm)
      _ -> do
        Just typ <- getTypeOfTerm cb r0
        pure . Just $ TermPrinter.prettyBinding ppEnv
          (Var.named name)
          (Term.ann (ABT.annotation tm) tm typ)
prettyBinding _ _ r _ = error $ "unpossible " ++ show r

prettyBindings :: (Var.Var v, Monad m)
  => Codebase m v a -> [(Name,Reference)] -> Branch -> m (PrettyPrint String)
prettyBindings cb tms b = do
  ds <- catMaybes <$> (forM tms $ \(name,r) -> prettyBinding cb name r b)
  pure $ PP.linesSpaced ds

-- Search for and display bindings matching the given query
prettyBindingsQ :: (Var.Var v, Monad m)
  => Codebase m v a -> String -> Branch -> m (PrettyPrint String)
prettyBindingsQ cb query b = let
  possible = Branch.allTermNames (Branch.head b)
  matches = sortedApproximateMatches query (Text.unpack <$> toList possible)
  str = fromString
  bs = [ (name,r) | name <- Text.pack <$> matches,
                    r <- take 1 (toList $ Branch.termsNamed name b) ]
  go pp = if length matches > 5
          then PP.linesSpaced [pp, "... " <> str (show (length matches - 5)) <>
                               " more (use `> list " <> str query <> "` to see all matches)"]
          else pp
  in go <$> prettyBindings cb (take 5 bs) b

prettyListingQ :: (Var.Var v, Monad m)
  => Codebase m v a -> String -> Branch -> m (AnnotatedText Color)
prettyListingQ _cb _query _b =
  error "todo - find all matches, display similar output to PrintError.prettyTypecheckedFile"

sortedApproximateMatches :: String -> [String] -> [String]
sortedApproximateMatches q possible = sortOn score matches where
  nq = length q
  score s | s == q           = 0 :: Int -- exact match is top choice
          | q `isSuffixOf` s = 1        -- matching suffix is pretty good
          | q `isInfixOf`  s = 2        -- a match somewhere
          | q `isPrefixOf` s = 3        -- ...
          | otherwise        = 3 + editDistance q s
  match s | q `isSubsequenceOf` s = True
          | editDistance q s < ((length s `max` nq) `div` 3) = True -- "pretty close"
          | otherwise = False
  editDistance q s = levenshteinDistance defaultEditCosts q s
  matches = filter match possible

branchExists :: Functor m => Codebase m v a -> Name -> m Bool
branchExists codebase name = elem name <$> branches codebase

branchToNames :: (Ord v, Monad m) => Codebase m v a -> Branch -> m (Names v a)
branchToNames code b = case Branch.head b of
  Branch0 {..} -> do
    let termRefs = Map.fromList $ R.toList termNamespace
        patterns = Map.fromList $ R.toList patternNamespace
        types = Map.fromList $ R.toList typeNamespace
    terms <- fmap Map.fromList . forM (Map.toList termRefs) $ \(name, ref) -> do
      Just typ <- getTypeOfTerm code ref
      pure (name, (Term.ref (ABT.annotation typ) ref, typ))
    pure $ Names terms patterns types
