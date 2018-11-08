{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Unison.Codebase where

import           Data.String                    ( fromString )
import           Control.Monad                  ( forM )
import           Data.Foldable                  ( toList, traverse_ )
import           Data.Maybe                     ( catMaybes )
import           Data.List
import Data.Map (Map)
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Text                     as Text
import           Text.EditDistance              ( defaultEditCosts
                                                , levenshteinDistance
                                                )
import qualified Unison.Builtin                as Builtin
import           Unison.Codebase.Branch         ( Branch
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.DataDeclaration        as DD
import           Unison.Parser                  ( Ann )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Term                   as Term
import qualified Unison.TermPrinter            as TermPrinter
import qualified Unison.Type                   as Type
import           Unison.Util.PrettyPrint        ( PrettyPrint )
import qualified Unison.Util.PrettyPrint       as PP
import           Unison.Util.AnnotatedText      ( AnnotatedText )
import           Unison.Util.ColorText          ( Color )
import qualified Unison.Util.Relation          as R
import qualified Unison.Var                    as Var
import qualified Unison.ABT                    as ABT
import           Unison.Names                   ( Names(..)
                                                , Name
                                                )

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
           , putTypeDeclarationImpl :: Reference.Id -> Decl v a -> m ()
           , branches           :: m [Name]
           , getBranch          :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch        :: Name -> Branch -> m Branch
           , branchUpdates      :: m (m (), m (Set Name))
           }

data ReadRefs v a =
  ReadRefs { typeOfTerm :: Map Reference (Type v a)
           , typeDeclaration :: Map Reference.Id (Decl v a) }

-- Scan the term for all its dependencies and pull out the `ReadRefs` that
-- gives info for all its dependencies, using the provided codebase.
typecheckingEnvironment :: (Monad m, Ord v) => Codebase m v a -> Term v a -> m (ReadRefs v a)
typecheckingEnvironment code t = do
  let deps = Term.dependencies t
  termTypes0 <- forM (toList deps) $ \r -> (r,) <$> getTypeOfTerm code r
  let termTypes = Map.fromList [ (r, t) | (r, Just t) <- termTypes0 ]
  let rids = [ r | Reference.DerivedId r <- toList deps ]
  decls0 <- forM rids $ \r -> (r,) <$> getTypeDeclaration code r
  let decls = Map.fromList [ (r, d) | (r, Just d) <- decls0 ]
  pure $ ReadRefs termTypes decls

data Err = InvalidBranchFile FilePath String deriving Show

putTypeDeclaration
  :: (Monad m, Ord v) => Codebase m v a -> Reference.Id -> Decl v a -> m ()
putTypeDeclaration c rid decl = do
  putTypeDeclarationImpl c rid decl
  traverse_ go $ case decl of
    Left  ed -> DD.effectConstructorTerms rid ed
    Right dd -> DD.dataConstructorTerms rid dd
  where go (r, tm, typ) = putTerm c r tm typ

-- | Put all the builtins into the codebase
initialize :: (Var.Var v, Monad m) => Codebase m v Ann -> m ()
initialize c = do
  traverse_ goData   Builtin.builtinDataDecls
  traverse_ goEffect Builtin.builtinEffectDecls
 where
  go f (_, (ref, decl)) = case ref of
    Reference.DerivedId id -> putTypeDeclaration c id (f decl)
    _                      -> pure ()
  goEffect = go Left
  goData   = go Right

prettyBinding :: (Var.Var v, Monad m)
  => Codebase m v a -> Name -> Reference -> Branch -> m (Maybe (PrettyPrint String))
prettyBinding _ _ (Reference.Builtin _) _ = pure Nothing
prettyBinding cb name r0@(Reference.DerivedId r) b = go =<< getTerm cb r where
  go Nothing = pure Nothing
  go (Just tm) = let
    -- We boost the `(r0,name)` association since if this is a recursive
    -- fn whose body also mentions `r`, want name to be the same as the binding.
    ppEnv = Branch.prettyPrintEnv [b] `mappend`
            PPE.scale 10 (PPE.fromTermNames [(r0, name)])
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

