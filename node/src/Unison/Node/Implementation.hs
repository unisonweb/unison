{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Unison.Node.Implementation (node) where

import Control.Monad
import Data.Bytes.Serial (Serial)
import Data.List
import Data.Ord
import Unison.Eval as Eval
import Unison.Node (Node(..))
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)
import Unison.Node.Store (Store)
import Unison.Note (Noted)
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT.Extra as ABT'
import qualified Unison.Node as Node
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TermEdit as TermEdit
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Hash as Hash
import qualified Unison.Metadata as Metadata
import qualified Unison.Node.Store as Store

-- debugging stuff
--import Debug.Trace
--import qualified Data.Foldable as Foldable
--import Data.Foldable (Foldable)
--
--watch :: Show a => String -> a -> a
--watch msg a = trace (msg ++ ": " ++ show a) a
--
--watches :: (Foldable f, Show a) => String -> f a -> f a
--watches msg as = trace (msg ++ ":\n" ++ intercalate "\n" (map show (Foldable.toList as)) ++ "\n.") as

node :: (Monad f, Var v, Serial v)
     => Eval (Noted f) v
     -> Store f v
     -> Node f v Reference.Reference (Type v) (Term v)
node eval store =
  let
    readTypeOf = Store.typeOfTerm store

    admissibleTypeAt e loc =
      Typechecker.admissibleTypeAt readTypeOf loc e

    createTerm e md = do
      t <- Typechecker.synthesize readTypeOf e
      let h = Hash.fromBytes (ABT'.hash e)
      Store.writeTerm store h e
      Store.writeMetadata store (Reference.Derived h) md
      Store.annotateTerm store (Reference.Derived h) t
      pure (Reference.Derived h)

    createType _ _ = error "todo - createType"

    dependencies _ (Reference.Builtin _) = pure Set.empty
    dependencies limit (Reference.Derived h) = let trim = maybe id Set.intersection limit in do
      e <- Store.readTerm store h
      pure $ trim (Set.map Reference.Derived (Term.dependencies e))

    dependents limit h = do
      hs <- Store.hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (Set.toList hs)
      pure $ Set.fromList [x | (x,deps) <- hs', Set.member h deps]

    edit rootPath path action e = case Term.at rootPath e of
      Nothing -> pure Nothing
      Just e -> f <$> TermEdit.interpret eval (Store.readTerm store) path action e
        where f Nothing = Nothing
              f (Just (newPath,e')) = Just (rootPath,e,e',newPath)

    evaluateTerms es = traverse go es
      where go (path,e) = (\e' -> (path,e,e')) <$> Eval.whnf eval (Store.readTerm store) e

    metadatas hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> Store.readMetadata store h) hs)

    localInfo e loc = do
      current <- Typechecker.typeAt readTypeOf loc e
      admissible <- Typechecker.admissibleTypeAt readTypeOf loc e
      locals <- Typechecker.locals readTypeOf loc e
      annotatedLocals <- pure $ map (\(v,t) -> Term.var v `Term.ann` t) locals
      let f focus = maybe (pure False)
                          (\e -> Typechecker.wellTyped readTypeOf e)
                          (Term.modify (const focus) loc e)
      let fi (e,_) = f e
      let currentApplies = maybe [] (\e -> TermEdit.applications e admissible) (Term.at loc e) `zip` [0..]
      matchingCurrentApplies <- case Term.at loc e of
        -- if we're pointing to a Var, matchingCurrentApplies is redundant with `matchingLocals`
        Just (Term.Var' _) -> pure []
        _ -> map snd <$> filterM fi currentApplies
      subterm <- maybe (fail "invalid path") pure (Term.at loc e)
      matchingLocals <- filterM f (locals >>= (\(v,t) -> TermEdit.applications (Term.var v) t))
      pure (subterm, current, admissible, annotatedLocals, matchingCurrentApplies, matchingLocals)

    search e loc limit query _ =
      let
        typeOk focus = maybe (pure False)
                             (\e -> Typechecker.wellTyped readTypeOf e)
                             (Term.modify (const focus) loc e)
        elaborate h = (\t -> TermEdit.applications (Term.ref h) t) <$> readTypeOf h
        queryOk e = do mds <- traverse (Store.readMetadata store) (Set.toList (Term.dependencies' e))
                       pure $ any (Metadata.matches query) mds
        trim rs =
          let rs' = sortBy (comparing fst) (map (\e -> (negate (Term.countBlanks e), e)) rs)
          in (map snd (take limit rs'), length (drop limit rs'))
      in
      do
        hs <- Store.hashes store Nothing
        tmatches <- do es <- traverse elaborate (Set.toList hs)
                       filterM typeOk (join es)
        qmatches <- filterM queryOk tmatches
        qmatches' <- filterM queryOk (map Term.ref (Set.toList hs))
        illtypedQmatches <-
          -- return type annotated versions of ill-typed terms
          let terms = qmatches' `ABT'.subtract` qmatches
          in zipWith Term.ann terms <$> traverse (Typechecker.synthesize readTypeOf) terms
        mds <- mapM (\h -> (,) h <$> Store.readMetadata store h)
                    (Set.toList (Set.unions (map Term.dependencies' (illtypedQmatches ++ qmatches))))
        pure $ Node.SearchResults
          query
          mds
          (trim qmatches)
          (trim illtypedQmatches)
          (Metadata.queryPositions query)

    readTermRef (Reference.Derived h) = Store.readTerm store h
    readTermRef r = pure (Term.ref r)

    terms hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> readTermRef h) hs)

    transitiveDependencies = error "todo"

    transitiveDependents = error "todo"

    types hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> readTypeOf h) hs)

    typeAt ctx loc =
      Typechecker.typeAt readTypeOf loc ctx

    updateMetadata = Store.writeMetadata store
  in Node
       admissibleTypeAt
       createTerm
       createType
       dependencies
       dependents
       edit
       evaluateTerms
       localInfo
       metadatas
       search
       terms
       transitiveDependencies
       transitiveDependents
       types
       typeAt
       updateMetadata
