{-# LANGUAGE OverloadedStrings #-}
module Unison.Node.Implementation (node) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Ord
import Data.Traversable (traverse)
import Debug.Trace
import Unison.Eval as Eval
import Unison.TermPath as Path
import Unison.Metadata as MD
import Unison.Node as N
import Unison.Node.Store
import Unison.Note (Noted)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Foldable as Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Unison.Note as Note
import qualified Unison.Reference as R
import qualified Unison.Term as E
import qualified Unison.TermEdit as TE
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Type as Type

watch msg a = trace (msg ++ " : " ++ show a) a

node :: (Applicative f, Monad f) => Eval (Noted f) -> Store f  -> Node f R.Reference Type Term
node eval store =
  let
    readTypeOf = typeOfTerm store

    admissibleTypeOf e loc =
      TE.admissibleTypeOf readTypeOf loc e

    createTerm e md = do
      t <- Typechecker.synthesize readTypeOf e
      ((R.Derived h,_), subterms) <- pure $ E.hashCons e
      writeTerm store h e
      writeMetadata store (R.Derived h) md
      annotateTerm store (R.Derived h) t
      pure (R.Derived h) <* mapM_ go subterms where -- declare all subterms extracted via hash-consing
        go (h,e) = do
          t <- Typechecker.synthesize readTypeOf e
          writeTerm store h e
          annotateTerm store (R.Derived h) t

    createType _ _ = error "todo - createType"

    dependencies _ (R.Builtin _) = pure S.empty
    dependencies limit (R.Derived h) = let trim = maybe id S.intersection limit in do
      e <- readTerm store h
      pure $ trim (S.map R.Derived (E.dependencies e))

    dependents limit h = do
      hs <- hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (S.toList hs)
      pure $ S.fromList [x | (x,deps) <- hs', S.member h deps]

    edit rootPath path action e =
      (\e' -> (rootPath,e,e')) <$> TE.interpret eval (readTerm store) path action e

    editType = error "Common.editType.todo"

    evaluateTerms es = traverse go es
      where go (path,e) = (\e' -> (path,e,e')) <$> Eval.whnf eval (readTerm store) e

    metadatas hs =
      M.fromList <$> sequence (map (\h -> (,) h <$> readMetadata store h) hs)

    localInfo e loc = do
      current <- TE.typeOf readTypeOf loc e
      admissible <- TE.admissibleTypeOf readTypeOf loc e
      locals <- TE.locals readTypeOf loc e
      annotatedLocals <- pure $ map (\(v,t) -> E.Var v `E.Ann` t) locals
      let f focus = maybe (pure False) (\e -> Typechecker.wellTyped readTypeOf e) (Path.set loc focus e)
      let fi (e,_) = f e
      let currentApplies = maybe [] (\e -> TE.applications e admissible) (Path.at loc e) `zip` [0..]
      matchingCurrentApplies <- case Path.at loc e of
        -- if we're pointing to a Var, matchingCurrentApplies is redundant with `matchingLocals`
        Just (E.Var _) -> pure []
        _ -> map snd <$> filterM fi currentApplies
      subterm <- maybe (fail "invalid path") pure (Path.at loc e)
      matchingLocals <- filterM f (locals >>= (\(v,t) -> TE.applications (E.Var v) t))
      pure (subterm, current, admissible, annotatedLocals, matchingCurrentApplies, matchingLocals)

    search e loc limit query _ =
      let
        typeOk focus = maybe (pure False)
                             (\e -> Typechecker.wellTyped readTypeOf e)
                             (Path.set loc focus e)
        elaborate h = (\t -> TE.applications (E.Ref h) t) <$> readTypeOf h
        queryOk e = do mds <- traverse (readMetadata store) (S.toList (E.dependencies' e))
                       pure $ any (MD.matches query) mds
        trim rs =
          let rs' = sortBy (comparing fst) (map (\e -> (negate (E.countBlanks e), e)) rs)
          in (map snd (take limit rs'), length (drop limit rs'))
      in
      do
        hs <- hashes store Nothing
        tmatches <- do es <- traverse elaborate (S.toList hs)
                       filterM typeOk (join es)
        qmatches <- filterM queryOk tmatches
        qmatches' <- filterM queryOk (map E.Ref (S.toList hs))
        illtypedQmatches <-
          -- return type annotated versions of ill-typed terms
          let terms = S.toList (S.difference (S.fromList qmatches') (S.fromList qmatches))
          in zipWith E.Ann terms <$> traverse (Typechecker.synthesize readTypeOf) terms
        mds <- mapM (\h -> (,) h <$> readMetadata store h)
                    (S.toList (S.unions (map E.dependencies' (illtypedQmatches ++ qmatches))))
        pure $ SearchResults
          query
          mds
          (trim qmatches)
          (trim illtypedQmatches)
          (MD.queryPositions query)

    readTermRef (R.Derived h) = readTerm store h
    readTermRef r = pure (E.Ref r)

    terms hs =
      M.fromList <$> sequence (map (\h -> (,) h <$> readTermRef h) hs)

    transitiveDependencies = error "todo"

    transitiveDependents = error "todo"

    types hs =
      M.fromList <$> sequence (map (\h -> (,) h <$> readTypeOf h) hs)

    typeOf ctx loc =
      TE.typeOf readTypeOf loc ctx

    updateMetadata = writeMetadata store
  in N.Node
       admissibleTypeOf
       createTerm
       createType
       dependencies
       dependents
       edit
       editType
       evaluateTerms
       localInfo
       metadatas
       search
       terms
       transitiveDependencies
       transitiveDependents
       types
       typeOf
       updateMetadata
