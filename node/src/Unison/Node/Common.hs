{-# LANGUAGE OverloadedStrings #-}
module Unison.Node.Common (node) where

import Control.Applicative
import Control.Monad
import Unison.Edit.Term.Eval as Eval
import Unison.Edit.Term.Path as Path
import Unison.Node as N
import Unison.Node.Metadata as MD
import Unison.Node.Store
import Unison.Note (Noted)
import qualified Unison.Note as Note
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Unison.Edit.Term as TE
import qualified Unison.Syntax.Reference as R
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Type as T
import qualified Unison.Type as Type

node :: (Applicative f, Monad f) => Eval (Noted f) -> Store f  -> Node f R.Reference Type Term
node eval store =
  let
    readTypeOf h = do
      md <- readMetadata store h
      case MD.annotation md of
        R.Derived h -> readType store h
        b@(R.Builtin _) -> pure (T.Unit (T.Ref b))

    admissibleTypeOf e loc =
      TE.admissibleTypeOf readTypeOf loc e

    createTerm e md = do
      t <- Type.synthesize readTypeOf e
      ((R.Derived h,_), subterms) <- pure $ E.hashCons e
      ht <- pure $ T.finalizeHash t
      writeTerm store h e
      writeType store ht t
      writeMetadata store (R.Derived h) (md { MD.annotation = R.Derived ht })
      pure (R.Derived h) <* mapM_ go subterms where -- declare all subterms extracted via hash-consing
        go (h,e) = do
          t <- Type.synthesize readTypeOf e
          ht <- pure $ T.finalizeHash t
          writeTerm store h e
          writeType store ht t
          writeMetadata store (R.Derived h) (MD.syntheticTerm (R.Derived ht))

    createType t md = let h = T.finalizeHash t in do
      writeType store h t
      writeMetadata store (R.Derived h) md
      pure (R.Derived h) -- todo: kindchecking

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

    metadatas hs =
      M.fromList <$> sequence (map (\h -> (,) h <$> readMetadata store h) hs)

    localInfo e loc = do
      current <- TE.typeOf readTypeOf loc e
      admissible <- TE.admissibleTypeOf readTypeOf loc e
      locals <- TE.locals readTypeOf loc e
      annotatedLocals <- pure $ map (\(v,t) -> E.Var v `E.Ann` t) locals
      let f focus = maybe (pure False) (\e -> Type.wellTyped readTypeOf e) (Path.set loc focus e)
      let fi (e,_) = f e
      let currentApplies = maybe [] (\e -> drop 1 (TE.applications e admissible)) (Path.at loc e) `zip` [0..]
      matchingCurrentApplies <- case Path.at loc e of
        -- if we're pointing to a Var, matchingCurrentApplies is redundant with `matchingLocals`
        Just (E.Var _) -> pure []
        _ -> map snd <$> filterM fi currentApplies
      matchingLocals <- filterM f (locals >>= (\(v,t) -> TE.applications (E.Var v) t))
      pure (current, admissible, annotatedLocals, matchingCurrentApplies, matchingLocals)

    search t query = do
      hs <- hashes store Nothing
      hs' <- case t of
        Nothing -> pure $ S.toList hs
        Just t -> filterM (\h -> flip Type.isSubtype t <$> readTypeOf h) (S.toList hs)
      mds <- mapM (\h -> (,) h <$> readMetadata store h) hs'
      pure . map (\(h,_) -> E.Ref h) . filter (\(_,md) -> MD.matches query md) $ mds

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

    typeOfConstructorArg = error "todo"

    updateMetadata = writeMetadata store
  in N.Node
       admissibleTypeOf
       createTerm
       createType
       dependencies
       dependents
       edit
       editType
       localInfo
       metadatas
       search
       terms
       transitiveDependencies
       transitiveDependents
       types
       typeOf
       typeOfConstructorArg
       updateMetadata
