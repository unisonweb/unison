{-# LANGUAGE OverloadedStrings #-}
module Unison.Node.Common (node) where

import Control.Applicative
import Control.Monad
import Unison.Edit.Term.Eval as Eval
import Unison.Node as N
import Unison.Node.Metadata as MD
import Unison.Node.Store
import Unison.Note (Noted)
import Unison.Syntax.Hash as H
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Unison.Edit.Term as TE
import qualified Unison.Syntax.Reference as R
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Type as T
import qualified Unison.Type as Type

node :: (Applicative f, Monad f) => Eval (Noted f) -> Store f  -> Node f R.Reference Type Term
node eval store =
  let
    readTypeOf h =
      readMetadata store h >>=
                   \md -> readType store (MD.annotation md)

    admissibleTypeOf e loc =
      TE.admissibleTypeOf readTypeOf loc e

    createTerm e md = do
      t <- Type.synthesize readTypeOf e
      ((R.Derived h,_), subterms) <- pure $ E.hashCons e
      ht <- pure $ T.finalizeHash t
      writeTerm store h e
      writeType store ht t
      writeMetadata store (R.Derived h) (md { MD.annotation = ht })
      pure h <* mapM_ go subterms where -- declare all subterms extracted via hash-consing
        go (h,e) = do
          t <- Type.synthesize readTypeOf e
          ht <- pure $ T.finalizeHash t
          writeTerm store h e
          writeType store ht t
          writeMetadata store (R.Derived h) (MD.syntheticTerm ht)

    createType t md = let h = T.finalizeHash t in do
      writeType store h t
      writeMetadata store (R.Derived h) md
      pure h -- todo: kindchecking

    dependencies limit (R.Builtin _) = pure S.empty
    dependencies limit (R.Derived h) = let trim = maybe id S.intersection limit in do
      e <- readTerm store h
      pure $ trim (E.dependencies e)

    dependents limit h = do
      hs <- hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (S.toList hs)
      pure $ S.fromList [x | (x,deps) <- hs', S.member h deps]

    edit path action e = do
      TE.interpret eval (readTerm store) readTypeOf path action e

    editType = error "todo later"

    metadatas hs =
      M.fromList <$> sequence (map (\h -> (,) h <$> readMetadata store h) hs)

    search t limit query = do
      hs <- hashes store limit
      hs' <- case t of
        Nothing -> pure $ S.toList hs
        Just t -> filterM (\h -> flip Type.isSubtype t <$> readTypeOf h) (S.toList hs)
      mds <- mapM (\h -> (,) h <$> readMetadata store h) hs'
      pure . M.fromList . filter (\(_,md) -> MD.matches query md) $ mds

    searchLocal h _ typ query =
      let
        allowed :: T.Type -> Bool
        allowed = maybe (const True) (flip Type.isSubtype) typ
      in do
        t <- readTypeOf h
        ctx <- readTermRef (E.Ref h) -- todo, get rid of `Con`?
        md <- readMetadata store h
        locals <- pure .
                  filter (\(v,lt) -> allowed lt && MD.localMatches v query md) $
                  TE.locals ctx t
        pure $ (md, locals)
      -- todo: if path is non-null, need to read type of all local variables
      -- introduced in nested lambdas

    readTermRef (E.Ref (R.Derived h)) = readTerm store h
    readTermRef (E.Con (R.Derived h)) = readTerm store h
    readTermRef r = pure r

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
  in undefined
  --N.Node
  --     admissibleTypeOf
  --     createTerm
  --     createType
  --     dependencies
  --     dependents
  --     edit
  --     editType
  --     metadatas
  --     search
  --     searchLocal
  --     terms
  --     transitiveDependencies
  --     transitiveDependents
  --     types
  --     typeOf
  --     typeOfConstructorArg
  --     updateMetadata
