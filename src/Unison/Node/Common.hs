module Unison.Node.Common (node) where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad
import Unison.Node.Metadata as MD
import qualified Unison.Type as Type
import Unison.Syntax.Hash as H
import qualified Unison.Syntax.Type as T
import qualified Unison.Syntax.Term as E
import qualified Unison.Edit.Term.Path as P
import qualified Unison.Edit.Term as TE
import Unison.Edit.Term.Eval as Eval
import Unison.Syntax.Type (Type)
import Unison.Syntax.Term (Term)
import Unison.Node as N
import Unison.Node.Store
import Unison.Note (Noted)

node :: (Applicative f, Monad f) => Eval f -> Store f  -> Node f H.Hash Type Term
node eval store =
  let
    readTypeOf h = readMetadata store h >>=
                   \md -> readType store (MD.annotation md)

    admissibleTypeOf h loc = case loc of
      P.Path [] -> readTypeOf h
      P.Path _ -> do
        ctx <- term h
        TE.admissibleTypeOf readTypeOf loc ctx

    createTerm e md = do
      t <- Type.synthesize readTypeOf e
      h <- pure $ E.finalizeHash e
      ht <- pure $ T.finalizeHash t
      writeTerm store h e
      writeType store ht t
      writeMetadata store h (md { MD.annotation = ht })
      pure h

    createType t md = let h = T.finalizeHash t in do
      writeType store h t
      writeMetadata store h md
      pure h -- todo: kindchecking

    dependencies limit h = let trim = maybe id S.intersection limit in do
      e <- readTerm store h
      pure $ trim (E.dependencies e)

    dependents limit h = do
      hs <- hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (S.toList hs)
      pure $ S.fromList [x | (x,deps) <- hs', S.member h deps]

    edit k path action = do
      e <- readTerm store k
      e' <- TE.interpret eval path action e
      pure $ (E.finalizeHash e', e')

    editType = error "todo later"

    metadata = readMetadata store

    panel = error "todo"

    search t limit query = do
      hs <- hashes store limit
      hs' <- case t of
        Nothing -> pure $ S.toList hs
        Just t -> filterM (\h -> flip Type.isSubtype t <$> readTypeOf h) (S.toList hs)
      mds <- mapM (\h -> (,) h <$> metadata h) hs'
      pure . M.fromList . filter (\(_,md) -> MD.matches query md) $ mds

    searchLocal h _ typ query =
      let
        allowed :: T.Type -> Bool
        allowed = maybe (const True) (flip Type.isSubtype) typ
      in do
        t <- readTypeOf h
        ctx <- readTerm store h
        md <- readMetadata store h
        locals <- pure .
                  filter (\(v,lt) -> allowed lt && MD.localMatches v query md) $
                  TE.locals ctx t
        pure $ (md, locals)
      -- todo: if path is non-null, need to read type of all local variables
      -- introduced in nested lambdas

    term =
      readTerm store

    transitiveDependencies = error "todo"

    transitiveDependents = error "todo"

    typ =
      readType store

    typeOf h loc = case loc of
      P.Path [] -> readTypeOf h
      P.Path _ -> do
        ctx <- term h
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
       metadata
       panel
       search
       searchLocal
       term
       transitiveDependencies
       transitiveDependents
       typ
       typeOf
       typeOfConstructorArg
       updateMetadata
