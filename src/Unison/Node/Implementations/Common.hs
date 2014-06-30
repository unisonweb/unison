module Unison.Node.Implementations.Common where

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
import Unison.Note (Noted)

data NodeState = NodeState {
  terms :: M.Map H.Hash Term, -- ^ Maps term hash to source
  typeOf :: M.Map H.Hash Type, -- ^ Maps term hash to type
  types :: M.Map H.Hash Type, -- ^ Maps type hash to source
  metadata :: M.Map H.Hash (MD.Metadata H.Hash) -- ^ Metadata for terms, types
}

empty :: NodeState
empty = NodeState M.empty M.empty M.empty M.empty

data Store f = Store {
  hashes :: Maybe (S.Set H.Hash) -> Noted f (S.Set H.Hash), -- ^ The set of hashes in this store, optionally constrained to intersect the given set
  readTerm :: H.Hash -> Noted f Term,
  writeTerm :: H.Hash -> Term -> Noted f (),
  readType :: H.Hash -> Noted f Type,
  writeType :: H.Hash -> Type -> Noted f (),
  readTypeOf :: H.Hash -> Noted f Type,
  writeTypeOf :: H.Hash -> Type -> Noted f (),
  readMetadata :: H.Hash -> Noted f (MD.Metadata H.Hash),
  writeMetadata :: H.Hash -> MD.Metadata H.Hash -> Noted f ()
}

node :: (Applicative f, Monad f) => Eval f -> Store f -> Node f H.Hash Type Term
node eval store =
  let
    admissibleTypeOf h loc = case loc of
      P.Path [] -> readTypeOf store h
      P.Path _ -> do
        ctx <- term h
        TE.admissibleTypeOf (readTypeOf store) loc ctx

    createTerm e md = do
      t <- Type.synthesize (readTypeOf store) e
      h <- pure $ E.finalizeHash e
      writeTerm store h e
      writeTypeOf store h t
      writeMetadata store h md
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
        Just t -> filterM (\h -> flip Type.isSubtype t <$> readTypeOf store h) (S.toList hs)
      mds <- mapM (\h -> (,) h <$> metadata h) hs'
      pure . M.fromList . filter (\(_,md) -> MD.matches query md) $ mds

    -- searchLocal :: k -> Maybe t -> Query -> Noted m [(e, Metadata k)],
    searchLocal h path t query = do
      t <- readTypeOf store h
      md <- readMetadata store h
      -- somehow read the type of all local variables from the given t
      -- what about if we're inside a nested lambda? then we need
      -- to obtain the type of that lambda
      undefined

    term =
      readTerm store

    transitiveDependencies = error "todo"

    transitiveDependents = error "todo"

    typ =
      readType store

    typeOf h loc = case loc of
      P.Path [] -> readTypeOf store h
      P.Path p -> do
        ctx <- term h
        TE.typeOf (readTypeOf store) loc ctx

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
