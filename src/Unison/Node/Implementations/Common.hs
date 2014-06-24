module Unison.Node.Implementations.Common where

import Data.Map as M
import Data.Maybe
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
import qualified Unison.Note as Note
import Unison.Note (Note)

data NodeState = NodeState {
  terms :: M.Map H.Hash Term, -- ^ Maps term hash to source
  typeOf :: M.Map H.Hash Type, -- ^ Maps term hash to type
  types :: M.Map H.Hash Type, -- ^ Maps type hash to source
  metadata :: M.Map H.Hash (MD.Metadata H.Hash) -- ^ Metadata for terms, types
}

empty :: NodeState
empty = NodeState M.empty M.empty M.empty M.empty

data Store f = Store {
  hashes :: Maybe (S.Set H.Hash) -> f (S.Set H.Hash), -- ^ The set of hashes in this store, optionally constrained to intersect the given set
  readTerm :: H.Hash -> f (Maybe Term),
  writeTerm :: H.Hash -> Term -> f (),
  readType :: H.Hash -> f (Maybe Type),
  writeType :: H.Hash -> Type -> f (),
  readTypeOf :: H.Hash -> f (Maybe Type),
  writeTypeOf :: H.Hash -> Type -> f (),
  readMetadata :: H.Hash -> f (Maybe (MD.Metadata H.Hash)),
  writeMetadata :: H.Hash -> MD.Metadata H.Hash -> f ()
}

lookup' :: (Show a, Monad f) => (a -> f (Maybe b)) -> a -> f (Either Note b)
lookup' f a = liftM (Note.note' missing) (f a) where
  missing = "unknown Unison ID: " ++ show a

node :: (Applicative f, Monad f) => Eval f -> Store f -> Node f H.Hash Type Term
node eval store =
  let
    createTerm e md = do
      t <- Note.noted $ Type.synthesize (lookup' (readTypeOf store)) e
      h <- pure $ E.finalizeHash e
      Note.lift $ do
        writeTerm store h e
        writeTypeOf store h t
        writeMetadata store h md
        pure h
    createType t md = let h = T.finalizeHash t in Note.lift $ do
      writeType store h t
      writeMetadata store h md
      pure h -- todo: kindchecking
    dependencies limit h = let trim = maybe id S.intersection limit in do
      e <- Note.noted' (unknown h) $ readTerm store h
      pure $ trim (E.dependencies e)
    dependents limit h = do
      hs <- Note.lift $ hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (S.toList hs)
      pure $ S.fromList [x | (x,deps) <- hs', S.member h deps]
    edit k path action = do
      e <- Note.noted' (unknown k) $ readTerm store k
      e' <- Note.noted $ TE.apply eval path action e
      h <- pure $ E.finalizeHash e'
      pure $ (h, e')
    editType = error "todo later"
    metadata k = Note.noted' (unknown k) (readMetadata store k)
    panel = error "todo"
    search = error "todo"
    searchLocal = error "todo"
    term k = Note.noted' (unknown k) (readTerm store k)
    transitiveDependencies = error "todo"
    transitiveDependents = error "todo"
    typ k = Note.noted' (unknown k) (readType store k)
    typeOf h p = Note.noted' (unknown h) $ case p of
      P.Path [] -> readTypeOf store h
      P.Path _ -> error "todo: typeOf"
    typeOfConstructorArg = error "todo"
    updateMetadata k md = Note.lift (writeMetadata store k md)
    unknown k = "unknown Unison ID: "++show k
  in N.Node
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
