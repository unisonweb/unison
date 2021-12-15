module U.Codebase.Sqlite.Patch.Full where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import qualified U.Codebase.Sqlite.DbId as Db
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalHashId, LocalTextId)
import U.Codebase.Sqlite.Patch.TermEdit (TermEdit')
import U.Codebase.Sqlite.Patch.TypeEdit (TypeEdit')
import qualified Unison.Util.Map as Map

type Patch = Patch' Db.TextId Db.HashId Db.ObjectId

type LocalPatch = Patch' LocalTextId LocalHashId LocalDefnId

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

data Patch' t h o = Patch
  { termEdits :: Map (Referent'' t h) (Set (TermEdit' t o)),
    typeEdits :: Map (Reference' t h) (Set (TypeEdit' t o))
  }

trimap ::
  (Ord t', Ord h', Ord o') =>
  (t -> t') ->
  (h -> h') ->
  (o -> o') ->
  Patch' t h o ->
  Patch' t' h' o'
trimap ft fh fo (Patch tms tps) =
  Patch
    (Map.bimap (bimap (bimap ft fh) (bimap ft fh)) (Set.map (bimap ft fo)) tms)
    (Map.bimap (bimap ft fh) (Set.map (bimap ft fo)) tps)
