module U.Codebase.Sqlite.Patch.Diff where

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

type PatchDiff = PatchDiff' Db.TextId Db.HashId Db.ObjectId

type LocalPatchDiff = PatchDiff' LocalTextId LocalHashId LocalDefnId

type Referent'' t h = Referent' (Reference' t h) (Reference' t h)

-- | diff. = min. - sub.
data PatchDiff' t h d = PatchDiff
  { -- | elements present in min. but absent in sub.
    addedTermEdits :: Map (Referent'' t h) (Set (TermEdit' t d)),
    addedTypeEdits :: Map (Reference' t h) (Set (TypeEdit' t d)),
    -- | elements missing in min. but present in sub.
    removedTermEdits :: Map (Referent'' t h) (Set (TermEdit' t d)),
    removedTypeEdits :: Map (Reference' t h) (Set (TypeEdit' t d))
  }
  deriving (Eq, Ord, Show)

trimap ::
  (Ord t', Ord h', Ord d') =>
  (t -> t') ->
  (h -> h') ->
  (d -> d') ->
  PatchDiff' t h d ->
  PatchDiff' t' h' d'
trimap ft fh fd (PatchDiff atm atp rtm rtp) =
  PatchDiff
    (Map.bimap bimapReferent (Set.map (bimap ft fd)) atm)
    (Map.bimap (bimap ft fh) (Set.map (bimap ft fd)) atp)
    (Map.bimap bimapReferent (Set.map (bimap ft fd)) rtm)
    (Map.bimap (bimap ft fh) (Set.map (bimap ft fd)) rtp)
  where
    bimapReferent = bimap (bimap ft fh) (bimap ft fh)
