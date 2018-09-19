module Unison.Codebase.Release where

import Data.Map (Map)
import Control.Applicative
import qualified Data.Map as Map
import Unison.Codebase.Code (Code)
import Unison.Codebase.Name (Name)
import Unison.Codebase.TermEdit (TermEdit)
import qualified Unison.Codebase.TermEdit as TermEdit
import Unison.Codebase.TypeEdit (TypeEdit)
import qualified Unison.Codebase.TypeEdit as TypeEdit
import Unison.Reference (Reference)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Term as Term

type DataDeclaration v a = DD.DataDeclaration' v a
type Term v a = Term.AnnotatedTerm v a

data Release v a =
  Release { namespace     :: Map Name (Code v a)
          , edited        :: Map Reference TermEdit
          , editedDatas   :: Map Reference TypeEdit
          , editedEffects :: Map Reference TypeEdit }

chain' :: (v -> Maybe k) -> (k -> Maybe v) -> (k -> Maybe v) -> (k -> Maybe v)
chain' toK m1 m2 k = case m1 k of
  -- if something upgraded by m1 is further upgraded by m2, then take m2's, else take m1's
  Just v1 -> (m2 =<< toK v1) <|> Just v1
  -- if not upgraded by m1, then take m2's
  Nothing -> m2 k

chain :: Ord k => (v -> Maybe k) -> Map k v -> Map k v -> Map k v
chain toK m1 m2 =
  Map.fromList
    [ (k,v) | k <- Map.keys m1 ++ Map.keys m2
            , Just v <- [chain' toK (`Map.lookup` m1) (`Map.lookup` m2) k] ]

instance Semigroup (Release v a) where
  (<>) = mappend

{- Denotation, a Release is a `(Namespace, Namespace -> Namespace)`,
   basically "the current namespace" and an upgrade function from
   prior namespaces.

   (<>) is interpreted as sequencing releases:

  sequence : Release -> Release -> Release
  sequence (ns1, up1) (ns2, up2) =
    -- namespace is not cumulative, but the upgrades are cumulative
    (ns2,
     \nsi -> Map.unionWith const (up2 . up1 $ nsi) (up1 nsi))
    -- Alternative implementation: both namespace and upgrades are cumulative
    (Map.unionWith const ns2 ns1,
     \nsi -> Map.unionWith const (up2 . up1 $ nsi) (up1 nsi)) -- cumulative
-}
instance Monoid (Release v a) where
  mempty = Release mempty mempty mempty mempty
  mappend (Release _n1 t1 d1 e1) (Release n2 t2 d2 e2) =
    Release n2
            (chain fromTermEdit t1 t2)
            (chain fromTypeEdit d1 d2)
            (chain fromTypeEdit e1 e2)
    where
      fromTermEdit (TermEdit.Replace r _t) = Just r
      fromTermEdit TermEdit.Deprecate      = Nothing
      fromTypeEdit (TypeEdit.Replace r)    = Just r
      fromTypeEdit TypeEdit.Deprecate      = Nothing