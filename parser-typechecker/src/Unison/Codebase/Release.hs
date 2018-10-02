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

{- Denotation, a Release is a `Namespace -> Namespace`,
   basically an upgrade function from prior namespaces.  -}

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

instance Monoid (Release v a) where
  mempty = Release mempty mempty mempty mempty
  mappend (Release _n1 t1 d1 e1) (Release n2 t2 d2 e2) =
    Release n2
            (chain TermEdit.toReference t1 t2)
            (chain TypeEdit.toReference d1 d2)
            (chain TypeEdit.toReference e1 e2)
