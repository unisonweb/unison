{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Patch where

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
--import           Control.Monad.Extra            ( whenM )
-- import           Data.GUID                (genText)
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Foldable                  ( foldl' )
import           Unison.Codebase.TermEdit       ( TermEdit, Typing(Same) )
import qualified Unison.Codebase.TermEdit      as TermEdit
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H


import           Unison.Reference               ( Reference )
--import qualified Unison.Reference             as Reference

import qualified Unison.Util.Relation          as R
import           Unison.Util.Relation           ( Relation )

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  } deriving (Eq, Ord)

makeLenses ''Patch

empty :: Patch
empty = Patch mempty mempty

isEmpty :: Patch -> Bool
isEmpty p = p == empty

-- we need:
-- all of the references from the `new` edits,
-- plus all of the references for things we're replacing
collectForTyping :: [(Reference, Reference)] -> Patch -> Set Reference
collectForTyping new old = foldl' f mempty (new ++ fromOld) where
  f acc (r, r') = Set.union (Set.fromList [r, r']) acc
  newLHS = Set.fromList . fmap fst $ new
  fromOld :: [(Reference, Reference)]
  fromOld = [ (r,r') | (r, TermEdit.Replace r' _) <- R.toList . _termEdits $ old
                     , Set.member r' newLHS ]


updateTerm :: (Reference -> Reference -> Typing)
           -> Reference -> TermEdit -> Patch -> Patch
updateTerm typing r edit p =
  -- get D ~= lookupRan r
  -- for each d ∈ D, remove (d, r) and add (d, r')
  -- add (r, r') and remove (r', r')
  let deleteCycle = case edit of
        TermEdit.Deprecate -> id
        TermEdit.Replace r' _ -> R.delete r' (TermEdit.Replace r' Same)
      edits' :: Relation Reference TermEdit
      edits' = deleteCycle . R.insert r edit . R.map f $ _termEdits p
      f (x, TermEdit.Replace y _) | y == r = case edit of
        TermEdit.Replace r' _ -> (x, TermEdit.Replace r' (typing x r'))
        TermEdit.Deprecate -> (x, TermEdit.Deprecate)
      f p = p
  in p { _termEdits = edits' }

updateType :: Reference -> TypeEdit -> Patch -> Patch
updateType r edit p =
  let deleteCycle = case edit of
        TypeEdit.Deprecate -> id
        TypeEdit.Replace r' -> R.delete r' (TypeEdit.Replace r')
      edits' :: Relation Reference TypeEdit
      edits' = deleteCycle . R.insert r edit . R.map f $ _typeEdits p
      f (x, TypeEdit.Replace y) | y == r = case edit of
        TypeEdit.Replace r' -> (x, TypeEdit.Replace r')
        TypeEdit.Deprecate -> (x, TypeEdit.Deprecate)
      f p = p
  in p { _typeEdits = edits' }


-- todo: replace with monoid for patch diff for 3-way merge
instance Semigroup Patch where
  a <> b = Patch (_termEdits a <> _termEdits b)
                 (_typeEdits a <> _typeEdits b)

instance Hashable Patch where
  tokens e = [ H.Hashed (H.accumulate (H.tokens (_termEdits e))),
               H.Hashed (H.accumulate (H.tokens (_typeEdits e))) ]

