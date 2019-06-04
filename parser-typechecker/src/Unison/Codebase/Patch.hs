{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Patch where

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
--import           Control.Monad.Extra            ( whenM )
-- import           Data.GUID                (genText)
import           Unison.Codebase.TermEdit       ( TermEdit, Typing(Same) )
import qualified Unison.Codebase.TermEdit      as TermEdit
import           Unison.Codebase.TypeEdit       ( TypeEdit )
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
      edits' = deleteCycle
             . R.insert r edit
             . R.map f
             $ _termEdits p
      f (x, TermEdit.Replace y _) | y == r = case edit of
        TermEdit.Replace r' _ -> (x, TermEdit.Replace r' (typing x r'))
        TermEdit.Deprecate -> (x, TermEdit.Deprecate)
      f p = p
  in p { _termEdits = edits' }


-- todo: replace with monoid for patch diff for 3-way merge
instance Semigroup Patch where
  a <> b = Patch (_termEdits a <> _termEdits b)
                 (_typeEdits a <> _typeEdits b)

instance Hashable Patch where
  tokens e = [ H.Hashed (H.accumulate (H.tokens (_termEdits e))),
               H.Hashed (H.accumulate (H.tokens (_typeEdits e))) ]

