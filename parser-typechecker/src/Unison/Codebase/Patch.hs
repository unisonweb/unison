{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Patch where

import           Prelude                  hiding (head,read,subtract)

import           Control.Lens            hiding ( children, cons, transform )
--import           Control.Monad.Extra            ( whenM )
-- import           Data.GUID                (genText)
import           Unison.Codebase.TermEdit       ( TermEdit )
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import           Unison.Hashable                ( Hashable )
import qualified Unison.Hashable               as H


import           Unison.Reference               ( Reference )
--import qualified Unison.Reference             as Reference

--import qualified Unison.Util.Relation          as R
import           Unison.Util.Relation           ( Relation )

data Patch = Patch
  { _termEdits :: Relation Reference TermEdit
  , _typeEdits :: Relation Reference TypeEdit
  } deriving (Eq, Ord)

makeLenses ''Patch

instance Semigroup Patch where
  a <> b = Patch (_termEdits a <> _termEdits b)
                 (_typeEdits a <> _typeEdits b)

instance Hashable Patch where
  tokens e = [ H.Hashed (H.accumulate (H.tokens (_termEdits e))),
               H.Hashed (H.accumulate (H.tokens (_typeEdits e))) ]

