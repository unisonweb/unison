{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.OldBranch where

import           Control.Lens

import           Unison.Codebase.TermEdit (TermEdit)
import           Unison.Codebase.TypeEdit (TypeEdit)
import           Unison.Name              (Name)
import           Unison.Reference         (Reference)
import           Unison.Referent          (Referent)
import           Unison.Util.Relation     (Relation)


data Namespace =
  Namespace { _terms :: Relation Name Referent
            , _types :: Relation Name Reference
            } deriving (Eq, Show)

makeLenses ''Namespace

data Branch0 =
  Branch0 { _namespace :: Namespace
          , _editedTerms :: Relation Reference TermEdit
          , _editedTypes :: Relation Reference TypeEdit
          } deriving (Eq, Show)

makeLenses ''Branch0

instance Semigroup Namespace where
  (<>) = mappend
instance Monoid Namespace where
  mempty = Namespace mempty mempty
  mappend (Namespace tm1 tp1) (Namespace tm2 tp2) =
    Namespace (tm1 <> tm2) (tp1 <> tp2)

instance Semigroup Branch0 where
  (<>) = mappend
instance Monoid Branch0 where
  mempty = Branch0 mempty mempty mempty
  mappend (Branch0 ns1 tm1 tp1) (Branch0 ns2 tm2 tp2) =
    Branch0 (ns1 <> ns2) (tm1 <> tm2) (tp1 <> tp2)

-- What does it mean to be a conflict that is both an edit and a name conflict?
-- (inspired by OutputMessages.todoOutput)
--
-- A name conflict is:
--   user 1 assigns name 'foo' to #abc
--   user 2 assigns name 'foo' to #xyz
--
-- An edit conflict is:
--   user 1 updates #a to #b
--   user 2 updates #a to #c
--
-- A name+edit conflict is:
--   user 1 updates 'foo#a' to 'foo#b'
--     i.e. (foo -> #b) and (#a -> #b)
--   user 2 updates 'foo#a' to 'foo#c'
--     i.e. (foo -> #c) and (#a -> #c)
--
-- So we say: if there is a name+edit conflict, we show it in the
-- edit conflicts section.
