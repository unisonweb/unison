{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Test.RemainingWork where

import           Control.Monad.Identity (Identity, runIdentity)
import           EasyTest
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           Unison.Reference (pattern Builtin)
import           Unison.Referent (Referent(Ref))
import qualified Unison.Codebase.Branch as Branch
import           Unison.Codebase.Branch (Branch0(..), ReferenceOps(..), RemainingWork(..))
import qualified Unison.Util.Relation as Relation
import           Unison.Codebase.TermEdit (TermEdit(Replace), Typing(Same))

ops :: ReferenceOps Identity
ops =
  let termDependencies = Map.fromList
        [(Builtin "foo", Set.fromList [Builtin "bar", Builtin "baz"])
        ,(Builtin "goo", Set.fromList [Builtin "bar"])
        ,(Builtin "bar", Set.fromList [Builtin "baz"])
        ,(Builtin "baz'", Set.empty)
        ,(Builtin "ping", Set.fromList [Builtin "pong", Builtin "Nat"])
        ,(Builtin "pong", Set.fromList [Builtin "ping", Builtin "Nat"])
        ]
      typeDependencies = Map.fromList [(Builtin "Int", mempty)]
  in ReferenceOps
      (pure . flip Map.member termDependencies)
      (pure . flip Map.member typeDependencies)
      (pure . fromMaybe mempty . flip Map.lookup (termDependencies <> typeDependencies))
      (pure . fromMaybe mempty . flip Map.lookup (Relation.range (Relation.fromMultimap termDependencies) <> Relation.range (Relation.fromMultimap typeDependencies)))

branch1 :: Branch0
branch1 = Branch0
  (Relation.fromList
      [("foo", Ref (Builtin "foo"))
      ,("bar", Ref (Builtin "bar"))
      ,("ping", Ref (Builtin "ping"))
      ,("pong", Ref (Builtin "pong"))])
  mempty
  (Relation.singleton "Int" (Builtin "Int"))
  mempty
  mempty

nameConflict :: Branch0
nameConflict = Branch0
  (Relation.fromList
      [("foo", Ref (Builtin "foo"))
      ,("bar", Ref (Builtin "bar"))
      ,("bar", Ref (Builtin "bar'"))
      ,("ping", Ref (Builtin "ping"))
      ,("pong", Ref (Builtin "pong"))])
  mempty
  (Relation.singleton "Int" (Builtin "Int"))
  mempty
  mempty

edit1 :: Branch0
edit1 = Branch0
  (Relation.fromList
      [("foo", Ref (Builtin "foo"))
      ,("bar", Ref (Builtin "bar"))
      ,("ping", Ref (Builtin "ping"))
      ,("pong", Ref (Builtin "pong"))])
  mempty
  (Relation.singleton "Int" (Builtin "Int"))
  (Relation.singleton (Builtin "baz") (Replace (Builtin "baz'") Same))
  mempty


test :: Test ()
test = scope "remainingwork" . tests $ [
    scope "branch1" . expect $ remaining' branch1 == Set.empty
  ]

remaining' :: Branch0 -> Set RemainingWork
remaining' b = runIdentity $
  Branch.remaining ops (Branch.modify (const b) Branch.empty)
