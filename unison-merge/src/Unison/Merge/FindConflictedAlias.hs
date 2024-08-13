module Unison.Merge.FindConflictedAlias
  ( findConflictedAlias,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Updated qualified
import Unison.Prelude
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..), DefnsF3)

-- @findConflictedAlias namespace diff@, given an old namespace and a diff to a new namespace, will return the first
-- "conflicted alias" encountered (if any), where a "conflicted alias" is a pair of names that referred to the same
-- thing in the old namespace, but different things in the new one.
--
-- For example, if the old namespace was
--
--   foo = #foo
--   bar = #foo
--
-- and the new namespace is
--
--   foo = #baz
--   bar = #qux
--
-- then (foo, bar) is a conflicted alias.
findConflictedAlias ::
  forall name synhashed term typ.
  (Ord name, forall ref. Eq (synhashed ref), Ord term, Ord typ) =>
  Defns (BiMultimap term name) (BiMultimap typ name) ->
  DefnsF3 (Map name) DiffOp synhashed term typ ->
  Maybe (Defn (name, name) (name, name))
findConflictedAlias defns diff =
  asum [TermDefn <$> go defns.terms diff.terms, TypeDefn <$> go defns.types diff.types]
  where
    go ::
      forall ref.
      (Eq (synhashed ref), Ord ref) =>
      BiMultimap ref name ->
      Map name (DiffOp (synhashed ref)) ->
      Maybe (name, name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (name, DiffOp (synhashed ref)) -> Maybe (name, name)
        f (name, op) =
          case op of
            DiffOp'Add _ -> Nothing
            DiffOp'Delete _ -> Nothing
            DiffOp'Update hashed1 ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hashed1.new)
                & asum
          where
            g :: synhashed ref -> name -> Maybe (name, name)
            g hashed1 alias =
              case Map.lookup alias diff of
                Just (DiffOp'Update hashed2) | hashed1 == hashed2.new -> Nothing
                -- If "foo" was updated but its alias "bar" was deleted, that's ok
                Just (DiffOp'Delete _) -> Nothing
                _ -> Just (name, alias)
