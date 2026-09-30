module Unison.Test.Typechecker.GivenApply (test) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..), SiteId (..), siteId)
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.GivenApply qualified as Apply
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.GivenSites
import Unison.Var qualified as Var

test :: Test ()
test =
  let f = Var.named "f" :: Symbol
      d = Var.named "dictionary"
      e = Var.named "secondDictionary"
      a = External
      v = Term.var a
      number = preparedTerm . prepare
      source = number (Term.app a (Term.app a (v f) (Term.nat a 1)) (Term.nat a 2))
      ident node = fromMaybe (error "test fixture missing site") (siteId (ABT.annotation node))
      rewrite = Apply.Rewrite []
   in scope "given-apply" $
        tests
          [ scope "empty-decisions-preserve-source" $ expectEqual (Apply.apply Map.empty source) (Right source),
            scope "insert-between-explicit-arguments" $
              case source of
                Term.App' prefix _ ->
                  expectEqual
                    (Apply.apply (Map.singleton (ident prefix) (rewrite [v d])) source)
                    (Right (Term.app a (Term.app a (Term.app a (v f) (Term.nat a 1)) (v d)) (Term.nat a 2)))
                _ -> crash "bad application fixture",
            scope "same-location-decisions-remain-distinct" $
              case source of
                Term.App' prefix@(Term.App' head _) _ -> do
                  let edits = Map.fromList [(ident head, rewrite [v d]), (ident prefix, rewrite [v e])]
                      expected = Term.app a (Term.app a (Term.app a (Term.app a (v f) (v d)) (Term.nat a 1)) (v e)) (Term.nat a 2)
                  expectEqual (Apply.apply edits source) (Right expected)
                _ -> crash "bad nested application fixture",
            scope "introductions-bind-inserted-dictionaries" $ do
              let body = number (v f)
              expectEqual
                (Apply.apply (Map.singleton (ident body) (Apply.Rewrite [d, e] [v d, v e])) body)
                (Right (Term.lam a (a, d) (Term.lam a (a, e) (Term.app a (Term.app a (v f) (v d)) (v e))))),
            scope "unresolved-sites-are-errors" $ do
              expectEqual (Apply.apply Map.empty (v f)) (Left (Apply.MissingSite a))
              expectEqual
                (Apply.apply (Map.singleton (SiteId 999) (rewrite [v d])) source)
                (Left (Apply.UnusedSites (Set.singleton (SiteId 999)))),
            scope "duplicated-source-nodes-are-errors" $ do
              let leaf = number (v f)
                  malformed = Term.app (ABT.annotation leaf) leaf leaf
              expectEqual (Apply.apply Map.empty malformed) (Left (Apply.DuplicateSite (ident leaf))),
            scope "recursive-dictionary-choices-are-explicit" $ do
              let ty = Type.nat ()
                  leaf = Given.ResolutionTree (Given.Local d) ty []
                  tree = Given.ResolutionTree (Given.Global (Reference.Builtin "chosen")) ty [leaf, leaf]
              expectEqual
                (Apply.dictionaryTerm a tree)
                (Term.app a (Term.app a (Term.ref a (Reference.Builtin "chosen")) (v d)) (v d))
          ]
