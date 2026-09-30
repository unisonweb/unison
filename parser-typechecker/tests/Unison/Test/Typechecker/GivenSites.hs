module Unison.Test.Typechecker.GivenSites (test) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..), SiteId (..), atSite, siteId)
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Typechecker.GivenSites
import Unison.Var qualified as Var

test :: Test ()
test =
  let x = Var.named "x" :: Symbol
      y = Var.named "y"
      v = Term.var External
      nested = Term.lam External (External, x) (Term.app External (v x) (Term.lam External (External, x) (Term.app External (v x) (v y))))
      prepared = prepare nested
      nodes node = node : foldMap nodes (ABT.out node)
      identities = map (siteId . ABT.annotation) . nodes . preparedTerm
      fresh = Map.elems (binderRenamings prepared)
   in scope "given-sites" $
        tests
          [ scope "same-location-distinct-sites" $ do
              let ids = identities prepared
              expect (all isJust ids)
              expectEqual (length ids) (Set.size (Set.fromList ids))
              expectEqual (map ABT.annotation (nodes (preparedTerm prepared))) (map ABT.annotation (nodes nested)),
            scope "binders-not-renamed-before-checking" $ do
              expectEqual (preparedTerm prepared) nested
              expectEqual [name | node <- nodes (preparedTerm prepared), ABT.Abs name _ <- [ABT.out node]] [x, x],
            scope "shadowed-binders-have-fresh-identities" $ do
              expectEqual (length fresh) 2
              expectEqual (Set.size (Set.fromList fresh)) 2
              expect (Set.disjoint (Set.fromList fresh) (Set.fromList (ABT.allVars nested))),
            scope "top-level-names-preserved" $ do
              let file = Term.letRec' True [(x, External, nested)] (v x)
                  p = prepare file
              expectEqual (Map.size (binderRenamings p)) 2
              expectEqual (preparedTerm p) file,
            scope "local-recursive-bindings-are-freshened" $ do
              let block = Term.letRec' False [(x, External, v y), (y, External, v x)] (v x)
              expectEqual (Map.size (binderRenamings (prepare block))) 2,
            scope "expression-depths-enclose-generated-parameters" $ do
              let p = prepare (Term.lam External (External, x) (Term.letRec' False [(y, External, v x)] (v y)))
                  levels = expressionDepths p
                  depth node = levels Map.! fromMaybe (error "test site missing") (siteId (ABT.annotation node))
              case preparedTerm p of
                lambda@(Term.LamNamed' _ block@(Term.LetRecNamed' [(_, binding)] body)) -> do
                  expectEqual (depth lambda) 0
                  expectEqual (depth block) 1
                  expectEqual (depth binding) 2
                  expectEqual (depth body) 2
                _ -> crash "wrong scope fixture",
            scope "nested-lexical-depths" $
              expectEqual (Map.elems (binderDepths prepared)) [1, 2],
            scope "recursive-group-shares-one-depth" $ do
              let block = Term.letRec' False [(x, External, v y), (y, External, v x)] (v x)
                  file = Term.letRec' True [(x, External, v y), (y, External, v x)] (v x)
              expectEqual (Map.elems (binderDepths (prepare block))) [1, 1]
              expectEqual (Map.elems (binderDepths (prepare file))) [0, 0],
            scope "renaming-preserves-binding-and-free-variables" $ do
              let renamed = renameLocals prepared
              expectEqual renamed nested
              expectEqual (ABT.freeVars renamed) (Set.singleton y)
              case renamed of
                Term.LamNamed' outer (Term.App' (Term.Var' outerUse) (Term.LamNamed' inner (Term.App' (Term.Var' innerUse) (Term.Var' free)))) -> do
                  expect (outer /= inner)
                  expectEqual outer outerUse
                  expectEqual inner innerUse
                  expectEqual free y
                _ -> crash "unexpected renamed tree",
            scope "renaming-mutual-recursion" $ do
              let block = Term.letRec' False [(x, External, v y), (y, External, v x)] (v x)
                  renamed = renameLocals (prepare block)
              expectEqual renamed block
              case renamed of
                Term.LetRecNamed' [(a, Term.Var' bUse), (b, Term.Var' aUse)] (Term.Var' result) -> do
                  expectEqual a aUse
                  expectEqual b bUse
                  expectEqual result a
                  expect (a /= x && b /= y)
                _ -> crash "unexpected recursive block",
            scope "top-level-reference-not-captured-by-local-name" $ do
              let file = Term.letRec' True [(x, External, Term.lam External (External, x) (v x))] (v x)
              case renameLocals (prepare file) of
                Term.LetRecNamed' [(top, Term.LamNamed' local (Term.Var' localUse))] (Term.Var' result) -> do
                  expectEqual top x
                  expectEqual result x
                  expect (local /= x)
                  expectEqual local localUse
                _ -> crash "unexpected file",
            scope "recursive-givens-visible-in-every-rhs" $ do
              let p = prepare (Term.letRec' False [(x, External, v y), (y, External, v x)] (v x))
                  marked = Map.keysSet (binderRenamings p)
                  visible = visibleGivens p marked
              case preparedTerm p of
                Term.LetRecNamed' bindings body ->
                  for_ (body : map snd bindings) \node ->
                    expectEqual (Map.lookup ((fromMaybe (error "test site missing")) (siteId (ABT.annotation node))) visible) (Just marked)
                _ -> crash "wrong recursive fixture",
            scope "nonrecursive-given-only-visible-in-body" $ do
              let p = prepare (Term.let1' False [(x, v y)] (v x))
                  marked = Map.keysSet (binderRenamings p)
                  visible = visibleGivens p marked
              case preparedTerm p of
                Term.Let1Named' _ rhs body -> do
                  expectEqual (Map.lookup ((fromMaybe (error "test site missing")) (siteId (ABT.annotation rhs))) visible) (Just Set.empty)
                  expectEqual (Map.lookup ((fromMaybe (error "test site missing")) (siteId (ABT.annotation body))) visible) (Just marked)
                _ -> crash "wrong nonrecursive fixture",
            scope "givens-do-not-leak-to-siblings" $ do
              let p = prepare (Term.app External (Term.lam External (External, x) (v x)) (Term.lam External (External, y) (v y)))
                  marked = Map.keysSet (binderRenamings p)
                  visible = visibleGivens p marked
              case preparedTerm p of
                Term.App' (Term.LamNamed' _ left) (Term.LamNamed' _ right) -> do
                  let at node = visible Map.! (fromMaybe (error "test site missing")) (siteId (ABT.annotation node))
                  expectEqual (Set.size (at left)) 1
                  expectEqual (Set.size (at right)) 1
                  expect (Set.disjoint (at left) (at right))
                _ -> crash "wrong sibling fixture",
            scope "unmarked-shadow-does-not-replace-given" $ do
              let marked = Set.singleton (fst (Map.findMin (binderRenamings prepared)))
                  visible = visibleGivens prepared marked
                  innermost = last (nodes (preparedTerm prepared))
              expectEqual (Map.lookup ((fromMaybe (error "test site missing")) (siteId (ABT.annotation innermost))) visible) (Just marked),
            scope "deterministic-and-replaces-old-identities" $ do
              let stale = ABT.amap (atSite (SiteId 99)) nested
              expectEqual (identities (prepare stale)) (identities prepared)
              expectEqual (binderRenamings (prepare stale)) (binderRenamings prepared)
          ]
