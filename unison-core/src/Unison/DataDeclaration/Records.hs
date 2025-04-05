-- | This module contains various utilities related to the implementation of record types.
module Unison.DataDeclaration.Records
  ( generateRecordAccessors,
  )
where

import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Var (Var)
import Unison.Var qualified as Var

generateRecordAccessors ::
  (Semigroup a, Var v) =>
  (List.NonEmpty v -> v) ->
  (a -> a) ->
  [(v, a)] ->
  v ->
  TypeReference ->
  [(v, a, Term v a)]
generateRecordAccessors namespaced generatedAnn fields typename typ =
  join [tm t i | (t, i) <- fields `zip` [(0 :: Int) ..]]
  where
    argname = Var.uncapitalize typename
    tm (fname, fieldAnn) i =
      [ (namespaced (typename :| [fname]), ann, get),
        (namespaced (typename :| [fname, Var.named "set"]), ann, set),
        (namespaced (typename :| [fname, Var.named "modify"]), ann, modify)
      ]
      where
        ann = generatedAnn fieldAnn
        conref = ConstructorReference typ 0
        pat = Pattern.Constructor ann conref

        -- point -> case point of Point _ y _ -> y
        get =
          Term.lam ann (ann, argname) $
            Term.match
              ann
              (Term.var ann argname)
              [Term.MatchCase (pat cargs) Nothing rhs]
          where
            -- [_, y, _]
            cargs =
              [ if j == i then Pattern.Var ann else Pattern.Unbound ann
                | (_, j) <- fields `zip` [0 ..]
              ]
            -- y -> y
            rhs = ABT.abs' ann fname (Term.var ann fname)

        -- y' point -> case point of Point x _ z -> Point x y' z
        set =
          Term.lam' ann [(ann, fname'), (ann, argname)] $
            Term.match
              ann
              (Term.var ann argname)
              [Term.MatchCase (pat cargs) Nothing rhs]
          where
            -- y'
            fname' =
              Var.named . Var.name $
                Var.freshIn (Set.fromList $ [argname] <> (fst <$> fields)) fname
            -- [x, _, z]
            cargs =
              [ if j == i then Pattern.Unbound ann else Pattern.Var ann
                | (_, j) <- fields `zip` [0 ..]
              ]
            -- x z -> Point x y' z
            rhs =
              foldr
                (ABT.abs' ann)
                (Term.constructor ann conref `Term.apps'` vargs)
                [v | ((v, _), j) <- fields `zip` [0 ..], j /= i]
            -- [x, y', z]
            vargs =
              [ if j == i then Term.var ann fname' else Term.var ann v
                | ((v, _), j) <- fields `zip` [0 ..]
              ]

        -- example: `f point -> case point of Point x y z -> Point x (f y) z`
        modify =
          Term.lam' ann [(ann, fname'), (ann, argname)] $
            Term.match
              ann
              (Term.var ann argname)
              [Term.MatchCase (pat cargs) Nothing rhs]
          where
            fname' =
              Var.named . Var.name $
                Var.freshIn
                  (Set.fromList $ [argname] <> (fst <$> fields))
                  (Var.named "f")
            cargs = [Pattern.Var ann | _ <- fields]
            rhs =
              foldr
                (ABT.abs' ann)
                (Term.constructor ann conref `Term.apps'` vargs)
                (fst <$> fields)
            vargs =
              [ if j == i
                  then Term.apps' (Term.var ann fname') [Term.var ann v]
                  else Term.var ann v
                | ((v, _), j) <- fields `zip` [0 ..]
              ]
