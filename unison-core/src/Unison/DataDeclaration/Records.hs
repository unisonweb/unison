-- | This module contains various utilities related to the implementation of record types.
module Unison.DataDeclaration.Records
  ( generateRecordAccessors,
    RecordKind (..),
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
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var

-- | Which surface keyword introduced the record. 'ClassRecord' is for
-- @class T a = { ... }@ declarations: the accessors elide setters and
-- modifiers, and the caller is expected to attach a type annotation
-- whose @T a@ parameter is implicit (an @=>@ arrow) so the dictionary
-- gets threaded through by the implicit-resolution elaborator. See
-- @Unison.Syntax.FileParser@ where this is wired.
data RecordKind = TypeRecord | ClassRecord
  deriving stock (Eq, Show)

-- | Generate getter (and, for 'TypeRecord', setter and modifier)
-- functions for each field of a record-style data declaration.
--
-- Each accessor body is wrapped with a 'Term.Ann' carrying its
-- declared type. The annotation lets the typechecker check (rather
-- than infer) the accessor against fields whose declared type
-- contains nested 'forall' quantifiers — pattern matching on such a
-- constructor argument produces instantiated existentials that
-- cannot be re-generalized from a purely inferred result, but
-- check-direction handling skolemizes the inner foralls and
-- succeeds. For 'ClassRecord', only the getter is emitted (here
-- without an annotation — the caller attaches an @=>@-bearing
-- annotation; see 'annotateClassAccessor' in
-- 'Unison.Syntax.FileParser').
generateRecordAccessors ::
  (Semigroup a, Var v) =>
  RecordKind ->
  (List.NonEmpty v -> v) ->
  (a -> a) ->
  -- | Each field as @(name, annotation, declared type)@. The
  -- declared type is used to build the accessor's annotation.
  [(v, a, Type v a)] ->
  -- | Type-level parameters of the enclosing data declaration. These
  -- become the outermost 'forall' on each accessor's annotation so
  -- the body is polymorphic in them.
  [v] ->
  v ->
  TypeReference ->
  [(v, a, Term v a)]
generateRecordAccessors kind namespaced generatedAnn fields tyvars typename typ =
  join [tm t i | (t, i) <- fields `zip` [(0 :: Int) ..]]
  where
    argname = Var.uncapitalize typename
    -- The enclosing record's own type, applied to its parameters:
    -- e.g. @Point a b@ for @type Point a b = …@.
    selfType ann =
      foldl' (\acc tyv -> Type.app ann acc (Type.var ann tyv)) (Type.ref ann typ) tyvars
    -- Quantify a body type by the data decl's tyvars only; field
    -- types' own 'forall's stay nested as-is.
    quantify ann body = Type.foralls ann tyvars body
    arrow ann i o = Type.arrow ann i o
    tm (fname, fieldAnn, fieldTy) i = case kind of
      TypeRecord ->
        [ (namespaced (typename :| [fname]), ann, Term.ann ann get getTy),
          (namespaced (typename :| [fname, Var.named "set"]), ann, Term.ann ann set setTy),
          (namespaced (typename :| [fname, Var.named "modify"]), ann, Term.ann ann modify modifyTy)
        ]
      ClassRecord ->
        -- 'class' accessors emit only the getter; setters and
        -- modifiers do not make sense when the dictionary is
        -- threaded implicitly. The getter term is identical to the
        -- type-record getter — the caller attaches the
        -- @=>@-bearing type annotation that turns the leading
        -- lambda's binder into a lexical given for the body.
        [(namespaced (typename :| [fname]), ann, get)]
      where
        ann = generatedAnn fieldAnn
        conref = ConstructorReference typ 0
        pat = Pattern.Constructor ann conref

        -- Accessor type annotations. Quantifying over the data
        -- decl's tyvars wraps the outer 'forall'; the field's own
        -- 'forall's (if any) remain nested inside @fieldTy@.
        getTy = quantify ann (arrow ann (selfType ann) fieldTy)
        setTy = quantify ann (arrow ann fieldTy (arrow ann (selfType ann) (selfType ann)))
        modifyTy =
          quantify
            ann
            (arrow ann (arrow ann fieldTy fieldTy) (arrow ann (selfType ann) (selfType ann)))

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
                Var.freshIn (Set.fromList $ [argname] <> (fst3 <$> fields)) fname
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
                [v | ((v, _, _), j) <- fields `zip` [0 ..], j /= i]
            -- [x, y', z]
            vargs =
              [ if j == i then Term.var ann fname' else Term.var ann v
              | ((v, _, _), j) <- fields `zip` [0 ..]
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
                  (Set.fromList $ [argname] <> (fst3 <$> fields))
                  (Var.named "f")
            cargs = [Pattern.Var ann | _ <- fields]
            rhs =
              foldr
                (ABT.abs' ann)
                (Term.constructor ann conref `Term.apps'` vargs)
                (fst3 <$> fields)
            vargs =
              [ if j == i
                  then Term.apps' (Term.var ann fname') [Term.var ann v]
                  else Term.var ann v
              | ((v, _, _), j) <- fields `zip` [0 ..]
              ]

    fst3 (x, _, _) = x
