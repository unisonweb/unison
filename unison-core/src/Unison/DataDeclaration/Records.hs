-- | This module contains various utilities related to the implementation of record types.
module Unison.DataDeclaration.Records
  ( generateRecordAccessors,
    RecordKind (..),
  )
where

import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Map qualified as Map
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

-- | Generate getter, setter, and modify functions for each field of
-- a record-style data declaration.
--
-- Each accessor body is wrapped with a 'Term.Ann' carrying its
-- declared type. The annotation is what lets the typechecker check
-- (rather than infer) the accessor against fields whose declared
-- type contains nested 'forall' quantifiers — pattern matching on
-- such a constructor argument produces instantiated existentials
-- that can't be re-generalized in a purely inferred result, but
-- check-direction handling skolemizes the inner foralls and
-- succeeds.
--
-- The setter and modifier are given their /fully general/ types: the
-- type variables that a field is the sole one to reference can change
-- when that field is updated, so they are freshened in the result
-- type. For example @type These a b = { here : a, there : b }@ yields
--
-- > These.here.set    : c -> These a b -> These c b
-- > These.here.modify : (a -> c) -> These a b -> These c b
--
-- since @here@ is the only field mentioning @a@. A variable shared by
-- more than one field (or referenced by no field) is left fixed, so
-- such records get the usual non-type-changing accessors.
--
-- For a @class@ record ('ClassRecord') only the getter is emitted (a
-- raw term); the caller re-annotates it with the class's @=>@-bearing
-- type. Setters and modifiers don't make sense when the dictionary is
-- threaded implicitly.
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
    tyvarSet = Set.fromList tyvars
    -- The enclosing record's own type, applied to its parameters with
    -- the given type-variable renaming: e.g. @Point a b@ (or, under a
    -- @{a ↦ a1}@ renaming, @Point a1 b@) for @type Point a b = …@.
    selfTypeWith ann renaming =
      foldl'
        (\acc tyv -> Type.app ann acc (Type.var ann (Map.findWithDefault tyv tyv renaming)))
        (Type.ref ann typ)
        tyvars
    -- All variable names to steer clear of when minting fresh type
    -- variables for type-changing accessors.
    avoidVars =
      Set.unions (tyvarSet : Set.singleton argname : [Type.freeVars ty | (_, _, ty) <- fields])
    arrow ann i o = Type.arrow ann i o
    tm (fname, fieldAnn, fieldTy) i = case kind of
      TypeRecord ->
        [ (namespaced (typename :| [fname]), ann, Term.ann ann get getTy),
          (namespaced (typename :| [fname, Var.named "set"]), ann, Term.ann ann set setTy),
          (namespaced (typename :| [fname, Var.named "modify"]), ann, Term.ann ann modify modifyTy)
        ]
      ClassRecord ->
        -- 'class' accessors emit only the getter, as a /raw/ term: the
        -- caller ('Unison.Syntax.FileParser') re-annotates it with the
        -- class's @=>@-bearing type, so both setters/modifiers and the
        -- plain @->@ getter annotation are omitted here.
        [(namespaced (typename :| [fname]), ann, get)]
      where
        ann = generatedAnn fieldAnn
        conref = ConstructorReference typ 0
        pat = Pattern.Constructor ann conref

        -- The decl's type variables that this field is the /sole/ one
        -- to reference. Updating this field can change them without
        -- affecting any other field, so the setter and modifier
        -- freshen them in their result type.
        soleTyvars =
          [ v
          | v <- tyvars,
            Set.member v thisFieldVars,
            not (Set.member v otherFieldVars)
          ]
          where
            thisFieldVars = Set.intersection tyvarSet (Type.freeVars fieldTy)
            otherFieldVars =
              Set.unions
                [ Set.intersection tyvarSet (Type.freeVars ty)
                | ((_, _, ty), j) <- fields `zip` [(0 :: Int) ..],
                  j /= i
                ]
        -- A fresh counterpart for each sole-referenced variable.
        renaming = snd (foldl' freshen (avoidVars, Map.empty) soleTyvars)
          where
            freshen (used, m) v =
              let v' = Var.freshIn used v
               in (Set.insert v' used, Map.insert v v' m)
        freshTyvars = Map.elems renaming
        -- @fieldTy@ with the sole-referenced variables freshened.
        fieldTy' = ABT.renames renaming fieldTy

        -- Accessor type annotations. Quantifying over the data decl's
        -- tyvars (plus the fresh ones for set/modify) wraps the outer
        -- 'forall'; the field's own 'forall's (if any) remain nested
        -- inside @fieldTy@.
        getTy = Type.foralls ann tyvars (arrow ann (selfTypeWith ann mempty) fieldTy)
        setTy =
          Type.foralls
            ann
            (tyvars <> freshTyvars)
            (arrow ann fieldTy' (arrow ann (selfTypeWith ann mempty) (selfTypeWith ann renaming)))
        modifyTy =
          Type.foralls
            ann
            (tyvars <> freshTyvars)
            (arrow ann (arrow ann fieldTy fieldTy') (arrow ann (selfTypeWith ann mempty) (selfTypeWith ann renaming)))

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
