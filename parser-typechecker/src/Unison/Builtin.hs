{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Builtin where

import           Control.Arrow                  ( first )
import           Control.Applicative            ( liftA2
                                                , (<|>)
                                                )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text.Internal             ( Text )
import qualified Text.Megaparsec.Error         as MPE
import qualified Unison.ABT                    as ABT
import           Unison.Codebase.CodeLookup     ( CodeLookup(..) )
import qualified Unison.ConstructorType        as CT
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                )
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Lexer                  as L
import           Unison.Parser                  ( Ann(..) )
import qualified Unison.Parser                 as Parser
import           Unison.PrintError              ( prettyParseError )
import qualified Unison.Reference              as R
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Term                   as Term
import qualified Unison.TermParser             as TermParser
import           Unison.Type                    ( AnnotatedType )
import qualified Unison.Type                   as Type
import qualified Unison.TypeParser             as TypeParser
import qualified Unison.Util.ColorText         as Color
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.Names                   ( Names )
import qualified Unison.Names                  as Names
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Util.Relation          as Rel

type Term v = Term.AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann

type Type' v = AnnotatedType v ()
type Term' v = Term.AnnotatedTerm v ()

showParseError :: Var v
               => String
               -> MPE.ParseError (L.Token L.Lexeme) (Parser.Error v)
               -> String
showParseError s = Color.toANSI . prettyParseError s

t :: Var v => String -> Type v
t s = ABT.amap (const Intrinsic) .
          Names.bindType names . either (error . showParseError s) tweak $
          Parser.run (Parser.root TypeParser.valueType) s mempty
  where tweak = Type.generalizeLowercase

-- parse a term, hard-coding the builtins defined in this file
tm :: Var v => String -> Term v
tm s = Names.bindTerm constructorType names
       . either (error . showParseError s) id
       $ Parser.run (Parser.root TermParser.term) s (mempty, names)

constructorType :: R.Reference -> CT.ConstructorType
constructorType r =
  if any f (builtinDataDecls @Symbol) then CT.Data
  else if any f (builtinEffectDecls @Symbol) then CT.Effect
  else error "a builtin term referenced a constructor for a non-builtin type"
  where f = (==r) . fst . snd

-- Todo: These definitions and groupings of builtins are getting a little
-- confusing.  Sort out these labrinthine definitions!
-- We have primitive types and primitive terms, but the types of the
-- primitive terms sometimes reference decls, and not just primitive types.
-- Primitive types and primitive terms can be deprecated in future iterations
-- of the typechecker and runtime, but the builtin decls don't become
-- deprecated in the same sense.  So (to do a deprecation check on these)
names0 :: Names
names0 = Names.fromTypes builtinTypes

names :: Names
names = Names.fromBuiltins (Map.keys $ builtins0 @Symbol) <> allTypeNames

allTypeNames :: Names
allTypeNames =
  Names.fromTypes builtinTypes
    <> foldMap (DD.dataDeclToNames' @Symbol)   builtinDataDecls
    <> foldMap (DD.effectDeclToNames' @Symbol) builtinEffectDecls

-- Is this a term (as opposed to a type)
isBuiltinTerm :: R.Reference -> Bool
isBuiltinTerm r = Map.member r (builtins0 @Symbol)

isBuiltinType :: R.Reference -> Bool
isBuiltinType r = elem r . fmap snd $ builtinTypes

typeLookup :: Var v => TL.TypeLookup v Ann
typeLookup =
  TL.TypeLookup (fmap (Intrinsic <$) builtins0)
    (Map.fromList $ map snd builtinDataDecls)
    (Map.fromList $ map snd builtinEffectDecls)

builtinTypedTerms :: Var v => [(v, (Term' v, Type' v))]
builtinTypedTerms = [(v, (e, t)) | (v, (Term.Ann' e t)) <- builtinTerms ]

builtinTerms :: Var v => [(v, Term' v)]
builtinTerms =
  [ (toSymbol r, Term.ann () (Term.ref () r) typ) |
    (r, typ) <- Map.toList builtins0 ]

builtinTypesV :: Var v => [(v, R.Reference)]
builtinTypesV = first (Name.toVar) <$> builtinTypes

builtinTypeNames :: Set Name
builtinTypeNames = Set.fromList (map fst builtinTypes)

builtinTypes :: [(Name, R.Reference)]
builtinTypes = liftA2 (,) Name.unsafeFromText R.Builtin <$>
  ["Int", "Nat", "Float", "Boolean", "Sequence", "Text", "Effect", "Bytes"]

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: Var v => [(v, (R.Reference, DataDeclaration v))]
builtinDataDecls =
  [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinDataDecls ]

builtinEffectDecls :: Var v => [(v, (R.Reference, EffectDeclaration v))]
builtinEffectDecls = []

codeLookup :: (Applicative m, Var v) => CodeLookup v m Ann
codeLookup = CodeLookup (const $ pure Nothing) $ \r ->
  pure
    $ lookup r [ (r, Right x) | (R.DerivedId r, x) <- snd <$> builtinDataDecls ]
    <|> lookup
          r
          [ (r, Left x) | (R.DerivedId r, x) <- snd <$> builtinEffectDecls ]

toSymbol :: Var v => R.Reference -> v
toSymbol (R.Builtin txt) = Var.named txt
toSymbol _ = error "unpossible"

-- Relation predicate: Domain depends on range.
builtinDependencies :: Rel.Relation R.Reference R.Reference
builtinDependencies =
  Rel.fromMultimap (Type.dependencies <$> builtins0 @Symbol)

-- The dependents of a builtin type is the set of builtin terms which
-- mention that type.
builtinTypeDependents :: R.Reference -> Set R.Reference
builtinTypeDependents r = Rel.lookupRan r builtinDependencies

allReferencedTypes :: Set R.Reference
allReferencedTypes = Rel.ran builtinDependencies

builtins0 :: Var v => Map.Map R.Reference (Type' v)
builtins0 = Map.fromList $
  [ (R.Builtin name, typ) |
    (name, typ) <-
      [ ("Int.+", int --> int --> int)
      , ("Int.-", int --> int --> int)
      , ("Int.*", int --> int --> int)
      , ("Int./", int --> int --> int)
      , ("Int.<", int --> int --> boolean)
      , ("Int.>", int --> int --> boolean)
      , ("Int.<=", int --> int --> boolean)
      , ("Int.>=", int --> int --> boolean)
      , ("Int.==", int --> int --> boolean)
      , ("Int.increment", int --> int)
      , ("Int.isEven", int --> boolean)
      , ("Int.isOdd", int --> boolean)
      , ("Int.signum", int --> int)
      , ("Int.negate", int --> int)
      , ("Int.truncate0", int --> nat)

      , ("Nat.+", nat --> nat --> nat)
      , ("Nat.drop", nat --> nat --> nat)
      , ("Nat.sub", nat --> nat --> int)
      , ("Nat.*", nat --> nat --> nat)
      , ("Nat./", nat --> nat --> nat)
      , ("Nat.mod", nat --> nat --> nat)
      , ("Nat.<", nat --> nat --> boolean)
      , ("Nat.>", nat --> nat --> boolean)
      , ("Nat.<=", nat --> nat --> boolean)
      , ("Nat.>=", nat --> nat --> boolean)
      , ("Nat.==", nat --> nat --> boolean)
      , ("Nat.increment", nat --> nat)
      , ("Nat.isEven", nat --> boolean)
      , ("Nat.isOdd", nat --> boolean)
      , ("Nat.toInt", nat --> int)
      , ("Nat.toText", nat --> text)

      , ("Float.+", float --> float --> float)
      , ("Float.-", float --> float --> float)
      , ("Float.*", float --> float --> float)
      , ("Float./", float --> float --> float)
      , ("Float.<", float --> float --> boolean)
      , ("Float.>", float --> float --> boolean)
      , ("Float.<=", float --> float --> boolean)
      , ("Float.>=", float --> float --> boolean)
      , ("Float.==", float --> float --> boolean)

      -- Trigonmetric Functions
      , ("Float.acos", float --> float)
      , ("Float.asin", float --> float)
      , ("Float.atan", float --> float)
      , ("Float.atan2", float --> float --> float)
      , ("Float.cos", float --> float)
      , ("Float.sin", float --> float)
      , ("Float.tan", float --> float)

      -- Hyperbolic Functions
      , ("Float.acosh", float --> float)
      , ("Float.asinh", float --> float)
      , ("Float.atanh", float --> float)
      , ("Float.cosh", float --> float)
      , ("Float.sinh", float --> float)
      , ("Float.tanh", float --> float)

      -- Exponential Functions
      , ("Float.exp", float --> float)
      , ("Float.log", float --> float)
      , ("Float.logBase", float --> float --> float)

      -- Power Functions
      , ("Float.pow", float --> float --> float)
      , ("Float.sqrt", float --> float)

      -- Rounding and Remainder Functions
      , ("Float.ceiling", float --> int)
      , ("Float.floor", float --> int)
      , ("Float.round", float --> int)
      , ("Float.truncate", float --> int)

      -- Float Utils
      , ("Float.abs", float --> float)
      , ("Float.max", float --> float --> float)
      , ("Float.min", float --> float --> float)
      , ("Float.toText", float --> text)
      , ("Float.fromText", text --> optional float)

      , ("Universal.==", forall1 "a" (\a -> a --> a --> boolean))

      -- Universal.compare intended as a low level function that just returns
      -- `Int` rather than some Ordering data type. If we want, later,
      -- could provide a pure Unison wrapper for Universal.compare that
      -- returns a proper data type.
      --
      -- 0 is equal, < 0 is less than, > 0 is greater than
      , ("Universal.compare", forall1 "a" (\a -> a --> a --> int))
      , ("Universal.>", forall1 "a" (\a -> a --> a --> boolean))
      , ("Universal.<", forall1 "a" (\a -> a --> a --> boolean))
      , ("Universal.>=", forall1 "a" (\a -> a --> a --> boolean))
      , ("Universal.<=", forall1 "a" (\a -> a --> a --> boolean))

      , ("Boolean.not", boolean --> boolean)

      , ("Text.empty", text)
      , ("Text.++", text --> text --> text)
      , ("Text.take", nat --> text --> text)
      , ("Text.drop", nat --> text --> text)
      , ("Text.size", text --> nat)
      , ("Text.==", text --> text --> boolean)
      , ("Text.!=", text --> text --> boolean)
      , ("Text.<=", text --> text --> boolean)
      , ("Text.>=", text --> text --> boolean)
      , ("Text.<", text --> text --> boolean)
      , ("Text.>", text --> text --> boolean)

      , ("Bytes.empty", bytes)
      , ("Bytes.fromSequence", sequence nat --> bytes)
      , ("Bytes.++", bytes --> bytes --> bytes)
      , ("Bytes.take", nat --> bytes --> bytes)
      , ("Bytes.drop", nat --> bytes --> bytes)
      , ("Bytes.at", nat --> bytes --> optional nat)
      , ("Bytes.toSequence", bytes --> sequence nat)
      , ("Bytes.size", bytes --> nat)
      , ("Bytes.flatten", bytes --> bytes)

      , ("Sequence.empty", forall1 "a" (\a -> sequence a))
      , ("Sequence.cons", forall1 "a" (\a -> a --> sequence a --> sequence a))
      , ("Sequence.snoc", forall1 "a" (\a -> sequence a --> a --> sequence a))
      , ("Sequence.take", forall1 "a" (\a -> nat --> sequence a --> sequence a))
      , ("Sequence.drop", forall1 "a" (\a -> nat --> sequence a --> sequence a))
      , ("Sequence.++", forall1 "a" (\a -> sequence a --> sequence a --> sequence a))
      , ("Sequence.size", forall1 "a" (\a -> sequence a --> nat))
      , ("Sequence.at", forall1 "a" (\a -> nat --> sequence a --> optional a))

      , ("Debug.watch", forall1 "a" (\a -> text --> a --> a))
      , ("Effect.pure", forall2 "a" "e" (\a e -> a --> effect e a)) -- Effect ambient e a
      , ("Effect.bind", forall4 "e" "a" "ambient" "b" (\e a ambient b -> delayed (effectful e a) --> (a --> effectful ambient b) --> effect e a)) -- Effect ambient e a
      ]
  ]
  where
    int = Type.int ()
    nat = Type.nat ()
    boolean = Type.boolean ()
    float = Type.float ()
    text = Type.text ()
    bytes = Type.bytes ()

    (-->) :: Ord v => Type' v -> Type' v -> Type' v
    a --> b = Type.arrow () a b

    infixr -->

    forall1 :: Var v => Text -> (Type' v -> Type' v) -> Type' v
    forall1 name body =
      let
        a = Var.named name
      in Type.forall () a (body $ Type.var () a)

    forall2 :: Var v => Text -> Text -> (Type' v -> Type' v -> Type' v) -> Type' v
    forall2 name1 name2 body = forall1 name1 (\tv1 -> forall1 name2 (\tv2 -> body tv1 tv2))

    forall4 :: Var v => Text -> Text -> Text -> Text -> (Type' v -> Type' v -> Type' v -> Type' v -> Type' v) -> Type' v
    forall4 name1 name2 name3 name4 body = forall2 name1 name2 (\tv1 tv2 -> forall2 name3 name4 (\tv3 tv4 -> body tv1 tv2 tv3 tv4))

    app :: Ord v => Type' v -> Type' v -> Type' v
    app f a = Type.app () f a

    sequence :: Ord v => Type' v -> Type' v
    sequence arg = Type.vector () `app` arg

    optional :: Ord v => Type' v -> Type' v
    optional arg = DD.optionalType () `app` arg

    effect :: Ord v => Type' v -> Type' v -> Type' v
    effect e a = Type.effectType () `app` e `app` a

    effectful :: Ord v => Type' v -> Type' v -> Type' v
    effectful e a = Type.effect1 () e a

    delayed :: Ord v => Type' v -> Type' v
    delayed a = DD.unitType () --> a
