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
  TL.TypeLookup builtins0
    (Map.fromList $ map snd builtinDataDecls)
    (Map.fromList $ map snd builtinEffectDecls)

builtinTypedTerms :: Var v => [(v, (Term v, Type v))]
builtinTypedTerms = [(v, (e, t)) | (v, (Term.Ann' e t)) <- builtinTerms ]

builtinTerms :: Var v => [(v, Term v)]
builtinTerms =
  [ (toSymbol r, Term.ann Intrinsic (Term.ref Intrinsic r) typ) |
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

builtins0 :: Var v => Map.Map R.Reference (Type v)
builtins0 = Map.fromList $
  [ (R.Builtin name, t typ) |
    (name, typ) <-
      [ ("Int.+", "Int -> Int -> Int")
      , ("Int.-", "Int -> Int -> Int")
      , ("Int.*", "Int -> Int -> Int")
      , ("Int./", "Int -> Int -> Int")
      , ("Int.<", "Int -> Int -> Boolean")
      , ("Int.>", "Int -> Int -> Boolean")
      , ("Int.<=", "Int -> Int -> Boolean")
      , ("Int.>=", "Int -> Int -> Boolean")
      , ("Int.==", "Int -> Int -> Boolean")
      , ("Int.increment", "Int -> Int")
      , ("Int.isEven", "Int -> Boolean")
      , ("Int.isOdd", "Int -> Boolean")
      , ("Int.signum", "Int -> Int")
      , ("Int.negate", "Int -> Int")
      , ("Int.truncate0", "Int -> Nat")

      , ("Nat.+", "Nat -> Nat -> Nat")
      , ("Nat.drop", "Nat -> Nat -> Nat")
      , ("Nat.sub", "Nat -> Nat -> Int")
      , ("Nat.*", "Nat -> Nat -> Nat")
      , ("Nat./", "Nat -> Nat -> Nat")
      , ("Nat.mod", "Nat -> Nat -> Nat")
      , ("Nat.<", "Nat -> Nat -> Boolean")
      , ("Nat.>", "Nat -> Nat -> Boolean")
      , ("Nat.<=", "Nat -> Nat -> Boolean")
      , ("Nat.>=", "Nat -> Nat -> Boolean")
      , ("Nat.==", "Nat -> Nat -> Boolean")
      , ("Nat.increment", "Nat -> Nat")
      , ("Nat.isEven", "Nat -> Boolean")
      , ("Nat.isOdd", "Nat -> Boolean")
      , ("Nat.toInt", "Nat -> Int")
      , ("Nat.toText", "Nat -> Text")

      , ("Float.+", "Float -> Float -> Float")
      , ("Float.-", "Float -> Float -> Float")
      , ("Float.*", "Float -> Float -> Float")
      , ("Float./", "Float -> Float -> Float")
      , ("Float.<", "Float -> Float -> Boolean")
      , ("Float.>", "Float -> Float -> Boolean")
      , ("Float.<=", "Float -> Float -> Boolean")
      , ("Float.>=", "Float -> Float -> Boolean")
      , ("Float.==", "Float -> Float -> Boolean")

      -- Trigonmetric Functions
      , ("Float.acos", "Float -> Float")
      , ("Float.asin", "Float -> Float")
      , ("Float.atan", "Float -> Float")
      , ("Float.atan2", "Float -> Float -> Float")
      , ("Float.cos", "Float -> Float")
      , ("Float.sin", "Float -> Float")
      , ("Float.tan", "Float -> Float")

      -- Hyperbolic Functions
      , ("Float.acosh", "Float -> Float")
      , ("Float.asinh", "Float -> Float")
      , ("Float.atanh", "Float -> Float")
      , ("Float.cosh", "Float -> Float")
      , ("Float.sinh", "Float -> Float")
      , ("Float.tanh", "Float -> Float")

      -- Exponential Functions
      , ("Float.exp", "Float -> Float")
      , ("Float.log", "Float -> Float")
      , ("Float.logBase", "Float -> Float -> Float")

      -- Power Functions
      , ("Float.pow", "Float -> Float -> Float")
      , ("Float.sqrt", "Float -> Float")

      -- Rounding and Remainder Functions
      , ("Float.ceiling", "Float -> Int")
      , ("Float.floor", "Float -> Int")
      , ("Float.round", "Float -> Int")
      , ("Float.truncate", "Float -> Int")

      -- Float Utils
      , ("Float.abs", "Float -> Float")
      , ("Float.max", "Float -> Float -> Float")
      , ("Float.min", "Float -> Float -> Float")
      , ("Float.toText", "Float -> Text")
      , ("Float.fromText", "Text -> Optional Float")

      , ("Universal.==", "a -> a -> Boolean")

      -- Universal.compare intended as a low level function that just returns
      -- `Int` rather than some Ordering data type. If we want, later,
      -- could provide a pure Unison wrapper for Universal.compare that
      -- returns a proper data type.
      --
      -- 0 is equal, < 0 is less than, > 0 is greater than
      , ("Universal.compare", "a -> a -> Int")
      , ("Universal.>", "a -> a -> Boolean")
      , ("Universal.<", "a -> a -> Boolean")
      , ("Universal.>=", "a -> a -> Boolean")
      , ("Universal.<=", "a -> a -> Boolean")

      , ("Boolean.not", "Boolean -> Boolean")

      , ("Text.empty", "Text")
      , ("Text.++", "Text -> Text -> Text")
      , ("Text.take", "Nat -> Text -> Text")
      , ("Text.drop", "Nat -> Text -> Text")
      , ("Text.size", "Text -> Nat")
      , ("Text.==", "Text -> Text -> Boolean")
      , ("Text.!=", "Text -> Text -> Boolean")
      , ("Text.<=", "Text -> Text -> Boolean")
      , ("Text.>=", "Text -> Text -> Boolean")
      , ("Text.<", "Text -> Text -> Boolean")
      , ("Text.>", "Text -> Text -> Boolean")

      , ("Bytes.empty", "Bytes")
      , ("Bytes.fromSequence", "[Nat] -> Bytes")
      , ("Bytes.++", "Bytes -> Bytes -> Bytes")
      , ("Bytes.take", "Nat -> Bytes -> Bytes")
      , ("Bytes.drop", "Nat -> Bytes -> Bytes")
      , ("Bytes.at", "Nat -> Bytes -> Optional Nat")
      , ("Bytes.toSequence", "Bytes -> [Nat]")
      , ("Bytes.size", "Bytes -> Nat")
      , ("Bytes.flatten", "Bytes -> Bytes")

      , ("Sequence.empty", "[a]")
      , ("Sequence.cons", "a -> [a] -> [a]")
      , ("Sequence.snoc", "[a] -> a -> [a]")
      , ("Sequence.take", "Nat -> [a] -> [a]")
      , ("Sequence.drop", "Nat -> [a] -> [a]")
      , ("Sequence.++", "[a] -> [a] -> [a]")
      , ("Sequence.size", "[a] -> Nat")
      , ("Sequence.at", "Nat -> [a] -> Optional a")

      , ("Debug.watch", "Text -> a -> a")
      , ("Effect.pure", "a -> Effect e a") -- Effect ambient e a
      , ("Effect.bind", "'{e} a -> (a ->{ambient} b) -> Effect e a") -- Effect ambient e a
      ]
  ]
