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
import qualified Unison.FileParser             as FileParser
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
  -- lowercase vars become forall'd, and we assume the function is pure up
  -- until it returns its result.
  where tweak = Type.generalizeEffects 100000 . Type.generalizeLowercase

-- parse a term, hard-coding the builtins defined in this file
tm :: Var v => String -> Term v
tm s = Names.bindTerm constructorType names
       . either (error . showParseError s) id
       $ Parser.run (Parser.root TermParser.term) s names

constructorType :: R.Reference -> CT.ConstructorType
constructorType r =
  if any f (builtinDataDecls @Symbol) then CT.Data
  else if any f (builtinEffectDecls @Symbol) then CT.Effect
  else error "a builtin term referenced a constructor for a non-builtin type"
  where f = (==r) . fst . snd

parseDataDeclAsBuiltin :: Var v => String -> (v, (R.Reference, DataDeclaration v))
parseDataDeclAsBuiltin s =
  let (v, dd) = either (error . showParseError s) id $
        Parser.run (Parser.root FileParser.dataDeclaration) s mempty
      [(_, r, dd')] = DD.hashDecls $ Map.singleton v (DD.bindBuiltins names0 dd)
  in (v, (r, const Intrinsic <$> dd'))

names0 :: Names
names0 = Names.fromTypes builtinTypes

names :: Names
names = Names.fromBuiltins (Map.keys $ builtins0 @Symbol) <> allTypeNames

allTypeNames :: Names
allTypeNames =
  Names.fromTypes builtinTypes
    <> foldMap (DD.dataDeclToNames' @Symbol)   builtinDataDecls
    <> foldMap (DD.effectDeclToNames' @Symbol) builtinEffectDecls

isBuiltinTerm :: Name -> Bool
isBuiltinTerm n = Map.member n $ Names.termNames names

isBuiltinType :: Name -> Bool
isBuiltinType n = Map.member n $ Names.typeNames names

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
  ["Int", "Nat", "Float", "Boolean", "Sequence", "Text", "Stream", "Effect"]

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: Var v => [(v, (R.Reference, DataDeclaration v))]
builtinDataDecls = l
  where
    l = [ (Var.named "()",
            (Type.unitRef,
             DD.mkDataDecl' Intrinsic [] [(Intrinsic,
                                           Var.named "()",
                                           Type.unit Intrinsic)]))
    -- todo: figure out why `type () = ()` doesn't parse:
    -- l = [ parseDataDeclAsBuiltin "type () = ()"
        , parseDataDeclAsBuiltin "type Pair a b = Pair a b"
        , parseDataDeclAsBuiltin "type Optional a = None | Some a"
        ]

builtinEffectDecls :: Var v => [(v, (R.Reference, EffectDeclaration v))]
builtinEffectDecls = []

codeLookup :: (Applicative m, Var v) => CodeLookup m v Ann
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

      , ("Float.+", "Float -> Float -> Float")
      , ("Float.-", "Float -> Float -> Float")
      , ("Float.*", "Float -> Float -> Float")
      , ("Float./", "Float -> Float -> Float")
      , ("Float.<", "Float -> Float -> Boolean")
      , ("Float.>", "Float -> Float -> Boolean")
      , ("Float.<=", "Float -> Float -> Boolean")
      , ("Float.>=", "Float -> Float -> Boolean")
      , ("Float.==", "Float -> Float -> Boolean")
      , ("Float.floor", "Float -> Int")

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
