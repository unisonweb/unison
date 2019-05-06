{-# OPTIONS_GHC -Wwarn #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Builtin2 where

-- import           Control.Arrow                  ( first )
import           Control.Applicative            ( liftA2
                                                -- , (<|>)
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
-- import           Data.Set                       ( Set )
-- import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
-- import qualified Text.Megaparsec.Error         as MPE
-- import qualified Unison.ABT                    as ABT
-- import           Unison.Codebase.CodeLookup     ( CodeLookup(..) )
-- import qualified Unison.ConstructorType        as CT
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                )
-- import qualified Unison.DataDeclaration        as DD
-- import qualified Unison.FileParser             as FileParser
-- import qualified Unison.Lexer                  as L
import           Unison.Parser                  ( Ann(..) )
-- import qualified Unison.Parser                 as Parser
-- import           Unison.PrintError              ( prettyParseError )
import qualified Unison.Reference              as R
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Term                   as Term
-- import qualified Unison.TermParser             as TermParser
import qualified Unison.Type                   as Type
-- import qualified Unison.TypeParser             as TypeParser
-- import qualified Unison.Util.ColorText         as Color
import           Unison.Var                     ( Var )
-- import qualified Unison.Var                    as Var
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
-- import           Unison.Names                   ( Names )
-- import qualified Unison.Names                  as Names
-- import qualified Unison.Typechecker.TypeLookup as TL
-- import qualified Unison.Util.Relation          as Rel

type Term v = Term.AnnotatedTerm v Ann
type Type v = Type.AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann

-- showParseError :: Var v
--                => String
--                -> MPE.ParseError (L.Token L.Lexeme) (Parser.Error v)
--                -> String
-- showParseError s = Color.toANSI . prettyParseError s

parseType :: Var v => String -> Type v
parseType = error "todo" -- is `Names` something we want to keep using?
-- parseType s = ABT.amap (const Intrinsic) .
--           Names.bindType names . either (error . showParseError s) tweak $
--           Parser.run (Parser.root TypeParser.valueType) s mempty
--   where tweak = Type.generalizeLowercase
--
-- -- parse a term, hard-coding the builtins defined in this file
-- tm :: Var v => String -> Term v
-- tm s = Names.bindTerm constructorType names
--        . either (error . showParseError s) id
--        $ Parser.run (Parser.root TermParser.term) s (mempty, names)
--
-- constructorType :: R.Reference -> CT.ConstructorType
-- constructorType r =
--   if any f (builtinDataDecls @Symbol) then CT.Data
--   else if any f (builtinEffectDecls @Symbol) then CT.Effect
--   else error "a builtin term referenced a constructor for a non-builtin type"
--   where f = (==r) . fst . snd
--
-- -- todo: does this need to be refactored if we have mutually recursive decls
-- parseDataDeclAsBuiltin :: Var v => String -> (v, (R.Reference, DataDeclaration v))
-- parseDataDeclAsBuiltin s =
--   let (v, dd) = either (error . showParseError s) id $
--         Parser.run (Parser.root FileParser.dataDeclaration) s mempty
--       [(_, r, dd')] = DD.hashDecls $ Map.singleton v (DD.bindBuiltins names0 dd)
--   in (v, (r, const Intrinsic <$> dd'))
--
-- -- Todo: These definitions and groupings of builtins are getting a little
-- -- confusing.  Sort out these labrinthine definitions!
-- -- We have primitive types and primitive terms, but the types of the
-- -- primitive terms sometimes reference decls, and not just primitive types.
-- -- Primitive types and primitive terms can be deprecated in future iterations
-- -- of the typechecker and runtime, but the builtin decls don't become
-- -- deprecated in the same sense.  So (to do a deprecation check on these)
-- names0 :: Names
-- names0 = Names.fromTypes builtinTypes
--
-- names :: Names
-- names = Names.fromBuiltins (Map.keys $ builtins0 @Symbol) <> allTypeNames
--
-- allTypeNames :: Names
-- allTypeNames =
--   Names.fromTypes builtinTypes
--     <> foldMap (DD.dataDeclToNames' @Symbol)   builtinDataDecls
--     <> foldMap (DD.effectDeclToNames' @Symbol) builtinEffectDecls

-- Is this a term (as opposed to a type)
isBuiltinTerm :: R.Reference -> Bool
isBuiltinTerm r = Map.member r (termRefTypes @Symbol)

isBuiltinType :: R.Reference -> Bool
isBuiltinType r = elem r . fmap snd $ builtinTypes

-- typeLookup :: Var v => TL.TypeLookup v Ann
-- typeLookup =
--   TL.TypeLookup builtins0
--     (Map.fromList $ map snd builtinDataDecls)
--     (Map.fromList $ map snd builtinEffectDecls)
--
-- builtinTypedTerms :: Var v => [(v, (Term v, Type v))]
-- builtinTypedTerms = [(v, (e, t)) | (v, (Term.Ann' e t)) <- builtinTerms ]
--
-- builtinTerms :: Var v => [(v, Term v)]
-- builtinTerms =
--   [ (toSymbol r, Term.ann Intrinsic (Term.ref Intrinsic r) typ) |
--     (r, typ) <- Map.toList builtins0 ]
--
-- builtinTypesV :: Var v => [(v, R.Reference)]
-- builtinTypesV = first (Name.toVar) <$> builtinTypes
--
-- builtinTypeNames :: Set Name
-- builtinTypeNames = Set.fromList (map fst builtinTypes)
--
-- builtinTypes :: [(Name, R.Reference)]
-- builtinTypes = liftA2 (,) Name.unsafeFromText R.Builtin <$>
--   ["Int", "Nat", "Float", "Boolean", "Sequence", "Text", "Effect", "Bytes"]
--
-- -- | parse some builtin data types, and resolve their free variables using
-- -- | builtinTypes' and those types defined herein
-- builtinDataDecls :: Var v => [(v, (R.Reference, DataDeclaration v))]
-- builtinDataDecls =
--   [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinDataDecls ]
--
-- builtinEffectDecls :: Var v => [(v, (R.Reference, EffectDeclaration v))]
-- builtinEffectDecls = []
--
-- codeLookup :: (Applicative m, Var v) => CodeLookup v m Ann
-- codeLookup = CodeLookup (const $ pure Nothing) $ \r ->
--   pure
--     $ lookup r [ (r, Right x) | (R.DerivedId r, x) <- snd <$> builtinDataDecls ]
--     <|> lookup
--           r
--           [ (r, Left x) | (R.DerivedId r, x) <- snd <$> builtinEffectDecls ]
--
-- toSymbol :: Var v => R.Reference -> v
-- toSymbol (R.Builtin txt) = Var.named txt
-- toSymbol _ = error "unpossible"
--
-- -- Relation predicate: Domain depends on range.
-- builtinDependencies :: Rel.Relation R.Reference R.Reference
-- builtinDependencies =
--   Rel.fromMultimap (Type.dependencies <$> builtins0 @Symbol)
--
-- -- The dependents of a builtin type is the set of builtin terms which
-- -- mention that type.
-- builtinTypeDependents :: R.Reference -> Set R.Reference
-- builtinTypeDependents r = Rel.lookupRan r builtinDependencies
--
-- allReferencedTypes :: Set R.Reference
-- allReferencedTypes = Rel.ran builtinDependencies

-- As with the terms, we should try to avoid changing these references, even
-- if we decide to change their names.
builtinTypes :: [(Name, R.Reference)]
builtinTypes = liftA2 (,) Name.unsafeFromText R.Builtin <$>
  ["Int", "Nat", "Float", "Boolean", "Sequence", "Text", "Effect", "Bytes"]

data BuiltinDSL
  -- simple builtin: name=ref, type
  = B Text Text
  -- deprecated builtin: name=ref, type (TBD)
  | D Text Text
  -- rename builtin: refname, newname
  -- must not appear before corresponding B/D
  -- will overwrite newname
  | Rename Text Text
  -- alias builtin: refname, newname
  -- must not appear before corresponding B/D
  -- will overwrite newname
  | Alias Text Text

termNameRefs :: Map Name R.Reference
termNameRefs = Map.mapKeys Name.unsafeFromText $ foldl' go mempty builtinsSrc where
  go m = \case
    B r _tp -> Map.insert r (R.Builtin r) m
    D r _tp -> Map.insert r (R.Builtin r) m
    Rename r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to rename `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                "tried to rename `" <> r <> "` before it was declared."
        Just t -> Map.insert name t . Map.delete r $ m
    Alias r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to alias `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                  "tried to alias `" <> r <> "` before it was declared."
        Just t -> Map.insert name t m

termRefTypes :: Var v => Map R.Reference (Type v)
termRefTypes = foldl' go mempty builtinsSrc where
  go m = \case
    B r t -> Map.insert (R.Builtin r) (parseType (Text.unpack t)) m
    D r t -> Map.insert (R.Builtin r) (parseType (Text.unpack t)) m
    _ -> m

builtinsSrc :: [BuiltinDSL]
builtinsSrc =
  [ B "Int.+" "Int -> Int -> Int"
  , B "Int.-" "Int -> Int -> Int"
  , B "Int.*" "Int -> Int -> Int"
  , B "Int./" "Int -> Int -> Int"
  , B "Int.<" "Int -> Int -> Boolean"
  , B "Int.>" "Int -> Int -> Boolean"
  , B "Int.<=" "Int -> Int -> Boolean"
  , B "Int.>=" "Int -> Int -> Boolean"
  , B "Int.==" "Int -> Int -> Boolean"
  , B "Int.increment" "Int -> Int"
  , B "Int.isEven" "Int -> Boolean"
  , B "Int.isOdd" "Int -> Boolean"
  , B "Int.signum" "Int -> Int"
  , B "Int.negate" "Int -> Int"
  , B "Int.truncate0" "Int -> Nat"

  , B "Nat.+" "Nat -> Nat -> Nat"
  , B "Nat.drop" "Nat -> Nat -> Nat"
  , B "Nat.sub" "Nat -> Nat -> Int"
  , B "Nat.*" "Nat -> Nat -> Nat"
  , B "Nat./" "Nat -> Nat -> Nat"
  , B "Nat.mod" "Nat -> Nat -> Nat"
  , B "Nat.<" "Nat -> Nat -> Boolean"
  , B "Nat.>" "Nat -> Nat -> Boolean"
  , B "Nat.<=" "Nat -> Nat -> Boolean"
  , B "Nat.>=" "Nat -> Nat -> Boolean"
  , B "Nat.==" "Nat -> Nat -> Boolean"
  , B "Nat.increment" "Nat -> Nat"
  , B "Nat.isEven" "Nat -> Boolean"
  , B "Nat.isOdd" "Nat -> Boolean"

  , B "Float.+" "Float -> Float -> Float"
  , B "Float.-" "Float -> Float -> Float"
  , B "Float.*" "Float -> Float -> Float"
  , B "Float./" "Float -> Float -> Float"
  , B "Float.<" "Float -> Float -> Boolean"
  , B "Float.>" "Float -> Float -> Boolean"
  , B "Float.<=" "Float -> Float -> Boolean"
  , B "Float.>=" "Float -> Float -> Boolean"
  , B "Float.==" "Float -> Float -> Boolean"

  -- Trigonmetric Functions
  , B "Float.acos" "Float -> Float"
  , B "Float.asin" "Float -> Float"
  , B "Float.atan" "Float -> Float"
  , B "Float.atan2" "Float -> Float -> Float"
  , B "Float.cos" "Float -> Float"
  , B "Float.sin" "Float -> Float"
  , B "Float.tan" "Float -> Float"

  -- Hyperbolic Functions
  , B "Float.acosh" "Float -> Float"
  , B "Float.asinh" "Float -> Float"
  , B "Float.atanh" "Float -> Float"
  , B "Float.cosh" "Float -> Float"
  , B "Float.sinh" "Float -> Float"
  , B "Float.tanh" "Float -> Float"

  -- Exponential Functions
  , B "Float.exp" "Float -> Float"
  , B "Float.log" "Float -> Float"
  , B "Float.logBase" "Float -> Float -> Float"

  -- Power Functions
  , B "Float.pow" "Float -> Float -> Float"
  , B "Float.sqrt" "Float -> Float"

  -- Rounding and Remainder Functions
  , B "Float.ceiling" "Float -> Int"
  , B "Float.floor" "Float -> Int"
  , B "Float.round" "Float -> Int"
  , B "Float.truncate" "Float -> Int"

  -- Float Utils
  , B "Float.abs" "Float -> Float"
  , B "Float.max" "Float -> Float -> Float"
  , B "Float.min" "Float -> Float -> Float"
  , B "Float.toText" "Float -> Text"
  , B "Float.fromText" "Text -> Optional Float"

  , B "Universal.==" "a -> a -> Boolean"
  -- Don't we want a Universal.!= ?

  -- Universal.compare intended as a low level function that just returns
  -- `Int` rather than some Ordering data type. If we want, later,
  -- could provide a pure Unison wrapper for Universal.compare that
  -- returns a proper data type.
  --
  -- 0 is equal, < 0 is less than, > 0 is greater than
  , B "Universal.compare" "a -> a -> Int"
  , B "Universal.>" "a -> a -> Boolean"
  , B "Universal.<" "a -> a -> Boolean"
  , B "Universal.>=" "a -> a -> Boolean"
  , B "Universal.<=" "a -> a -> Boolean"

  , B "Boolean.not" "Boolean -> Boolean"

  , B "Text.empty" "Text"
  , B "Text.++" "Text -> Text -> Text"
  , B "Text.take" "Nat -> Text -> Text"
  , B "Text.drop" "Nat -> Text -> Text"
  , B "Text.size" "Text -> Nat"
  , B "Text.==" "Text -> Text -> Boolean"
  , B "Text.!=" "Text -> Text -> Boolean"
  , B "Text.<=" "Text -> Text -> Boolean"
  , B "Text.>=" "Text -> Text -> Boolean"
  , B "Text.<" "Text -> Text -> Boolean"
  , B "Text.>" "Text -> Text -> Boolean"

  , B "Bytes.empty" "Bytes"
  , B "Bytes.fromSequence" "[Nat] -> Bytes"
  , B "Bytes.++" "Bytes -> Bytes -> Bytes"
  , B "Bytes.take" "Nat -> Bytes -> Bytes"
  , B "Bytes.drop" "Nat -> Bytes -> Bytes"
  , B "Bytes.at" "Nat -> Bytes -> Optional Nat"
  , B "Bytes.toSequence" "Bytes -> [Nat]"
  , B "Bytes.size" "Bytes -> Nat"
  , B "Bytes.flatten" "Bytes -> Bytes"

  , B "Sequence.empty" "[a]"
  , B "Sequence.cons" "a -> [a] -> [a]"
  , B "Sequence.snoc" "[a] -> a -> [a]"
  , B "Sequence.take" "Nat -> [a] -> [a]"
  , B "Sequence.drop" "Nat -> [a] -> [a]"
  , B "Sequence.++" "[a] -> [a] -> [a]"
  , B "Sequence.size" "[a] -> Nat"
  , B "Sequence.at" "Nat -> [a] -> Optional a"

  , B "Debug.watch" "Text -> a -> a"
  , B "Effect.pure" "a -> Effect e a" -- Effect ambient e a
  , B "Effect.bind" "'{e} a -> (a ->{ambient} b) -> Effect e a" -- Effect ambient e a
  ]
