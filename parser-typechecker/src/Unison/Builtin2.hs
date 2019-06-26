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
import           Data.Bifunctor                 ( second )
import           Data.Foldable                  ( foldl', toList )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
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
import qualified Unison.DataDeclaration        as DD
-- import qualified Unison.FileParser             as FileParser
-- import qualified Unison.Lexer                  as L
import           Unison.Parser                  ( Ann(..) )
-- import qualified Unison.Parser                 as Parser
-- import           Unison.PrintError              ( prettyParseError )
import qualified Unison.Reference              as R
import qualified Unison.Referent               as Referent
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Term                   as Term
-- import qualified Unison.TermParser             as TermParser
import           Unison.Type                    ( Type )
import qualified Unison.Type                   as Type
-- import qualified Unison.TypeParser             as TypeParser
-- import qualified Unison.Util.ColorText         as Color
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import Unison.Names2 (Names'(Names), Names0)
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Util.Relation          as Rel

type Term v = Term.AnnotatedTerm v Ann
-- type Type v = Type.AnnotatedType v Ann
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

names0 :: Names0
names0 = Names terms types where
  terms = Rel.mapRan Referent.Ref (Rel.fromMap termNameRefs) <>
    Rel.fromList [ (Name.fromVar vc, Referent.Con r cid)
                 | (_,(r,decl)) <- builtinDataDecls @Symbol <>
                    ((second . second) DD.toDataDecl <$> builtinEffectDecls)
                 , ((_,vc,_), cid) <- DD.constructors' decl `zip` [0..]]
  types = Rel.fromList builtinTypes <>
    Rel.fromList [ (Name.fromVar v, r) | (v,(r,_)) <- builtinDataDecls @Symbol ] <>
    Rel.fromList [ (Name.fromVar v, r) | (v,(r,_)) <- builtinEffectDecls @Symbol ]

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

typeLookup :: Var v => TL.TypeLookup v Ann
typeLookup =
  TL.TypeLookup
    (fmap (const Intrinsic) <$> termRefTypes)
    (Map.fromList $ map snd builtinDataDecls)
    (Map.fromList $ map snd builtinEffectDecls)

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

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: Var v => [(v, (R.Reference, DataDeclaration v))]
builtinDataDecls =
  [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinDataDecls ]

builtinEffectDecls :: Var v => [(v, (R.Reference, EffectDeclaration v))]
builtinEffectDecls = []

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

-- Relation predicate: Domain depends on range.
builtinDependencies :: Rel.Relation R.Reference R.Reference
builtinDependencies =
  Rel.fromMultimap (Type.dependencies <$> termRefTypes @Symbol)

-- a relation whose domain is types and whose range is builtin terms with that type
builtinTermsByType :: Rel.Relation R.Reference Referent.Referent
builtinTermsByType =
  Rel.fromList [ (Type.toReference ty, Referent.Ref r)
               | (r, ty) <- Map.toList (termRefTypes @Symbol) ]

-- a relation whose domain is types and whose range is builtin terms that mention that type
-- example: Nat.+ mentions the type `Nat`
builtinTermsByTypeMention :: Rel.Relation R.Reference Referent.Referent
builtinTermsByTypeMention =
  Rel.fromList [ (m, Referent.Ref r) | (r, ty) <- Map.toList (termRefTypes @Symbol)
                                     , m <- toList $ Type.toReferenceMentions ty ]

-- The dependents of a builtin type is the set of builtin terms which
-- mention that type.
builtinTypeDependents :: R.Reference -> Set R.Reference
builtinTypeDependents r = Rel.lookupRan r builtinDependencies

-- allReferencedTypes :: Set R.Reference
-- allReferencedTypes = Rel.ran builtinDependencies

-- As with the terms, we should try to avoid changing these references, even
-- if we decide to change their names.
builtinTypes :: [(Name, R.Reference)]
builtinTypes = liftA2 (,) Name.unsafeFromText R.Builtin <$>
  ["Int", "Nat", "Float", "Boolean", "Sequence", "Text", "Effect", "Bytes"]

data BuiltinDSL v
  -- simple builtin: name=ref, type
  = B Text (Type v)
  -- deprecated builtin: name=ref, type (TBD)
  | D Text (Type v)
  -- rename builtin: refname, newname
  -- must not appear before corresponding B/D
  -- will overwrite newname
  | Rename Text Text
  -- alias builtin: refname, newname
  -- must not appear before corresponding B/D
  -- will overwrite newname
  | Alias Text Text

termNameRefs :: Map Name R.Reference
termNameRefs = Map.mapKeys Name.unsafeFromText $ foldl' go mempty (builtinsSrc @Symbol) where
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
    B r t -> Map.insert (R.Builtin r) t m
    D r t -> Map.insert (R.Builtin r) t m
    _ -> m

builtinsSrc :: Var v => [BuiltinDSL v]
builtinsSrc =
  [ B "Int.+" $ int --> int --> int
  , B "Int.-" $ int --> int --> int
  , B "Int.*" $ int --> int --> int
  , B "Int./" $ int --> int --> int
  , B "Int.<" $ int --> int --> boolean
  , B "Int.>" $ int --> int --> boolean
  , B "Int.<=" $ int --> int --> boolean
  , B "Int.>=" $ int --> int --> boolean
  , B "Int.==" $ int --> int --> boolean
  , B "Int.increment" $ int --> int
  , B "Int.isEven" $ int --> boolean
  , B "Int.isOdd" $ int --> boolean
  , B "Int.signum" $ int --> int
  , B "Int.negate" $ int --> int
  , B "Int.truncate0" $ int --> nat

  , B "Nat.+" $ nat --> nat --> nat
  , B "Nat.drop" $ nat --> nat --> nat
  , B "Nat.sub" $ nat --> nat --> int
  , B "Nat.*" $ nat --> nat --> nat
  , B "Nat./" $ nat --> nat --> nat
  , B "Nat.mod" $ nat --> nat --> nat
  , B "Nat.<" $ nat --> nat --> boolean
  , B "Nat.>" $ nat --> nat --> boolean
  , B "Nat.<=" $ nat --> nat --> boolean
  , B "Nat.>=" $ nat --> nat --> boolean
  , B "Nat.==" $ nat --> nat --> boolean
  , B "Nat.increment" $ nat --> nat
  , B "Nat.isEven" $ nat --> boolean
  , B "Nat.isOdd" $ nat --> boolean

  , B "Float.+" $ float --> float --> float
  , B "Float.-" $ float --> float --> float
  , B "Float.*" $ float --> float --> float
  , B "Float./" $ float --> float --> float
  , B "Float.<" $ float --> float --> boolean
  , B "Float.>" $ float --> float --> boolean
  , B "Float.<=" $ float --> float --> boolean
  , B "Float.>=" $ float --> float --> boolean
  , B "Float.==" $ float --> float --> boolean

  -- Trigonmetric Functions
  , B "Float.acos" $ float --> float
  , B "Float.asin" $ float --> float
  , B "Float.atan" $ float --> float
  , B "Float.atan2" $ float --> float --> float
  , B "Float.cos" $ float --> float
  , B "Float.sin" $ float --> float
  , B "Float.tan" $ float --> float

  -- Hyperbolic Functions
  , B "Float.acosh" $ float --> float
  , B "Float.asinh" $ float --> float
  , B "Float.atanh" $ float --> float
  , B "Float.cosh" $ float --> float
  , B "Float.sinh" $ float --> float
  , B "Float.tanh" $ float --> float

  -- Exponential Functions
  , B "Float.exp" $ float --> float
  , B "Float.log" $ float --> float
  , B "Float.logBase" $ float --> float --> float

  -- Power Functions
  , B "Float.pow" $ float --> float --> float
  , B "Float.sqrt" $ float --> float

  -- Rounding and Remainder Functions
  , B "Float.ceiling" $ float --> int
  , B "Float.floor" $ float --> int
  , B "Float.round" $ float --> int
  , B "Float.truncate" $ float --> int

  -- Float Utils
  , B "Float.abs" $ float --> float
  , B "Float.max" $ float --> float --> float
  , B "Float.min" $ float --> float --> float
  , B "Float.toText" $ float --> text
  , B "Float.fromText" $ text --> optional float

  , B "Universal.==" $ forall1 "a" (\a -> a --> a --> boolean)
  -- Don't we want a Universal.!= ?

  -- Universal.compare intended as a low level function that just returns
  -- `Int` rather than some Ordering data type. If we want, later,
  -- could provide a pure Unison wrapper for Universal.compare that
  -- returns a proper data type.
  --
  -- 0 is equal, < 0 is less than, > 0 is greater than
  , B "Universal.compare" $ forall1 "a" (\a -> a --> a --> int)
  , B "Universal.>" $ forall1 "a" (\a -> a --> a --> boolean)
  , B "Universal.<" $ forall1 "a" (\a -> a --> a --> boolean)
  , B "Universal.>=" $ forall1 "a" (\a -> a --> a --> boolean)
  , B "Universal.<=" $ forall1 "a" (\a -> a --> a --> boolean)

  , B "Boolean.not" $ boolean --> boolean

  , B "Text.empty" $ text
  , B "Text.++" $ text --> text --> text
  , B "Text.take" $ nat --> text --> text
  , B "Text.drop" $ nat --> text --> text
  , B "Text.size" $ text --> nat
  , B "Text.==" $ text --> text --> boolean
  , B "Text.!=" $ text --> text --> boolean
  , B "Text.<=" $ text --> text --> boolean
  , B "Text.>=" $ text --> text --> boolean
  , B "Text.<" $ text --> text --> boolean
  , B "Text.>" $ text --> text --> boolean

  , B "Bytes.empty" $ bytes
  , B "Bytes.fromSequence" $ sequence nat --> bytes
  , B "Bytes.++" $ bytes --> bytes --> bytes
  , B "Bytes.take" $ nat --> bytes --> bytes
  , B "Bytes.drop" $ nat --> bytes --> bytes
  , B "Bytes.at" $ nat --> bytes --> optional nat
  , B "Bytes.toSequence" $ bytes --> sequence nat
  , B "Bytes.size" $ bytes --> nat
  , B "Bytes.flatten" $ bytes --> bytes

  , B "Sequence.empty" $ forall1 "a" (\a -> sequence a)
  , B "Sequence.cons" $ forall1 "a" (\a -> a --> sequence a --> sequence a)
  , B "Sequence.snoc" $ forall1 "a" (\a -> sequence a --> a --> sequence a)
  , B "Sequence.take" $ forall1 "a" (\a -> nat --> sequence a --> sequence a)
  , B "Sequence.drop" $ forall1 "a" (\a -> nat --> sequence a --> sequence a)
  , B "Sequence.++" $ forall1 "a" (\a -> sequence a --> sequence a --> sequence a)
  , B "Sequence.size" $ forall1 "a" (\a -> sequence a --> nat)
  , B "Sequence.at" $ forall1 "a" (\a -> nat --> sequence a --> optional a)

  , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
  , B "Effect.pure" $ forall2 "e" "a" (\e a -> a --> effect e a) -- Effect ambient e a
  , B "Effect.bind" $ forall4 "e" "a" "ambient" "b" (\e a ambient b -> delayed (effectful e a) --> (a --> effectful ambient b) --> effect e a) -- Effect ambient e a
  ]
  where
    int = Type.int ()
    nat = Type.nat ()
    boolean = Type.boolean ()
    float = Type.float ()
    text = Type.text ()
    bytes = Type.bytes ()

    (-->) :: Ord v => Type v -> Type v -> Type v
    a --> b = Type.arrow () a b

    infixr -->

    forall1 :: Var v => Text -> (Type v -> Type v) -> Type v
    forall1 name body =
      let
        a = Var.named name
      in Type.forall () a (body $ Type.var () a)

    forall2 :: Var v => Text -> Text -> (Type v -> Type v -> Type v) -> Type v
    forall2 name1 name2 body = forall1 name1 (\tv1 -> forall1 name2 (\tv2 -> body tv1 tv2))

    forall4 :: Var v => Text -> Text -> Text -> Text -> (Type v -> Type v -> Type v -> Type v -> Type v) -> Type v
    forall4 name1 name2 name3 name4 body = forall2 name1 name2 (\tv1 tv2 -> forall2 name3 name4 (\tv3 tv4 -> body tv1 tv2 tv3 tv4))

    app :: Ord v => Type v -> Type v -> Type v
    app f a = Type.app () f a

    sequence :: Ord v => Type v -> Type v
    sequence arg = Type.vector () `app` arg

    optional :: Ord v => Type v -> Type v
    optional arg = DD.optionalType () `app` arg

    effect :: Ord v => Type v -> Type v -> Type v
    effect e a = Type.effectType () `app` e `app` a

    effectful :: Ord v => Type v -> Type v -> Type v
    effectful e a = Type.effect1 () e a

    delayed :: Ord v => Type v -> Type v
    delayed a = DD.unitType () --> a
