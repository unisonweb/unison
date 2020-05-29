{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Builtin
  (codeLookup
  ,constructorType
  ,names
  ,names0
  ,builtinDataDecls
  ,builtinEffectDecls
  ,builtinConstructorType
  ,builtinTypeDependents
  ,builtinTermsByType
  ,builtinTermsByTypeMention
  ,intrinsicTermReferences
  ,intrinsicTypeReferences
  ,isBuiltinType
  ,typeLookup
  ,termRefTypes
  ) where

import Unison.Prelude

import           Data.Bifunctor                 ( second, first )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Unison.ConstructorType        as CT
import           Unison.Codebase.CodeLookup     ( CodeLookup(..) )
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                )
import qualified Unison.Builtin.Decls          as DD
import qualified Unison.DataDeclaration        as DD
import           Unison.Parser                  ( Ann(..) )
import qualified Unison.Reference              as R
import qualified Unison.Referent               as Referent
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Type                   as Type
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import Unison.Names3 (Names(Names), Names0)
import qualified Unison.Names3 as Names3
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Util.Relation          as Rel

type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann
type Type v = Type.Type v ()

names :: Names
names = Names names0 mempty

names0 :: Names0
names0 = Names3.names0 terms types where
  terms = Rel.mapRan Referent.Ref (Rel.fromMap termNameRefs) <>
    Rel.fromList [ (Name.fromVar vc, Referent.Con (R.DerivedId r) cid ct)
                 | (ct, (_,(r,decl))) <- ((CT.Data,) <$> builtinDataDecls @Symbol) <>
                    ((CT.Effect,) . (second . second) DD.toDataDecl <$> builtinEffectDecls)
                 , ((_,vc,_), cid) <- DD.constructors' decl `zip` [0..]]
  types = Rel.fromList builtinTypes <>
    Rel.fromList [ (Name.fromVar v, R.DerivedId r)
                 | (v,(r,_)) <- builtinDataDecls @Symbol ] <>
    Rel.fromList [ (Name.fromVar v, R.DerivedId r)
                 | (v,(r,_)) <- builtinEffectDecls @Symbol ]

-- note: this function is really for deciding whether `r` is a term or type,
-- but it can only answer correctly for Builtins.
isBuiltinType :: R.Reference -> Bool
isBuiltinType r = elem r . fmap snd $ builtinTypes

typeLookup :: Var v => TL.TypeLookup v Ann
typeLookup =
  TL.TypeLookup
    (fmap (const Intrinsic) <$> termRefTypes)
    (Map.fromList . map (first R.DerivedId) $ map snd builtinDataDecls)
    (Map.fromList . map (first R.DerivedId) $ map snd builtinEffectDecls)

constructorType :: R.Reference -> Maybe CT.ConstructorType
constructorType r = TL.constructorType (typeLookup @Symbol) r
                <|> Map.lookup r builtinConstructorType

builtinDataDecls :: Var v => [(v, (R.Id, DataDeclaration v))]
builtinDataDecls =
  [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinDataDecls ]

builtinEffectDecls :: Var v => [(v, (R.Id, EffectDeclaration v))]
builtinEffectDecls = [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinEffectDecls ]

codeLookup :: (Applicative m, Var v) => CodeLookup v m Ann
codeLookup = CodeLookup (const $ pure Nothing) $ \r ->
  pure
    $ lookup r [ (r, Right x) | (r, x) <- snd <$> builtinDataDecls ]
  <|> lookup r [ (r, Left x)  | (r, x) <- snd <$> builtinEffectDecls ]

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

-- WARNING:
-- As with the terms, we should avoid changing these references, even
-- if we decide to change their names.
builtinTypes :: [(Name, R.Reference)]
builtinTypes = Map.toList . Map.mapKeys Name.unsafeFromText
                          $ foldl' go mempty builtinTypesSrc where
  go m = \case
    B' r _ -> Map.insert r (R.Builtin r) m
    D' r -> Map.insert r (R.Builtin r) m
    Rename' r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to rename `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                "tried to rename `" <> r <> "` before it was declared."
        Just t -> Map.insert name t . Map.delete r $ m
    Alias' r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to alias `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                  "tried to alias `" <> r <> "` before it was declared."
        Just t -> Map.insert name t m

-- WARNING: Don't delete any of these lines, only add corrections.
builtinTypesSrc :: [BuiltinTypeDSL]
builtinTypesSrc =
  [ B' "Int" CT.Data
  , B' "Nat" CT.Data
  , B' "Float" CT.Data
  , B' "Boolean" CT.Data
  , B' "Sequence" CT.Data, Rename' "Sequence" "List"
  , B' "Text" CT.Data
  , B' "Char" CT.Data
  , B' "Effect" CT.Data, Rename' "Effect" "Request"
  , B' "Bytes" CT.Data
  , B' "Link.Term" CT.Data
  , B' "Link.Type" CT.Data
  ]

-- rename these to "builtin" later, when builtin means intrinsic as opposed to
-- stuff that intrinsics depend on.
intrinsicTypeReferences :: Set R.Reference
intrinsicTypeReferences = foldl' go mempty builtinTypesSrc where
  go acc = \case
    B' r _ -> Set.insert (R.Builtin r) acc
    D' r -> Set.insert (R.Builtin r) acc
    _ -> acc

intrinsicTermReferences :: Set R.Reference
intrinsicTermReferences = Map.keysSet (termRefTypes @Symbol)

builtinConstructorType :: Map R.Reference CT.ConstructorType
builtinConstructorType = Map.fromList [ (R.Builtin r, ct) | B' r ct <- builtinTypesSrc ]

data BuiltinTypeDSL = B' Text CT.ConstructorType | D' Text | Rename' Text Text | Alias' Text Text


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
  , B "Int.and" $ int --> int --> int
  , B "Int.or" $ int --> int --> int
  , B "Int.xor" $ int --> int --> int
  , B "Int.complement" $ int --> int
  , B "Int.increment" $ int --> int
  , B "Int.isEven" $ int --> boolean
  , B "Int.isOdd" $ int --> boolean
  , B "Int.signum" $ int --> int
  , B "Int.leadingZeros" $ int --> nat
  , B "Int.negate" $ int --> int
  , B "Int.negate" $ int --> int
  , B "Int.mod" $ int --> int --> int
  , B "Int.pow" $ int --> nat --> int
  , B "Int.shiftLeft" $ int --> nat --> int
  , B "Int.shiftRight" $ int --> nat --> int
  , B "Int.truncate0" $ int --> nat
  , B "Int.toText" $ int --> text
  , B "Int.fromText" $ text --> optional int
  , B "Int.toFloat" $ int --> float
  , B "Int.trailingZeros" $ int --> nat

  , B "Nat.*" $ nat --> nat --> nat
  , B "Nat.+" $ nat --> nat --> nat
  , B "Nat./" $ nat --> nat --> nat
  , B "Nat.<" $ nat --> nat --> boolean
  , B "Nat.<=" $ nat --> nat --> boolean
  , B "Nat.==" $ nat --> nat --> boolean
  , B "Nat.>" $ nat --> nat --> boolean
  , B "Nat.>=" $ nat --> nat --> boolean
  , B "Nat.and" $ nat --> nat --> nat
  , B "Nat.or" $ nat --> nat --> nat
  , B "Nat.xor" $ nat --> nat --> nat
  , B "Nat.complement" $ nat --> nat
  , B "Nat.drop" $ nat --> nat --> nat
  , B "Nat.fromText" $ text --> optional nat
  , B "Nat.increment" $ nat --> nat
  , B "Nat.isEven" $ nat --> boolean
  , B "Nat.isOdd" $ nat --> boolean
  , B "Nat.leadingZeros" $ nat --> nat
  , B "Nat.mod" $ nat --> nat --> nat
  , B "Nat.pow" $ nat --> nat --> nat
  , B "Nat.shiftLeft" $ nat --> nat --> nat
  , B "Nat.shiftRight" $ nat --> nat --> nat
  , B "Nat.sub" $ nat --> nat --> int
  , B "Nat.toFloat" $ nat --> float
  , B "Nat.toInt" $ nat --> int
  , B "Nat.toText" $ nat --> text
  , B "Nat.trailingZeros" $ nat --> nat

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

  , B "bug" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b))
  , B "todo" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b))

  , B "Boolean.not" $ boolean --> boolean

  , B "Text.empty" text
  , B "Text.++" $ text --> text --> text
  , B "Text.take" $ nat --> text --> text
  , B "Text.drop" $ nat --> text --> text
  , B "Text.size" $ text --> nat
  , B "Text.==" $ text --> text --> boolean
  , D "Text.!=" $ text --> text --> boolean
  , B "Text.<=" $ text --> text --> boolean
  , B "Text.>=" $ text --> text --> boolean
  , B "Text.<" $ text --> text --> boolean
  , B "Text.>" $ text --> text --> boolean
  , B "Text.uncons" $ text --> optional (tuple [char, text])
  , B "Text.unsnoc" $ text --> optional (tuple [text, char])

  , B "Text.toCharList" $ text --> list char
  , B "Text.fromCharList" $ list char --> text

  , B "Char.toNat" $ char --> nat
  , B "Char.fromNat" $ nat --> char

  , B "Bytes.empty" bytes
  , B "Bytes.fromList" $ list nat --> bytes
  , B "Bytes.++" $ bytes --> bytes --> bytes
  , B "Bytes.take" $ nat --> bytes --> bytes
  , B "Bytes.drop" $ nat --> bytes --> bytes
  , B "Bytes.at" $ nat --> bytes --> optional nat
  , B "Bytes.toList" $ bytes --> list nat
  , B "Bytes.size" $ bytes --> nat
  , B "Bytes.flatten" $ bytes --> bytes

  , B "List.empty" $ forall1 "a" list
  , B "List.cons" $ forall1 "a" (\a -> a --> list a --> list a)
  , Alias "List.cons" "List.+:"
  , B "List.snoc" $ forall1 "a" (\a -> list a --> a --> list a)
  , Alias "List.snoc" "List.:+"
  , B "List.take" $ forall1 "a" (\a -> nat --> list a --> list a)
  , B "List.drop" $ forall1 "a" (\a -> nat --> list a --> list a)
  , B "List.++" $ forall1 "a" (\a -> list a --> list a --> list a)
  , B "List.size" $ forall1 "a" (\a -> list a --> nat)
  , B "List.at" $ forall1 "a" (\a -> nat --> list a --> optional a)

  , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
  ] ++
  -- avoid name conflicts with Universal == < > <= >=
  [ Rename (t <> "." <> old) (t <> "." <> new)
  | t <- ["Int", "Nat", "Float", "Text"]
  , (old, new) <- [("==", "eq")
                  ,("<" , "lt")
                  ,("<=", "lteq")
                  ,(">" , "gt")
                  ,(">=", "gteq")]
  ]
  where
    int = Type.int ()
    nat = Type.nat ()
    boolean = Type.boolean ()
    float = Type.float ()
    text = Type.text ()
    bytes = Type.bytes ()
    char = Type.char ()

    (-->) :: Ord v => Type v -> Type v -> Type v
    a --> b = Type.arrow () a b

    infixr -->

    forall1 :: Var v => Text -> (Type v -> Type v) -> Type v
    forall1 name body =
      let
        a = Var.named name
      in Type.forall () a (body $ Type.var () a)

    app :: Ord v => Type v -> Type v -> Type v
    app = Type.app ()

    list :: Ord v => Type v -> Type v
    list arg = Type.vector () `app` arg

    optional :: Ord v => Type v -> Type v
    optional arg = DD.optionalType () `app` arg

    tuple :: Ord v => [Type v] -> Type v
    tuple [t] = t
    tuple ts = foldr pair (DD.unitType ()) ts

    pair :: Ord v => Type v -> Type v -> Type v
    pair l r = DD.pairType () `app` l `app` r

