{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
module Unison.Builtin where

import           Control.Arrow ((&&&), second)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Unison.DataDeclaration (DataDeclaration(..))
import qualified Unison.DataDeclaration as DD
import qualified Unison.Parser as Parser
import qualified Unison.Reference as R
import           Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.FileParser as FileParser
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import           Unison.Type (Type)
import qualified Unison.Type as Type
import           Unison.Var (Var)
import qualified Unison.Var as Var

-- todo: to update these, just inline definition of Parsers.{unsafeParseType, unsafeParseTerm}
-- then merge Parsers2 back into Parsers (and GC and unused functions)
-- parse a type, hard-coding the builtins defined in this file
t :: Var v => String -> Type v
t s = bindTypeBuiltins . either error id $
  Parser.run (Parser.root TypeParser.valueType) s TypeParser.s0 Parser.penv0

-- parse a term, hard-coding the builtins defined in this file
tm :: Var v => String -> Term v
tm s = bindBuiltins . either error id $
  Parser.run (Parser.root TermParser.term) s TypeParser.s0 Parser.penv0

parseDataDeclAsBuiltin :: Var v => String -> (R.Reference, DataDeclaration v)
parseDataDeclAsBuiltin s =
  let (v, dd) = either error id $
        Parser.run (Parser.root FileParser.dataDeclaration) s TypeParser.s0 Parser.penv0
  in (R.Builtin . Var.qualifiedName $ v, DD.bindBuiltins builtinTypes dd)

bindBuiltins :: Var v => Term v -> Term v
bindBuiltins = Term.bindBuiltins builtinTerms builtinTypes

bindTypeBuiltins :: Var v => Type v -> Type v
bindTypeBuiltins = Type.bindBuiltins builtinTypes

builtinTerms :: forall v. Var v => [(v, Term v)]
builtinTerms = builtinTerms' ++
    (mkConstructors =<< Map.toList (builtinDataDecls @ v))
  where
    -- (Reference, DataDeclaration v) -> [(v, Term v)]
    mkConstructors (r, dd) =
      mkConstructor r <$> DD.constructors dd `zip` [0..]
    -- Reference -> String -> ((v, Type v), Int) -> (v, Term v)
    mkConstructor r@(R.Builtin s) ((v, _t), i) =
      (Var.named $ mconcat [s, ".", Var.qualifiedName v],
        Term.constructor r i)
    mkConstructor (R.Derived h) ((v, _t), _i) =
      error $ "what kind of name do you want for this one? " ++
                show h ++ "." ++ Text.unpack (Var.qualifiedName v)
-- each dd has a bunch of constructors.  add those constructors as builtinTerms!
  -- where f (R.Builtin s, dd) =

builtinTerms' :: forall v. Var v => [(v, Term v)]
builtinTerms' = (toSymbol &&& Term.ref) <$> Map.keys (builtins @v)


builtinTypes :: forall v. Var v => [(v, Type v)]
builtinTypes = builtinTypes' ++ (f <$> Map.toList (builtinDataDecls @v))
  where f (r@(R.Builtin s), _) = (Var.named s, Type.ref r)
        f (R.Derived h, _) =
          error $ "expected builtinDataDecls to be all R.Builtins; " ++
                  "don't know what name to assign to " ++ show h

builtinTypes' :: Var v => [(v, Type v)]
builtinTypes' = (Var.named &&& (Type.ref . R.Builtin)) <$>
  ["Int64", "UInt64", "Float", "Boolean",
    "Sequence", "Text", "Stream", "Effect"]

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: (Var v) => Map.Map R.Reference (DataDeclaration v)
builtinDataDecls =
  Map.fromList . (bindAllTheTypes <$>) $ l
  where
    bindAllTheTypes =
      second (DD.bindBuiltins $ builtinTypes' ++ (ddPairToType <$> l))
    ddPairToType (r@(R.Builtin s), _) = (Var.named s, Type.ref r)
    ddPairToType _ = error "expected them all to be R.Builtins"
    l = [ (R.Builtin "()", DataDeclaration [] [(Var.named "()", Type.builtin "()")])
        -- todo: these should get replaced by hashes,
        --       same as the user-defined data types.
        --       but we still will want a way to associate a name
        , parseDataDeclAsBuiltin "type Pair a b = Pair a b"
        , parseDataDeclAsBuiltin "type Optional a = None | Some a"
        ]

toSymbol :: Var v => R.Reference -> v
toSymbol (R.Builtin txt) = Var.named txt
toSymbol _ = error "unpossible"

builtins :: Var v => Map.Map R.Reference (Type v)
builtins = Map.fromList $
  [ (R.Builtin name, t typ) |
    (name, typ) <-
      [ ("Int64.+", "Int64 -> Int64 -> Int64")
      , ("Int64.-", "Int64 -> Int64 -> Int64")
      , ("Int64.*", "Int64 -> Int64 -> Int64")
      , ("Int64./", "Int64 -> Int64 -> Int64")
      , ("Int64.<", "Int64 -> Int64 -> Boolean")
      , ("Int64.>", "Int64 -> Int64 -> Boolean")
      , ("Int64.<=", "Int64 -> Int64 -> Boolean")
      , ("Int64.>=", "Int64 -> Int64 -> Boolean")
      , ("Int64.==", "Int64 -> Int64 -> Boolean")
      , ("Int64.increment", "Int64 -> Int64")
      , ("Int64.is-even", "Int64 -> Boolean")
      , ("Int64.is-odd", "Int64 -> Boolean")
      , ("Int64.signum", "Int64 -> Int64")
      , ("Int64.negate", "Int64 -> Int64")

      , ("UInt64.+", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.drop", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.sub", "UInt64 -> UInt64 -> Int64")
      , ("UInt64.*", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64./", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.<", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.<=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.==", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.increment", "UInt64 -> UInt64")
      , ("UInt64.is-even", "UInt64 -> Boolean")
      , ("UInt64.is-odd", "UInt64 -> Boolean")

      , ("Float.+", "Float -> Float -> Float")
      , ("Float.-", "Float -> Float -> Float")
      , ("Float.*", "Float -> Float -> Float")
      , ("Float./", "Float -> Float -> Float")
      , ("Float.<", "Float -> Float -> Boolean")
      , ("Float.>", "Float -> Float -> Boolean")
      , ("Float.<=", "Float -> Float -> Boolean")
      , ("Float.>=", "Float -> Float -> Boolean")
      , ("Float.==", "Float -> Float -> Boolean")

      , ("Boolean.not", "Boolean -> Boolean")

      , ("Text.empty", "Text")
      , ("Text.concatenate", "Text -> Text -> Text")
      , ("Text.take", "UInt64 -> Text -> Text")
      , ("Text.drop", "UInt64 -> Text -> Text")
      , ("Text.size", "Text -> UInt64")
      , ("Text.==", "Text -> Text -> Boolean")
      , ("Text.!=", "Text -> Text -> Boolean")
      , ("Text.<=", "Text -> Text -> Boolean")
      , ("Text.>=", "Text -> Text -> Boolean")
      , ("Text.<", "Text -> Text -> Boolean")
      , ("Text.>", "Text -> Text -> Boolean")

      , ("Stream.empty", "forall a . Stream a")
      , ("Stream.single", "forall a . a -> Stream a")
      , ("Stream.constant", "forall a . a -> Stream a")
      , ("Stream.from-int64", "Int64 -> Stream Int64")
      , ("Stream.from-uint64", "UInt64 -> Stream UInt64")
      , ("Stream.cons", "forall a . a -> Stream a -> Stream a")
      , ("Stream.take", "forall a . UInt64 -> Stream a -> Stream a")
      , ("Stream.drop", "forall a . UInt64 -> Stream a -> Stream a")
      , ("Stream.take-while", "forall a . (a -> Boolean) -> Stream a -> Stream a")
      , ("Stream.drop-while", "forall a . (a -> Boolean) -> Stream a -> Stream a")
      , ("Stream.map", "forall a b . (a -> b) -> Stream a -> Stream b")
      , ("Stream.flat-map", "forall a b . (a -> Stream b) -> Stream a -> Stream b")
      , ("Stream.fold-left", "forall a b . b -> (b -> a -> b) -> Stream a -> b")
      , ("Stream.iterate", "forall a . a -> (a -> a) -> Stream a")
      , ("Stream.reduce", "forall a . a -> (a -> a -> a) -> Stream a -> a")
      , ("Stream.to-sequence", "forall a . Stream a -> Sequence a")
      , ("Stream.filter", "forall a . (a -> Boolean) -> Stream a -> Stream a")
      , ("Stream.scan-left", "forall a b . b -> (b -> a -> b) -> Stream a -> Stream b")
      , ("Stream.sum-int64", "Stream Int64 -> Int64")
      , ("Stream.sum-uint64", "Stream UInt64 -> UInt64")
      , ("Stream.sum-float", "Stream Float -> Float")
      , ("Stream.append", "forall a . Stream a -> Stream a -> Stream a")
      , ("Stream.zip-with", "forall a b c . (a -> b -> c) -> Stream a -> Stream b -> Stream c")
      , ("Stream.unfold", "forall a b . (a -> Optional (b, a)) -> b -> Stream a")

      , ("Sequence.empty", "forall a . Sequence a")
      , ("Sequence.cons", "forall a . a -> Sequence a -> Sequence a")
      , ("Sequence.snoc", "forall a . Sequence a -> a -> Sequence a")
      , ("Sequence.take", "forall a . UInt64 -> Sequence a -> Sequence a")
      , ("Sequence.size", "forall a . Sequence a -> UInt64")
      ]
  ]
