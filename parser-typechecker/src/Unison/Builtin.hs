{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unison.Builtin where

import           Control.Arrow ((&&&), second)
import qualified Data.Map as Map
import qualified Unison.ABT as ABT
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
import qualified Unison.FileParser as FileParser
import           Unison.Parser (Ann(..))
import qualified Unison.Parser as Parser
import           Unison.PrintError (parseErrorToAnsiString)
import qualified Unison.Reference as R
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import           Unison.Var (Var)
import qualified Unison.Var as Var

type Term v = Term.AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann

-- todo: to update these, just inline definition of Parsers.{unsafeParseType, unsafeParseTerm}
-- then merge Parsers back into Parsers (and GC and unused functions)
-- parse a type, hard-coding the builtins defined in this file
t :: Var v => String -> Type v
t s = ABT.amap (const Intrinsic) .
          bindTypeBuiltins . either (error . parseErrorToAnsiString s) tweak $
          Parser.run (Parser.root TypeParser.valueType) s Parser.penv0
  -- lowercase vars become forall'd, and we assume the function is pure up
  -- until it returns its result.
  where tweak = Type.generalizeEffects 100000 . Type.generalizeLowercase

-- parse a term, hard-coding the builtins defined in this file
tm :: Var v => String -> Term v
tm s = bindBuiltins . either (error . parseErrorToAnsiString s) id $
          Parser.run (Parser.root TermParser.term) s Parser.penv0

parseDataDeclAsBuiltin :: Var v => String -> (v, (R.Reference, DataDeclaration v))
parseDataDeclAsBuiltin s =
  let (v, dd) = either (error . parseErrorToAnsiString s) id $
        Parser.run (Parser.root FileParser.dataDeclaration) s Parser.penv0
  in (v, (R.Builtin . Var.qualifiedName $ v,
          const Intrinsic <$>
          DD.bindBuiltins builtinTypes dd))

bindBuiltins :: Var v => Term v -> Term v
bindBuiltins = Term.bindBuiltins builtinTerms builtinTypes

bindTypeBuiltins :: Var v => Type v -> Type v
bindTypeBuiltins = Type.bindBuiltins builtinTypes

builtinTypedTerms :: Var v => [(v, (Term v, Type v))]
builtinTypedTerms = [(v, (e, t)) | (v, e@(Term.Ann' _ t)) <- builtinTerms ]

builtinTerms :: Var v => [(v, Term v)]
builtinTerms =
  let fns = [ (toSymbol r, Term.ann Intrinsic (Term.ref Intrinsic r) typ) |
              (r, typ) <- Map.toList builtins0 ]
  in (builtinDataAndEffectCtors ++ fns)

builtinDataAndEffectCtors :: forall v . Var v => [(v, Term v)]
builtinDataAndEffectCtors = (mkConstructors =<< builtinDataDecls')
  where
    mkConstructors :: (v, (R.Reference, DataDeclaration v)) -> [(v, Term v)]
    mkConstructors (vt, (r, dd)) =
      mkConstructor vt r <$> DD.constructors dd `zip` [0..]
    mkConstructor :: v -> R.Reference -> ((v, Type v), Int) -> (v, Term v)
    mkConstructor vt r ((v, _t), i) =
      (Var.named $ mconcat [Var.qualifiedName vt, ".", Var.qualifiedName v],
        Term.constructor Intrinsic r i)

builtinTypes :: forall v. Var v => [(v, R.Reference)]
builtinTypes = builtinTypes' ++ (f <$> Map.toList (builtinDataDecls @v))
  where f (r@(R.Builtin s), _) = (Var.named s, r)
        f (R.Derived h, _) =
          error $ "expected builtin to be all R.Builtins; " ++
                  "don't know what name to assign to " ++ show h

builtinTypes' :: Var v => [(v, R.Reference)]
builtinTypes' = (Var.named &&& R.Builtin) <$>
  ["Int64", "UInt64", "Float", "Boolean",
    "Sequence", "Text", "Stream", "Effect"]

builtinEffectDecls :: forall v. Var v => Map.Map R.Reference (EffectDeclaration v)
builtinEffectDecls = Map.empty

builtinDataDecls :: forall v. (Var v) => Map.Map R.Reference (DataDeclaration v)
builtinDataDecls = Map.fromList (snd <$> builtinDataDecls')

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls' :: forall v. (Var v) => [(v, (R.Reference, DataDeclaration v))]
builtinDataDecls' = bindAllTheTypes <$> l
  where
    bindAllTheTypes :: (v, (R.Reference, DataDeclaration v)) -> (v, (R.Reference, DataDeclaration v))
    bindAllTheTypes =
      second . second $ (DD.bindBuiltins $ builtinTypes' ++ (dd3ToType <$> l))
    dd3ToType (v, (r, _)) = (v, r)
    l :: [(v, (R.Reference, DataDeclaration v))]
    l = [ (Var.named "()",
            (R.Builtin "()",
             DD.mkDataDecl' Intrinsic [] [(Intrinsic,
                                           Var.named "()",
                                           Type.builtin Intrinsic "()")]))
    -- todo: figure out why `type () = ()` doesn't parse:
    -- l = [ parseDataDeclAsBuiltin "type () = ()"
        -- todo: These should get replaced by hashes,
        --       same as the user-defined data types.
        --       But we still will want a way to associate a name.
        --
        , parseDataDeclAsBuiltin "type Pair a b = Pair a b"
        , parseDataDeclAsBuiltin "type Optional a = None | Some a"
        ]

toSymbol :: Var v => R.Reference -> v
toSymbol (R.Builtin txt) = Var.named txt
toSymbol _ = error "unpossible"

builtins0 :: Var v => Map.Map R.Reference (Type v)
builtins0 = Map.fromList $
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

      , ("Stream.empty", "Stream a")
      , ("Stream.single", "a -> Stream a")
      , ("Stream.constant", "a -> Stream a")
      , ("Stream.from-int64", "Int64 -> Stream Int64")
      , ("Stream.from-uint64", "UInt64 -> Stream UInt64")
      , ("Stream.cons", "a -> Stream a -> Stream a")
      , ("Stream.take", "UInt64 -> Stream a -> Stream a")
      , ("Stream.drop", "UInt64 -> Stream a -> Stream a")
      , ("Stream.take-while", "(a ->{} Boolean) -> Stream a -> Stream a")
      , ("Stream.drop-while", "(a ->{} Boolean) -> Stream a -> Stream a")
      , ("Stream.map", "(a ->{} b) -> Stream a -> Stream b")
      , ("Stream.flat-map", "(a ->{} Stream b) -> Stream a -> Stream b")
      , ("Stream.fold-left", "b -> (b ->{} a ->{} b) -> Stream a -> b")
      , ("Stream.iterate", "a -> (a -> a) -> Stream a")
      , ("Stream.reduce", "a -> (a ->{} a ->{} a) -> Stream a -> a")
      , ("Stream.to-sequence", "Stream a -> Sequence a")
      , ("Stream.filter", "(a ->{} Boolean) -> Stream a -> Stream a")
      , ("Stream.scan-left", "b -> (b ->{} a ->{} b) -> Stream a -> Stream b")
      , ("Stream.sum-int64", "Stream Int64 -> Int64")
      , ("Stream.sum-uint64", "Stream UInt64 -> UInt64")
      , ("Stream.sum-float", "Stream Float -> Float")
      , ("Stream.append", "Stream a -> Stream a -> Stream a")
      , ("Stream.zip-with", "(a ->{} b ->{} c) -> Stream a -> Stream b -> Stream c")
      , ("Stream.unfold", "(a ->{} Optional (b, a)) -> b -> Stream a")

      , ("Sequence.empty", "[a]")
      , ("Sequence.cons", "a -> [a] -> [a]")
      , ("Sequence.snoc", "[a] -> a -> [a]")
      , ("Sequence.take", "UInt64 -> [a] -> [a]")
      , ("Sequence.drop", "UInt64 -> [a] -> [a]")
      , ("Sequence.++", "[a] -> [a] -> [a]")
      , ("Sequence.size", "[a] -> UInt64")
      , ("Sequence.at", "UInt64 -> [a] -> Optional a")

      , ("Debug.watch", "Text -> a -> a")
      ]
  ]
