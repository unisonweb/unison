{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unison.Builtin where

import           Control.Arrow ((&&&), second)
import qualified Data.Map as Map
import qualified Text.Megaparsec.Error as MPE
import qualified Unison.ABT as ABT
import           Unison.Names (Name)
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
import qualified Unison.FileParser as FileParser
import qualified Unison.Lexer as L
import           Unison.Parser (Ann(..))
import qualified Unison.Parser as Parser
import           Unison.PrintError (prettyParseError)
import qualified Unison.Reference as R
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Util.ColorText as Color
import           Unison.Var (Var)
import qualified Unison.Var as Var

type Term v = Term.AnnotatedTerm v Ann
type Type v = AnnotatedType v Ann
type DataDeclaration v = DataDeclaration' v Ann
type EffectDeclaration v = EffectDeclaration' v Ann

showParseError :: Var v
               => String
               -> MPE.ParseError (L.Token L.Lexeme) (Parser.Error v)
               -> String
showParseError s = show . Color.renderText . prettyParseError s

-- todo: to update these, just inline definition of Parsers.{unsafeParseType, unsafeParseTerm}
-- then merge Parsers back into Parsers (and GC and unused functions)
-- parse a type, hard-coding the builtins defined in this file
t :: Var v => String -> Type v
t s = ABT.amap (const Intrinsic) .
          bindTypeBuiltins . either (error . showParseError s) tweak $
          Parser.run (Parser.root TypeParser.valueType) s Parser.penv0
  -- lowercase vars become forall'd, and we assume the function is pure up
  -- until it returns its result.
  where tweak = Type.generalizeEffects 100000 . Type.generalizeLowercase

-- parse a term, hard-coding the builtins defined in this file
tm :: Var v => String -> Term v
tm s = bindBuiltins . either (error . showParseError s) id $
          Parser.run (Parser.root TermParser.term) s Parser.penv0

parseDataDeclAsBuiltin :: Var v => String -> (v, (R.Reference, DataDeclaration v))
parseDataDeclAsBuiltin s =
  let (v, dd) = either (error . showParseError s) id $
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

lookupBuiltinTerm :: Var v => Name -> Maybe (Term v)
lookupBuiltinTerm v = lookup (Var.named v) builtinTerms

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
        f (R.Derived h _ _, _) =
          error $ "expected builtin to be all R.Builtins; " ++
                  "don't know what name to assign to " ++ show h
        f r = error $ "what is this " ++ show r

builtinTypes' :: Var v => [(v, R.Reference)]
builtinTypes' = (Var.named &&& R.Builtin) <$>
  ["Int", "Nat", "Float", "Boolean",
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
      , ("Int.is-even", "Int -> Boolean")
      , ("Int.is-odd", "Int -> Boolean")
      , ("Int.signum", "Int -> Int")
      , ("Int.negate", "Int -> Int")

      , ("Nat.+", "Nat -> Nat -> Nat")
      , ("Nat.drop", "Nat -> Nat -> Nat")
      , ("Nat.sub", "Nat -> Nat -> Int")
      , ("Nat.*", "Nat -> Nat -> Nat")
      , ("Nat./", "Nat -> Nat -> Nat")
      , ("Nat.<", "Nat -> Nat -> Boolean")
      , ("Nat.>", "Nat -> Nat -> Boolean")
      , ("Nat.<=", "Nat -> Nat -> Boolean")
      , ("Nat.>=", "Nat -> Nat -> Boolean")
      , ("Nat.==", "Nat -> Nat -> Boolean")
      , ("Nat.increment", "Nat -> Nat")
      , ("Nat.is-even", "Nat -> Boolean")
      , ("Nat.is-odd", "Nat -> Boolean")

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

      , ("Stream.empty", "Stream a")
      , ("Stream.single", "a -> Stream a")
      , ("Stream.constant", "a -> Stream a")
      , ("Stream.from-int", "Int -> Stream Int")
      , ("Stream.from-nat", "Nat -> Stream Nat")
      , ("Stream.cons", "a -> Stream a -> Stream a")
      , ("Stream.take", "Nat -> Stream a -> Stream a")
      , ("Stream.drop", "Nat -> Stream a -> Stream a")
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
      , ("Stream.sum-int", "Stream Int -> Int")
      , ("Stream.sum-nat", "Stream Nat -> Nat")
      , ("Stream.sum-float", "Stream Float -> Float")
      , ("Stream.append", "Stream a -> Stream a -> Stream a")
      , ("Stream.zip-with", "(a ->{} b ->{} c) -> Stream a -> Stream b -> Stream c")
      , ("Stream.unfold", "(a ->{} Optional (b, a)) -> b -> Stream a")

      , ("Sequence.empty", "[a]")
      , ("Sequence.cons", "a -> [a] -> [a]")
      , ("Sequence.snoc", "[a] -> a -> [a]")
      , ("Sequence.take", "Nat -> [a] -> [a]")
      , ("Sequence.drop", "Nat -> [a] -> [a]")
      , ("Sequence.++", "[a] -> [a] -> [a]")
      , ("Sequence.size", "[a] -> Nat")
      , ("Sequence.at", "Nat -> [a] -> Optional a")

      , ("Debug.watch", "Text -> a -> a")
      ]
  ]
