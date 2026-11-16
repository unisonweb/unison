-- | Prints a runtime value on one line, for programs run without a
-- codebase (unisonrt): its trace output, and the values of its crashes.
--
-- Only data prints in full: numbers, characters, text, bytes, lists,
-- records, and constructors, named as the compiled program's constructor
-- table names them (Unison.Runtime.Standalone). Anything else prints as a
-- placeholder: a function as @<function>@, a foreign value by its type.
-- Nothing here is Unison syntax that parses back; ucm's printer
-- (Unison.Syntax.TermPrinter, by way of decompiling the value) is the one
-- that does that.
module Unison.Runtime.Render
  ( renderValue,
    renderTracer,
  )
where

import Data.Char (intToDigit, isPrint, ord)
import Data.List (intersperse)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Vector qualified as V
import Numeric (showHex)
import Unison.Prelude
import Unison.Reference (Reference, pattern Builtin)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime.ANF (maskTags)
import Unison.Runtime.Array (byteArrayToList)
import Unison.Runtime.Builtin.Decls (builtinConstructorNames, pairRef, unitRef)
import Unison.Runtime.MCode (CombIx (..), shapeFields)
import Unison.Runtime.Machine (Tracer (..))
import Unison.Runtime.Stack
  ( Closure (..),
    Foreign (..),
    Val (..),
    foreignRef,
    pattern BytesC,
    pattern CharVal,
    pattern DataC,
    pattern DoubleVal,
    pattern IntVal,
    pattern ListC,
    pattern NatVal,
    pattern PApV,
    pattern TextC,
  )
import Unison.Runtime.Standalone (ConstructorNames)
import Unison.Runtime.TypeNumbers qualified as TN
import Unison.Runtime.TypeTags qualified as TT
import Unison.ShortHash qualified as SH
import Unison.Util.Bytes qualified as By
import Unison.Util.Text qualified as UText

-- | The tracer a program run without a codebase gives Debug.trace and
-- Debug.toText: the value on one line.
renderTracer :: ConstructorNames -> Bool -> Val -> Tracer
renderTracer names _fancy v = SimpleTrace (Text.unpack (renderValue names v))

renderValue :: ConstructorNames -> Val -> Text
renderValue names = TL.toStrict . TB.toLazyText . value top
  where
    known = names <> Map.fromList builtinConstructorNames

    -- whether the value is an argument, which needs parentheses if it
    -- has spaces in it
    top, arg :: Bool
    top = False
    arg = True

    value :: Bool -> Val -> TB.Builder
    value nested = \case
      CharVal c -> TB.fromString ('?' : charLiteral c)
      NatVal n -> TB.fromString (show n)
      IntVal i -> TB.fromString (if i >= 0 then '+' : show i else show i)
      DoubleVal d -> TB.fromString (show d)
      -- an unboxed value of a type the runtime doesn't number
      Val n (UnboxedTypeTag _) -> TB.fromString (show n)
      Val _ clos -> closure nested clos

    closure :: Bool -> Closure -> TB.Builder
    closure nested = \case
      DataC t []
        | TT.isBooleanTag t -> if maskTags t == 1 then "true" else "false"
      DataC t [b]
        | TT.isAnyTag t -> parens nested ("Any " <> value arg b)
      DataC t vs -> constructor nested (TN.typeReference (TT.typeTag t)) (maskTags t) vs
      RecordC shape vals ->
        "{"
          <> commas
            [ TB.fromText f <> " = " <> value top v
            | (f, v) <- zip (V.toList (shapeFields shape)) (V.toList vals)
            ]
          <> "}"
      PApV (CIx rf _ _) _ _
        | rf == Builtin "jumpCont" -> "<continuation>"
      PApV {} -> "<function>"
      PAp {} -> "<function>"
      BlackHole -> "<exception>"
      Captured {} -> "<continuation>"
      Affine {} -> "<affine>"
      TextC t -> TB.fromString (quote (UText.unpack t))
      BytesC b -> "0xs" <> foldMap hex (By.toWord8s b)
      ListC s -> "[" <> commas (value top <$> toList s) <> "]"
      Foreign f -> foreignValue nested f
      -- the patterns above cover every closure, but aren't marked complete
      _ -> "<unknown>"

    constructor :: Bool -> Reference -> Word64 -> [Val] -> TB.Builder
    constructor nested r c vs
      | r == unitRef, null vs = "()"
      | r == pairRef, c == 0, Just es <- tuple vs = "(" <> commas (value top <$> es) <> ")"
      | null vs = name
      | otherwise = parens nested (mconcat (intersperse " " (name : (value arg <$> vs))))
      where
        name = case Map.lookup r known of
          Just ns
            | (n : _) <- drop (fromIntegral c) ns -> TB.fromText n
          _ -> TB.fromText (short (Reference.toShortHash r)) <> "#" <> TB.fromString (show c)

    -- the elements of a tuple, which is a chain of pairs ending in unit
    tuple :: [Val] -> Maybe [Val]
    tuple [a, rest] = (a :) <$> tupleRest rest
    tuple _ = Nothing
    tupleRest = \case
      Val _ (DataC t [])
        | TN.typeReference (TT.typeTag t) == unitRef -> Just []
      Val _ (DataC t [a, rest])
        | TN.typeReference (TT.typeTag t) == pairRef -> (a :) <$> tupleRest rest
      _ -> Nothing

    foreignValue :: Bool -> Foreign -> TB.Builder
    foreignValue nested = \case
      WrapReferent l -> parens nested ("termLink " <> TB.fromText (short (Referent.toShortHash l)))
      WrapReference l -> parens nested ("typeLink " <> TB.fromText (short (Reference.toShortHash l)))
      WrapArray a -> parens nested ("ImmutableArray.fromList [" <> commas (value top <$> toList a) <> "]")
      WrapByteArray a -> parens nested ("ImmutableByteArray.fromBytes 0xs" <> foldMap hex (byteArrayToList a))
      WrapMap m ->
        parens nested $
          "Map.fromList ["
            <> commas ["(" <> value top k <> ", " <> value top v <> ")" | (k, v) <- Map.toList m]
            <> "]"
      WrapNatural n -> TB.fromString (show n)
      WrapInteger i -> TB.fromString (if i >= 0 then '+' : show i else show i)
      fo -> case foreignRef fo of
        Builtin name -> "<" <> TB.fromText name <> ">"
        r -> "<" <> TB.fromText (short (Reference.toShortHash r)) <> ">"

    parens nested b = if nested then "(" <> b <> ")" else b
    commas = mconcat . intersperse ", "

-- | A hash, abbreviated as ucm abbreviates one it has no name for
short :: SH.ShortHash -> Text
short = SH.toText . SH.shortenTo 8

hex :: Word8 -> TB.Builder
hex w = TB.fromString [intToDigit (fromIntegral (w `div` 16)), intToDigit (fromIntegral (w `mod` 16))]

-- | A character, after the @?@ of a Unison character literal
charLiteral :: Char -> String
charLiteral = \case
  ' ' -> "\\s"
  c -> escape c

-- | Text as a Unison text literal.
quote :: String -> String
quote s = '"' : concatMap esc s ++ "\""
  where
    esc = \case
      '"' -> "\\\""
      c -> escape c

escape :: Char -> String
escape = \case
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\t' -> "\\t"
  '\r' -> "\\r"
  c
    | isPrint c -> [c]
    | otherwise -> "\\x" ++ pad (showHex (ord c) "")
  where
    pad h = replicate (2 - length h) '0' ++ h
