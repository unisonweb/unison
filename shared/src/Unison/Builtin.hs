{-# LANGUAGE OverloadedStrings #-}
module Unison.Builtin where

import Data.List
import Data.Text (Text)
import Unison.Metadata (Metadata(..))
import Unison.Parsers (unsafeParseType)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker.Context (remoteSignatureOf)
import Unison.Util.Logger (Logger)
import Unison.Var (Var)
import qualified Data.Char as Char
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Interpreter as I
import qualified Unison.Metadata as Metadata
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Remote as Remote
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import qualified Unison.View as View
import qualified Unison.Util.Logger as L

type DFO = View.DFO
type V = Symbol DFO

data Builtin v =
  Builtin { reference :: R.Reference
          , op :: Maybe (I.Primop (N.Noted IO) v)
          , bType :: Type v
          , metadata :: Metadata v R.Reference
          }

unitRef :: Ord v => Term v
unitRef = Term.ref (R.Builtin "()")
true, false :: Ord v => Term v
true = Term.builtin "True"
false = Term.builtin "False"
pair :: Ord v => Term v
pair = Term.builtin "Pair"
pair' :: Ord v => Term v -> Term v -> Term v
pair' t1 t2 = pair `Term.app` t1 `Term.app` (pair `Term.app` t2 `Term.app` unitRef)

makeBuiltins :: (Show v, Var v) => Logger -> [Builtin v]
makeBuiltins logger =
  let
    numeric2 :: Var v => (Double -> Double -> Double) -> I.Primop (N.Noted IO) v
    numeric2 f = I.Primop 2 $ \xs -> case xs of
      [Term.Number' x, Term.Number' y] -> pure (Term.num $ f x y)
      _ -> error "unpossible"
    numericCompare :: Var v => (Double -> Double -> Bool) -> I.Primop (N.Noted IO) v
    numericCompare f = I.Primop 2 $ \xs -> case xs of
      [Term.Number' x, Term.Number' y] -> case f x y of
        False -> pure false
        True -> pure true
      _ -> error "unpossible"
    string2 :: Var v => (Text -> Text -> Text) -> I.Primop (N.Noted IO) v
    string2 f = I.Primop 2 $ \xs -> case xs of
      [Term.Text' x, Term.Text' y] -> pure (Term.text $ f x y)
      _ -> error "unpossible"
    string2' :: Var v => (Text -> Text -> Bool) -> I.Primop (N.Noted IO) v
    string2' f = I.Primop 2 $ \xs -> case xs of
      [Term.Text' x, Term.Text' y] -> case f x y of
        False -> pure false
        True -> pure true
      _ -> error "unpossible"
  in map (\(r, o, t, m) -> Builtin r o t m)
     [ -- Unit type
       let r = R.Builtin "()"
       in (r, Nothing, unitT, prefix "()")
     , let r = R.Builtin "Unit.Order"
       in (r, Nothing, unsafeParseType "Order Unit", prefix "Unit.Order")

     -- debugging printlns
     , let r = R.Builtin "Debug.log";
           op [Term.Text' msg,logged,a] = do
             N.lift $ L.error logger (Text.unpack msg ++ ": " ++ show logged)
             pure a
           op _ = error "unpossible"
           typ = "∀ a b . Text -> a -> b -> b"
       in (r, Just (I.Primop 3 op), unsafeParseType typ, prefix "Debug.log")

     , let r = R.Builtin "Debug.watch";
           op [Term.Text' msg, a] = do
             N.lift $ L.error logger (Text.unpack msg ++ ": " ++ show a)
             pure a
           op _ = error "unpossible"
           typ = "∀ a . Text -> a -> a"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Debug.watch")

     -- Boolean
     , let r = R.Builtin "True"
       in (r, Nothing, Type.builtin "Boolean", prefix "True")
     , let r = R.Builtin "False";
       in (r, Nothing, Type.builtin "Boolean", prefix "False")
     , let r = R.Builtin "Boolean.and";
           op [Term.Builtin' b1, Term.Builtin' b2] =
             pure $ case (b1,b2) of
               _ | Text.head b1 /= Text.head b2 -> false
                 | otherwise -> if Text.head b1 == 'T' then true else false
           op _ = error "unpossible"
           typ = "Boolean -> Boolean -> Boolean"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "and")
     , let r = R.Builtin "Boolean.or";
           op [Term.Builtin' b1,Term.Builtin' b2] =
             pure $ case (b1,b2) of
               _ | Text.head b1 /= Text.head b2 -> true
                 | otherwise -> if Text.head b1 == 'F' then false else true
           op _ = error "unpossible"
           typ = "Boolean -> Boolean -> Boolean"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "or")
     , let r = R.Builtin "Boolean.not";
           op [Term.Builtin' b1] =
             pure $ if Text.head b1 == 'T' then false else true
           op _ = error "unpossible"
           typ = "Boolean -> Boolean"
       in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "not")

     -- Number
     , let r = R.Builtin "Number.+"
       in (r, Just (numeric2 (+)), unsafeParseType "Number -> Number -> Number", prefix "+")
     , let r = R.Builtin "Number.-"
       in (r, Just (numeric2 (-)), unsafeParseType "Number -> Number -> Number", prefix "-")
     , let r = R.Builtin "Number.*"
       in (r, Just (numeric2 (*)), unsafeParseType "Number -> Number -> Number", prefix "*")
     , let r = R.Builtin "Number./"
       in (r, Just (numeric2 (/)), unsafeParseType "Number -> Number -> Number", prefix "/")
     , let r = R.Builtin "Number.>"
       in (r, Just (numericCompare (>)), unsafeParseType "Number -> Number -> Boolean", prefix "Number.>")
     , let r = R.Builtin "Number.<"
       in (r, Just (numericCompare (<)), unsafeParseType "Number -> Number -> Boolean", prefix "Number.<")
     , let r = R.Builtin "Number.>="
       in (r, Just (numericCompare (>=)), unsafeParseType "Number -> Number -> Boolean", prefix "Number.>=")
     , let r = R.Builtin "Number.<="
       in (r, Just (numericCompare (<=)), unsafeParseType "Number -> Number -> Boolean", prefix "Number.<=")
     , let r = R.Builtin "Number.=="
       in (r, Just (numericCompare (==)), unsafeParseType "Number -> Number -> Boolean", prefix "Number.==")
     , let r = R.Builtin "Number.Order"
       in (r, Nothing, unsafeParseType "Order Number", prefix "Number.Order")

     -- Duration
     , let r = R.Builtin "Duration.seconds"
           op [Term.Number' n] = pure $ Term.num n
           op _ = fail "Duration.seconds unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Number -> Duration", prefix "Duration.seconds")

     -- Remote
     , let r = R.Builtin "Remote.sleep"
           op [Term.Number' seconds] =
             let s = Remote.Seconds seconds
             in pure $ Term.remote (Remote.Step (Remote.Local (Remote.Sleep s)))
           op _ = fail "Remote.sleep unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "Duration -> Remote Unit", prefix "Remote.sleep")
     , let r = R.Builtin "Remote.at"
           op [Term.Distributed' (Term.Node node), term] =
             pure $ Term.remote (Remote.Step (Remote.At node term))
           op _ = fail "Remote.at unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.at", prefix "Remote.at")
     , let r = R.Builtin "Remote.here"
           op [] = pure $ Term.remote (Remote.Step (Remote.Local Remote.Here))
           op _ = fail "Remote.here unpossible"
       in (r, Just (I.Primop 0 op), remoteSignatureOf "Remote.here", prefix "Remote.here")
     , let r = R.Builtin "Remote.spawn"
           op [] = pure $ Term.remote (Remote.Step (Remote.Local Remote.Spawn))
           op _ = fail "Remote.spawn unpossible"
       in (r, Just (I.Primop 0 op), remoteSignatureOf "Remote.spawn", prefix "Remote.spawn")
     , let r = R.Builtin "Remote.send"
           op [Term.Distributed' (Term.Channel c), v] =
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.Send c v)))
           op _ = fail "Remote.send unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.send", prefix "Remote.send")
     , let r = R.Builtin "Remote.channel"
           op [] = pure $ Term.remote (Remote.Step (Remote.Local Remote.CreateChannel))
           op _ = fail "Remote.channel unpossible"
       in (r, Just (I.Primop 0 op), remoteSignatureOf "Remote.channel", prefix "Remote.channel")
    , let r = R.Builtin "Remote.bind"
          op [g, r] = do
            -- right associate the binds so that there is always a Step on the outside
            let kcomp f g = Term.lam' ["x"] $ Term.builtin "Remote.bind" `Term.apps` [g, f `Term.app` Term.var' "x"]
            case r of
              Term.Distributed' (Term.Remote (Remote.Step s)) -> pure $ Term.remote (Remote.Bind s g)
              Term.Distributed' (Term.Remote (Remote.Bind s f)) -> pure $ Term.remote (Remote.Bind s (kcomp f g))
              _ -> fail $ "Remote.bind given a value that was not a Remote: " ++ show r
                        ++ " "
                        ++ show (ABT.freeVars r)
          op _ = fail "Remote.bind unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.bind", prefix "Remote.bind")
     , let r = R.Builtin "Remote.pure"
           op [a] = pure $ Term.remote (Remote.Step (Remote.Local (Remote.Pure a)))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 1 op), remoteSignatureOf "Remote.pure", prefix "Remote.pure")
     , let r = R.Builtin "Remote.map"
           op [f, r] = pure $ Term.builtin "Remote.bind" `Term.app`
             (Term.lam' ["x"] $ Term.remote
               (Remote.Step . Remote.Local . Remote.Pure $ f `Term.app` Term.var' "x"))
             `Term.app` r
           op _ = fail "unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.map", prefix "Remote.map")
     , let r = R.Builtin "Remote.receive-async"
           op [Term.Distributed' (Term.Channel chan), Term.Number' seconds] =
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.ReceiveAsync chan (Remote.Seconds seconds))))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.receive-async", prefix "Remote.receive-async")
     , let r = R.Builtin "Remote.receive"
           op [Term.Distributed' (Term.Channel chan)] =
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.Receive chan)))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 1 op), remoteSignatureOf "Remote.receive", prefix "Remote.receive")
     , let r = R.Builtin "Remote.fork"
           op [Term.Distributed' (Term.Remote r)] =
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.Fork r)))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 1 op), remoteSignatureOf "Remote.fork", prefix "Remote.fork")

     -- Text
     , let r = R.Builtin "Text.concatenate"
       in (r, Just (string2 mappend), unsafeParseType "Text -> Text -> Text", prefix "Text.concatenate")
     , let r = R.Builtin "Text.=="
       in (r, Just (string2' (==)), unsafeParseType "Text -> Text -> Boolean", prefix "Text.==")
     , let r = R.Builtin "Text.<"
       in (r, Just (string2' (<)), unsafeParseType "Text -> Text -> Boolean", prefix "Text.<")
     , let r = R.Builtin "Text.<="
       in (r, Just (string2' (<=)), unsafeParseType "Text -> Text -> Boolean", prefix "Text.<=")
     , let r = R.Builtin "Text.>"
       in (r, Just (string2' (>)), unsafeParseType "Text -> Text -> Boolean", prefix "Text.>")
     , let r = R.Builtin "Text.>="
       in (r, Just (string2' (>=)), unsafeParseType "Text -> Text -> Boolean", prefix "Text.>=")
     , let r = R.Builtin "Text.Order"
       in (r, Nothing, unsafeParseType "Order Text", prefix "Text.Order")
     , let r = R.Builtin "Text.lowercase"
           op [Term.Text' txt] = pure $ Term.text (Text.toLower txt)
           op _ = error "Text.lowercase unpossible"
           typ = "Text -> Text"
       in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Text.lowercase")
     , let r = R.Builtin "Text.uppercase"
           op [Term.Text' txt] = pure $ Term.text (Text.toUpper txt)
           op _ = error "Text.uppercase unpossible"
           typ = "Text -> Text"
       in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Text.lowercase")
     , let r = R.Builtin "Text.take"
           op [Term.Number' n, Term.Text' txt] = pure $ Term.text (Text.take (floor n) txt)
           op _ = error "Text.take unpossible"
           typ = "Number -> Text -> Text"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Text.take")
     , let r = R.Builtin "Text.drop"
           op [Term.Number' n, Term.Text' txt] = pure $ Term.text (Text.drop (floor n) txt)
           op _ = error "Text.drop unpossible"
           typ = "Number -> Text -> Text"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Text.drop")
     , -- todo: rather special purpose, remove this in favor of more generic regex
       let r = R.Builtin "Text.words"
           op [Term.Text' txt] = pure $
             let words = map stripPunctuation $ Text.split Char.isSpace txt
                 stripPunctuation word = Text.dropAround (not . Char.isAlphaNum) word
             in Term.vector (map Term.text . filter (not . Text.null) $ words)
           op _ = error "Text.words unpossible"
           typ = "Text -> Vector Text"
       in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Text.words")

     -- Pair
     , let r = R.Builtin "Pair"
       in (r, Nothing, unsafeParseType "forall a b . a -> b -> Pair a b", prefix "Pair")
     , let r = R.Builtin "Pair.fold"
           op [f,Term.Apps' (Term.Builtin' "Pair") [a,b]] = pure $ f `Term.apps` [a,b]
           op _ = error "Pair.fold unpossible"
       in (r, Just (I.Primop 2 op), unsafeParseType "forall a b c . (a -> b -> c) -> Pair a b -> c", prefix "Pair.fold")
     , let r = R.Builtin "Pair.Order"
       in (r, Nothing, unsafeParseType "forall a b . Order a -> Order b -> Order (Pair a b)", prefix "Pair.Order")

     -- Either
     , let r = R.Builtin "Either.Left"
       in (r, Nothing, unsafeParseType "forall a b . a -> Either a b", prefix "Left")
     , let r = R.Builtin "Either.Right"
       in (r, Nothing, unsafeParseType "forall a b . b -> Either a b", prefix "Right")
     , let r = R.Builtin "Either.fold"
           op [fa,fb,e] = do
             let Term.App' (Term.Builtin' tag) aOrB = e
             case tag of
               _ | tag == "Either.Left" -> pure $ fa `Term.app` aOrB
                 | tag == "Either.Right" -> pure $ fb `Term.app` aOrB
                 | otherwise -> error "type errror"
           op _ = error "Either.fold unpossible"
       in (r, Just (I.Primop 3 op), unsafeParseType "forall a b r . (a -> r) -> (b -> r) -> Either a b -> r", prefix "Either.fold")

     -- Optional
     , let r = R.Builtin "Optional.None"
       in (r, Nothing, unsafeParseType "forall a . Optional a", prefix "None")
     , let r = R.Builtin "Optional.Some"
       in (r, Nothing, unsafeParseType "forall a . a -> Optional a", prefix "Some")
     , let r = R.Builtin "Optional.fold"
           op [fz,f,o] = case o of
             Term.Builtin' tag | tag == "Optional.None" -> pure fz
             Term.App' (Term.Builtin' tag) a | tag == "Optional.Some" -> pure (f `Term.app` a)
             _ -> error $ "Optional.fold unpossible: " ++ show o
           op _ = error "Optional.fold unpossible"
       in (r, Just (I.Primop 3 op), unsafeParseType "forall a r . r -> (a -> r) -> Optional a -> r", prefix "Optional.fold")

     -- Vector
     , let r = R.Builtin "Vector.append"
           op [last,Term.Vector' init] = do
             pure $ Term.vector' (Vector.snoc init last)
           op _ = fail "Vector.append unpossible"
       in (r, Just (I.Primop 2 op), unsafeParseType "forall a . a -> Vector a -> Vector a", prefix "Vector.append")
     , let r = R.Builtin "Vector.concatenate"
           op [Term.Vector' a, Term.Vector' b] = pure $ Term.vector' (a `mappend` b)
           op _ = fail "Vector.concatenate unpossible"
       in (r, Just (I.Primop 2 op), unsafeParseType "forall a . Vector a -> Vector a -> Vector a", prefix "Vector.concatenate")
     , let r = R.Builtin "Vector.empty"
           op [] = pure $ Term.vector mempty
           op _ = fail "Vector.empty unpossible"
       in (r, Just (I.Primop 0 op), unsafeParseType "forall a . Vector a", prefix "Vector.empty")
     , let r = R.Builtin "Vector.range"
           op [Term.Number' start, Term.Number' stop] =
             let num = Term.num . fromIntegral
                 ns = [floor start .. floor stop - (1 :: Int)]
             in pure $ Term.vector' (Vector.fromList . map num $ ns)
           op _ = fail "Vector.range unpossible"
           typ = unsafeParseType "Number -> Number -> Vector Number"
       in (r, Just (I.Primop 2 op), typ, prefix "Vector.range")
     , let r = R.Builtin "Vector.empty?"
           op [Term.Vector' vs] = pure $ if Vector.null vs then true else false
           op _ = fail "Vector.empty? unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "forall a . Vector a -> Boolean", prefix "empty?")
     , let r = R.Builtin "Vector.zip"
           op [Term.Vector' vs, Term.Vector' vs2] =
             pure $ Term.vector' (Vector.zipWith pair' vs vs2)
           op _ = fail "Vector.zip unpossible"
           typ = "∀ a b . Vector a -> Vector b -> Vector (a,b)"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Vector.zip")
     , let r = R.Builtin "Vector.sort-keyed"
           op [Term.Vector' vs0] =
             let extract1 (Term.Apps' _ [hd, _]) = extractKey hd
                 extract1 _ = error "extract1 failure"
                 extract2 (Term.Apps' _ [_, Term.Apps' _ [hd,_]]) = hd
                 extract2 _ = error "extract2 failure"
                 ks = fmap extract1 vs0
                 vs = fmap extract2 vs0
                 sortableVs = Vector.zip ks vs
                 f' (a, _) (b, _) = a `compare` b
                 sorted = sortBy f' (Vector.toList sortableVs)
             in pure . Term.vector . fmap snd $ sorted
           op _ = fail "Vector.sort-keyed unpossible"
           typ = "∀ a k . Vector (Order.Key k, a) -> Vector a"
       in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Vector.sort-keyed")
     , let r = R.Builtin "Vector.size"
           op [Term.Vector' vs] = pure $ Term.num (fromIntegral $ Vector.length vs)
           op _ = fail "Vector.size unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "forall a . Vector a -> Number", prefix "Vector.size")
     , let r = R.Builtin "Vector.reverse"
           op [Term.Vector' vs] = pure $ Term.vector' (Vector.reverse vs)
           op _ = fail "Vector.reverse unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "forall a . Vector a -> Vector a", prefix "Vector.reverse")
     , let r = R.Builtin "Vector.halve"
           op [Term.Vector' vs] = pure $ case Vector.null vs of
             True -> pair' (Term.vector []) (Term.vector [])
             False -> case Vector.splitAt (Vector.length vs `div` 2) vs of
               (x,y) -> pair' (Term.vector' x) (Term.vector' y)
           op _ = fail "Vector.halve unpossible"
           typ = "forall a . Vector a -> (Vector a, Vector a)"
       in (r, Just (I.Primop 1 op), unsafeParseType typ, prefix "Vector.halve")
     , let r = R.Builtin "Vector.at"
           op [Term.Number' n, Term.Vector' vs] =
             pure $ case vs Vector.!? (floor n) of
               Nothing -> none
               Just t -> some t
           op _ = fail "Vector.at unpossible"
           typ = "forall a . Number -> Vector a -> Optional a"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Vector.at")
     , let r = R.Builtin "Vector.take"
           op [Term.Number' n, Term.Vector' vs] = pure $ Term.vector' (Vector.take (floor n) vs)
           op _ = fail "Vector.take unpossible"
           typ = "forall a . Number -> Vector a -> Vector a"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Vector.take")
     , let r = R.Builtin "Vector.drop"
           op [Term.Number' n, Term.Vector' vs] =
             pure $ Term.vector' (Vector.drop (floor n) vs)
           op _ = fail "Vector.drop unpossible"
           typ = "forall a . Number -> Vector a -> Vector a"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Vector.drop")
       -- I AM HERE
     , let r = R.Builtin "Vector.fold-left"
           op [f,z,Term.Vector' vs] = pure $
             Vector.foldl' (\acc a -> f `Term.apps` [acc, a]) z vs
           op _ = fail "Vector.fold-left unpossible"
           typ = "forall a b . (b -> a -> b) -> b -> Vector a -> b"
       in (r, Just (I.Primop 3 op), unsafeParseType typ, prefix "Vector.fold-left")
     , let r = R.Builtin "Vector.map"
           op [f,Term.Vector' vs] = pure $ Term.vector' (fmap (Term.app f) vs)
           op _ = fail "Vector.map unpossible"
       in (r, Just (I.Primop 2 op), unsafeParseType "forall a b . (a -> b) -> Vector a -> Vector b", prefix "Vector.map")
     , let r = R.Builtin "Vector.prepend"
           op [hd, Term.Vector' tl] = pure $ Term.vector' (Vector.cons hd tl)
           op _ = fail "Vector.prepend unpossible"
       in (r, Just (I.Primop 2 op), unsafeParseType "forall a . a -> Vector a -> Vector a", prefix "Vector.prepend")
     , let r = R.Builtin "Vector.single"
           op [hd] = pure $ Term.vector (pure hd)
           op _ = fail "Vector.single unpossible"
       in (r, Just (I.Primop 1 op), unsafeParseType "forall a . a -> Vector a", prefix "Vector.single")

     , let r = R.Builtin "Order.invert"
       in (r, Nothing, unsafeParseType "forall a . Order a -> Order a", prefix "Order.invert")
     , let r = R.Builtin "Order.ignore"
       in (r, Nothing, unsafeParseType "forall a . Order a", prefix "Order.ignore")

     , let r = R.Builtin "Less"
       in (r, Nothing, unsafeParseType "Comparison", prefix "Less")
     , let r = R.Builtin "Greater"
       in (r, Nothing, unsafeParseType "Comparison", prefix "Greater")
     , let r = R.Builtin "Equal"
       in (r, Nothing, unsafeParseType "Comparison", prefix "Equal")
     , let r = R.Builtin "Comparison.fold"
           op [lt,eq,gt, Term.Builtin' c] = case Text.head c of
             'L' -> pure lt
             'E' -> pure eq
             'G' -> pure gt
             _ -> fail $ "Comparison.fold not one of {Less,Equal,Greater}" ++ show c
           op _ = error "Comparison.fold unpossible"
       in (r, Just (I.Primop 4 op), unsafeParseType "∀ r . r -> r -> r -> Comparison -> r", prefix "Comparison.fold")

     , let r = R.Builtin "Order.Key.compare"
           op [a,b] =
             pure $ case compareKeys a b of
               LT -> Term.builtin "Less"
               EQ -> Term.builtin "Equal"
               GT -> Term.builtin "Greater"
           op _ = error "Order.Key.compare unpossible"
           typ = "∀ a . Order.Key a -> Order.Key a -> Comparison"
       in (r, Just (I.Primop 2 op), unsafeParseType typ, prefix "Order.Key.compare")

     , let r = R.Builtin "Order.key"
           flip ts = (map neg (ts []) ++) where
             neg (Term.Text' t) = Term.text (Text.reverse t)
             neg (Term.Number' n) = Term.num (negate n)
             neg t@(Term.Builtin' _) = t
             neg t = error $ "don't know how to negate " ++ show t
           op' ord a = case ord of
             Term.App' (Term.Builtin' invert) ord
               | invert == "Order.invert" -> flip <$> op' ord a
             Term.Builtin' b
               | b == "Text.Order" -> pure (a:)
               | b == "Number.Order" -> pure (a:)
               | b == "Hash.Order" -> do Term.App' _ a <- pure a; pure (a:)
               | b == "Unit.Order" -> pure (a:)
               | b == "Order.ignore" -> pure id
               | otherwise -> fail $ "unrecognized order type: " ++ Text.unpack b
             Term.Apps' (Term.Builtin' pair) [ord1, ord2]
               | pair == "Pair.Order" -> do
                   Term.Apps' _ [a,b] <- pure a
                   (.) <$> op' ord1 a <*> op' ord2 b
               | otherwise -> fail $ "unrecognized order type: " ++ Text.unpack pair
           op [ord,a] = Term.app (Term.builtin "Order.Key")
                      . foldr Term.app unitRef
                      . ($ [])
                    <$> op' ord a
           op _ = fail "Order.key unpossible"
       in (r, Just (I.Primop 2 op), unsafeParseType "forall a . Order a -> a -> Order.Key a", prefix "Order.key")
     ]

extractKey :: Var v => Term v -> [Either Double Text]
extractKey (Term.App' _ t1) = go t1 where
  go (Term.Builtin' _) = []
  go (Term.App' (Term.Text' t) tl) = Right t : go tl
  go (Term.App' (Term.Number' n) tl) = Left n : go tl
  go (Term.App' (Term.Builtin' b) tl) = Right b : go tl
  go _ = error $ "don't know what to do with this in extractKey: " ++ show t1
extractKey t = error $ "not a key: " ++ show t

compareKeys :: Term v -> Term v -> Ordering
compareKeys (Term.App' _ t1) (Term.App' _ t2) = go t1 t2 where
  go (Term.Builtin' u) (Term.Builtin' u2) = u `compare` u2
  go (Term.App' h1 t1) (Term.App' h2 t2) =
    let go' :: Ord a => a -> a -> Ordering
        go' a a2 = case a `compare` a2 of
          EQ -> go t1 t2
          done -> done
    in
      case (h1,h2) of
        (Term.Text' h1, Term.Text' h2) -> go' h1 h2
        (Term.Number' h1, Term.Number' h2) -> go' h1 h2
        (Term.Builtin' h1, Term.Builtin' h2) -> go' h1 h2
  go (Term.App' _ _) _ = GT
  go _ _ = LT
compareKeys _ _ = error "not a key"

-- type helpers
unitT :: Ord v => Type v
unitT = Type.ref (R.Builtin "Unit")

infixr 7 -->
(-->) :: Ord v => Type v -> Type v -> Type v
(-->) = Type.arrow

-- term helpers
none :: Var v => Term v
none = Term.ref $ R.Builtin "Optional.None"
some :: Var v => Term v -> Term v
some t = Term.ref (R.Builtin "Optional.Some") `Term.app` t
left :: Var v => Term v -> Term v
left t = Term.ref (R.Builtin "Either.Left") `Term.app` t
right :: Var v => Term v -> Term v
right t = Term.ref (R.Builtin "Either.Right") `Term.app` t

-- metadata helpers
opl :: Int -> Text -> Metadata V h
opl p s =
  let
    sym :: Symbol DFO
    sym = Var.named s
    s' = Symbol.annotate (View.binary View.AssociateL (View.Precedence p)) sym
  in
    Metadata Metadata.Term (Metadata.Names [s']) Nothing

assoc :: Int -> Text -> Metadata V h
assoc p s =
  let
    sym :: Symbol DFO
    sym = Var.named s
    s' = Symbol.annotate (View.binary View.Associative (View.Precedence p)) sym
  in
    Metadata Metadata.Term (Metadata.Names [s']) Nothing

prefix :: Var v => Text -> Metadata v h
prefix s = prefixes [s]

prefixes :: Var v => [Text] -> Metadata v h
prefixes s = Metadata Metadata.Term
                      (Metadata.Names (map Var.named s))
                      Nothing
