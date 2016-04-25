{-# LANGUAGE OverloadedStrings #-}
module Unison.Node.Builtin where

import Data.Text (Text)
import Unison.Metadata (Metadata(..))
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker.Context (remoteSignatureOf)
import Unison.Var (Var)
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Metadata as Metadata
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Remote as Remote
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import qualified Unison.View as View

type DFO = View.DFO
type V = Symbol DFO
type WHNFEval = Term V -> N.Noted IO (ABT.Term (Term.F V) V ())

data Builtin = Builtin
               { reference :: R.Reference
               , op :: Maybe (I.Primop (N.Noted IO) V)
               , bType :: Type V
               , metadata :: Metadata V R.Reference
               }

{--
addNodeDefinition node store (r, _, t, md) = Node.updateMetadata node r md *> Store.annotateTerm store r t
--}

{--
store k v = Type.app (Type.app (Type.ref (R.Builtin "Store")) k) v
store' k v = Type.ref (R.Builtin "Store") `Type.app` k `Type.app` v
--}

{--
extraOps = [ let r = R.Builtin "KeyValue.store"
             in (r, Nothing, store' num num, prefix "numberStore")
           ]
--}
-- whnf :: (Hash -> m (Term v)) -> Term v -> m (Term v), -- ^ Simplify to weak head normal form
unitRef :: Ord v => Term v
unitRef = Term.ref (R.Builtin "()")

makeBuiltins :: WHNFEval -> [Builtin]
makeBuiltins whnf =
  let
    numeric2 :: Term V -> (Double -> Double -> Double) -> I.Primop (N.Noted IO) V
    numeric2 sym f = I.Primop 2 $ \xs -> case xs of
      [x,y] -> g <$> whnf x <*> whnf y
        where g (Term.Number' x) (Term.Number' y) = Term.lit (Term.Number (f x y))
              g x y = sym `Term.app` x `Term.app` y
      _ -> error "unpossible"
    strict r n = Just (I.Primop n f)
      where f args = reapply <$> traverse whnf (take n args)
                      where reapply args' = Term.ref r `apps` args' `apps` drop n args
            apps f args = foldl Term.app f args
    string2 :: Term V -> (Text -> Text -> Text) -> I.Primop (N.Noted IO) V
    string2 sym f = I.Primop 2 $ \xs -> case xs of
      [x,y] -> g <$> whnf x <*> whnf y
        where g (Term.Text' x) (Term.Text' y) = Term.lit (Term.Text (f x y))
              g x y = sym `Term.app` x `Term.app` y
      _ -> error "unpossible"
  in map (\(r, o, t, m) -> Builtin r o t m)
     [ let r = R.Builtin "()"
       in (r, Nothing, unitT, prefix "()")

     , let r = R.Builtin "Color.rgba"
       in (r, strict r 4, num --> num --> num --> num --> colorT, prefix "rgba")

     , let r = R.Builtin "Number.plus"
       in (r, Just (numeric2 (Term.ref r) (+)), numOpTyp, assoc 4 "+")
     , let r = R.Builtin "Number.minus"
       in (r, Just (numeric2 (Term.ref r) (-)), numOpTyp, opl 4 "-")
     , let r = R.Builtin "Number.times"
       in (r, Just (numeric2 (Term.ref r) (*)), numOpTyp, assoc 5 "*")
     , let r = R.Builtin "Number.divide"
       in (r, Just (numeric2 (Term.ref r) (/)), numOpTyp, opl 5 "/")

     , let r = R.Builtin "Remote.at"
           op [node,term] = do
             Term.Distributed' (Term.Node node) <- whnf node
             pure $ Term.remote (Remote.Step (Remote.At node term))
           op _ = fail "Remote.at unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.at", prefix "at")
     , let r = R.Builtin "Remote.here"
           op [] = pure $ Term.remote (Remote.Step (Remote.Local (Remote.Here)))
           op _ = fail "Remote.here unpossible"
       in (r, Just (I.Primop 0 op), remoteSignatureOf "Remote.here", prefix "here")
     , let r = R.Builtin "Remote.send"
           op [c, v] = do
             Term.Distributed' (Term.Channel c) <- whnf c
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.Send c v)))
           op _ = fail "Remote.send unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.send", prefix "send")
     , let r = R.Builtin "Remote.channel"
           op [] = pure $ Term.remote (Remote.Step (Remote.Local Remote.CreateChannel))
           op _ = fail "Remote.channel unpossible"
       in (r, Just (I.Primop 0 op), remoteSignatureOf "Remote.channel", prefix "channel")
    , let r = R.Builtin "Remote.bind"
          op [g, r] = do
            r <- whnf r
            -- right associate the binds so that there is always a Step on the outside
            let kcomp f g = Term.lam' ["x"] $ Term.builtin "Remote.bind" `Term.apps` [g, f `Term.app` Term.var' "x"]
            case r of
              Term.Distributed' (Term.Remote (Remote.Step s)) -> pure $ Term.remote (Remote.Bind s g)
              Term.Distributed' (Term.Remote (Remote.Bind s f)) -> pure $ Term.remote (Remote.Bind s (kcomp f g))
              _ -> fail "Remote.bind given a value that was not a Remote"
          op _ = fail "Remote.bind unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.bind", prefix "bind")
     , let r = R.Builtin "Remote.pure"
           op [a] = pure $ Term.remote (Remote.Step (Remote.Local (Remote.Pure a)))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 1 op), remoteSignatureOf "Remote.pure", prefix "pure")
     , let r = R.Builtin "Remote.receiveAsync"
           op [chan, timeout] = do
             Term.Number' seconds <- whnf timeout
             Term.Distributed' (Term.Channel chan) <- whnf chan
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.ReceiveAsync chan (Remote.Seconds seconds))))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 2 op), remoteSignatureOf "Remote.receiveAsync", prefix "receiveAsync")
     , let r = R.Builtin "Remote.receive"
           op [chan] = do
             Term.Distributed' (Term.Channel chan) <- whnf chan
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.Receive chan)))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 1 op), remoteSignatureOf "Remote.receive", prefix "receive")
     , let r = R.Builtin "Remote.fork"
           op [r] = do
             Term.Distributed' (Term.Remote r) <- whnf r
             pure $ Term.remote (Remote.Step (Remote.Local (Remote.Fork r)))
           op _ = fail "unpossible"
       in (r, Just (I.Primop 1 op), remoteSignatureOf "Remote.fork", prefix "fork")

     , let r = R.Builtin "Symbol.Symbol"
       in (r, Nothing, str --> fixityT --> num --> symbolT, prefix "Symbol")

     , let r = R.Builtin "Text.concatenate"
       in (r, Just (string2 (Term.ref r) mappend), strOpTyp, prefixes ["concatenate", "Text"])
     , let r = R.Builtin "Text.left"
       in (r, Nothing, alignmentT, prefixes ["left", "Text"])
     , let r = R.Builtin "Text.right"
       in (r, Nothing, alignmentT, prefixes ["right", "Text"])
     , let r = R.Builtin "Text.center"
       in (r, Nothing, alignmentT, prefixes ["center", "Text"])
     , let r = R.Builtin "Text.justify"
       in (r, Nothing, alignmentT, prefixes ["justify", "Text"])

     , let r = R.Builtin "Vector.append"
           op [last,init] = do
             initr <- whnf init
             pure $ case initr of
               Term.Vector' init -> Term.vector' (Vector.snoc init last)
               init -> Term.ref r `Term.app` last `Term.app` init
           op _ = fail "Vector.append unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a"] $ v' "a" --> vec (v' "a") --> vec (v' "a"), prefix "append")
     , let r = R.Builtin "Vector.concatenate"
           op [a,b] = do
             ar <- whnf a
             br <- whnf b
             pure $ case (ar,br) of
               (Term.Vector' a, Term.Vector' b) -> Term.vector' (a `mappend` b)
               (a,b) -> Term.ref r `Term.app` a `Term.app` b
           op _ = fail "Vector.concatenate unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a"] $ vec (v' "a") --> vec (v' "a") --> vec (v' "a"), prefix "concatenate")
     , let r = R.Builtin "Vector.empty"
           op [] = pure $ Term.vector mempty
           op _ = fail "Vector.empty unpossible"
       in (r, Just (I.Primop 0 op), Type.forall' ["a"] (vec (v' "a")), prefix "empty")
     , let r = R.Builtin "Vector.fold-left"
           op [f,z,vec] = whnf vec >>= \vec -> case vec of
             Term.Vector' vs -> Vector.foldM (\acc a -> whnf (f `Term.apps` [acc, a])) z vs
             _ -> pure $ Term.ref r `Term.app` vec
           op _ = fail "Vector.fold-left unpossible"
       in (r, Just (I.Primop 3 op), Type.forall' ["a","b"] $ (v' "b" --> v' "a" --> v' "b") --> v' "b" --> vec (v' "a") --> v' "b", prefix "fold-left")
     , let r = R.Builtin "Vector.map"
           op [f,vec] = do
             vecr <- whnf vec
             pure $ case vecr of
               Term.Vector' vs -> Term.vector' (fmap (Term.app f) vs)
               _ -> Term.ref r `Term.app` vecr
           op _ = fail "Vector.map unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a","b"] $ (v' "a" --> v' "b") --> vec (v' "a") --> vec (v' "b"), prefix "map")
     , let r = R.Builtin "Vector.prepend"
           op [hd,tl] = do
             tlr <- whnf tl
             pure $ case tlr of
               Term.Vector' tl -> Term.vector' (Vector.cons hd tl)
               tl -> Term.ref r `Term.app` hd `Term.app` tl
           op _ = fail "Vector.prepend unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a"] $ v' "a" --> vec (v' "a") --> vec (v' "a"), prefix "prepend")
     , let r = R.Builtin "Vector.single"
           op [hd] = pure $ Term.vector (pure hd)
           op _ = fail "Vector.single unpossible"
       in (r, Just (I.Primop 1 op), Type.forall' ["a"] $ v' "a" --> vec (v' "a"), prefix "single")
     ]

-- types ripped from "make"
alignmentT :: Ord v => Type v
alignmentT = Type.ref (R.Builtin "Alignment")
arr :: Ord v => Type v -> Type v -> Type v
arr = Type.arrow
colorT :: Ord v => Type v
colorT = Type.ref (R.Builtin "Color")
fixityT :: Ord v => Type v
fixityT = Type.ref (R.Builtin "Fixity")
num :: Ord v => Type v
num = Type.lit Type.Number
numOpTyp :: Ord v => Type v
numOpTyp = num --> num --> num
str :: Ord v => Type v
str = Type.lit Type.Text
strOpTyp :: Ord v => Type v
strOpTyp = str `arr` (str `arr` str)
symbolT :: Ord v => Type v
symbolT = Type.ref (R.Builtin "Symbol")
unitT :: Ord v => Type v
unitT = Type.ref (R.Builtin "Unit")
v' :: (Ord v, Var v) => Text -> Type v
v' = Type.v'
vec a = Type.app (Type.lit Type.Vector) a
remote :: Ord v => Type v -> Type v
remote v = Type.builtin "Remote" `Type.app` v
remote' :: Ord v => Type v -> Type v
remote' v = Type.builtin "Remote!" `Type.app` v
future :: Ord v => Type v -> Type v
future v = Type.builtin "Future" `Type.app` v
channel :: Ord v => Type v -> Type v
channel v = Type.builtin "Channel" `Type.app` v

infixr 7 -->
(-->) :: Ord v => Type v -> Type v -> Type v
(-->) = Type.arrow

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

prefix :: Text -> Metadata V h
prefix s = prefixes [s]

prefixes :: [Text] -> Metadata V h
prefixes s = Metadata Metadata.Term
                      (Metadata.Names (map Var.named s))
                      Nothing
