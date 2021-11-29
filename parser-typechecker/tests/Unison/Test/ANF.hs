{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# language BangPatterns #-}
{-# language PatternGuards #-}

module Unison.Test.ANF where

import EasyTest

import Unison.ABT.Normalized (Term(TAbs))
import Unison.ConstructorReference (GConstructorReference(..))
import qualified Unison.Pattern as P
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF
import Unison.Runtime.MCode (emitCombs, RefNums(..))
import Unison.Type as Ty
import Unison.Var as Var

import Unison.Util.EnumContainers as EC

import Data.Word (Word64)

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Unison.Term as Term
import qualified Unison.ABT as ABT
import Unison.Test.Common (tm)

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (evalState)
import qualified Unison.Util.Text as Util.Text

-- testSNF s = ok
--   where
--   t0 = tm s
--   snf = toSuperNormal (const 0) t0

simpleRefs :: Reference -> RTag
simpleRefs r
  | r == Ty.natRef = 0
  | r == Ty.intRef = 1
  | r == Ty.floatRef = 2
  | r == Ty.booleanRef = 3
  | r == Ty.textRef = 4
  | r == Ty.charRef = 5
  | otherwise = 100

runANF :: Var v => ANFM v a -> a
runANF m = evalState (runReaderT m Set.empty) (0, 1, [])

testANF :: String -> Test ()
testANF s
  | t0 == denormalize anf = ok
  | otherwise = crash $ show $ denormalize anf
  where
  t0 = const () `Term.amap` tm s
  anf = snd . runANF $ anfTerm t0

testLift :: String -> Test ()
testLift s = case cs of !_ -> ok
  where
  cs = emitCombs (RN (const 0) (const 0)) 0
     . superNormalize
     . fst
     . lamLift
     $ tm s

denormalize :: Var v => ANormal v -> Term.Term0 v
denormalize (TVar v) = Term.var () v
denormalize (TLit l) = case l of
  I i -> Term.int () i
  N n -> Term.nat () n
  F f -> Term.float () f
  T t -> Term.text () (Util.Text.toText t)
  C c -> Term.char () c
  LM r -> Term.termLink () r
  LY r -> Term.typeLink () r
denormalize (THnd _ _ _)
  = error "denormalize handler"
  -- = Term.match () (denormalize b) $ denormalizeHandler h
denormalize (TShift _ _ _)
  = error "denormalize shift"
denormalize (TLet _ v _ bn bo)
  | typeOf v == ANFBlank = ABT.subst v dbn dbo
  | otherwise = Term.let1_ False [(v, dbn)] dbo
  where
  dbn = denormalize bn
  dbo = denormalize bo
denormalize (TName _ _ _ _)
  = error "can't denormalize by-name bindings"
denormalize (TMatch v cs)
  = Term.match () (ABT.var v) $ denormalizeMatch cs
denormalize (TApp f args)
  | FCon r 0 <- f
  , r `elem` [Ty.natRef, Ty.intRef]
  , [v] <- args
  = Term.var () v
denormalize (TApp f args) = Term.apps' df (Term.var () <$> args)
  where
  df = case f of
    FVar v -> Term.var () v
    FComb _ -> error "FComb"
    FCon r n ->
      Term.constructor () (ConstructorReference r (fromIntegral $ rawTag n))
    FReq r n ->
      Term.request () (ConstructorReference r (fromIntegral $ rawTag n))
    FPrim _ -> error "FPrim"
    FCont _ -> error "denormalize FCont"
denormalize (TFrc _) = error "denormalize TFrc"

denormalizeRef :: RTag -> Reference
denormalizeRef r
  | 0 <- rawTag r = Ty.natRef
  | 1 <- rawTag r = Ty.intRef
  | 2 <- rawTag r = Ty.floatRef
  | 3 <- rawTag r = Ty.booleanRef
  | 4 <- rawTag r = Ty.textRef
  | 5 <- rawTag r = Ty.charRef
  | otherwise = error "denormalizeRef"

backReference :: Word64 -> Reference
backReference _ = error "backReference"

denormalizeMatch
  :: Var v => Branched (ANormal v) -> [Term.MatchCase () (Term.Term0 v)]
denormalizeMatch b
  | MatchEmpty <- b = []
  | MatchIntegral m df <- b
  = (dcase (ipat Ty.intRef) <$> mapToList m) ++ dfcase df
  | MatchText m df <- b
  = (dcase (const $ P.Text () . Util.Text.toText) <$> Map.toList m) ++ dfcase df
  | MatchData r cs Nothing <- b
  , [(0, ([UN], zb))] <- mapToList cs
  , TAbs i (TMatch j (MatchIntegral m df))  <- zb
  , i == j
  = (dcase (ipat r) <$> mapToList m) ++ dfcase df
  | MatchData r m df <- b
  = (dcase (dpat r) . fmap snd <$> mapToList m) ++ dfcase df
  | MatchRequest hs df <- b = denormalizeHandler hs df
  | MatchSum _ <- b = error "MatchSum not a compilation target"
  where
  dfcase (Just d)
    = [Term.MatchCase (P.Unbound ()) Nothing $ denormalize d]
  dfcase Nothing = []

  dcase p (t, br) = Term.MatchCase (p n t) Nothing dbr
   where (n, dbr) = denormalizeBranch br

  ipat r _ i
    | r == Ty.natRef = P.Nat () $ fromIntegral i
    | otherwise = P.Int () $ fromIntegral i
  dpat r n t = P.Constructor () (ConstructorReference r (fromIntegral (fromEnum t))) (replicate n $ P.Var ())

denormalizeBranch :: (Num a, Var v) =>
                     Term ANormalF v -> (a, ABT.Term (Term.F v () ()) v ())
denormalizeBranch (TAbs v br) = (n+1, ABT.abs v dbr)
 where (n, dbr) = denormalizeBranch br
denormalizeBranch tm = (0, denormalize tm)

denormalizeHandler
  :: Var v
  => Map.Map Reference (EnumMap CTag ([Mem], ANormal v))
  -> ANormal v
  -> [Term.MatchCase () (Term.Term0 v)]
denormalizeHandler cs df = dcs
  where
  dcs = Map.foldMapWithKey rf cs <> dfc
  dfc = [ Term.MatchCase
            (P.EffectPure () (P.Var ()))
            Nothing
            db
        ]
   where (_, db) = denormalizeBranch df
  rf r rcs = foldMapWithKey (cf r) rcs
  cf r t b = [ Term.MatchCase
                 (P.EffectBind () (ConstructorReference r (fromIntegral (fromEnum t)))
                   (replicate n $ P.Var ()) (P.Var ()))
                 Nothing
                 db
             ]
   where (n, db) = denormalizeBranch (snd b)

test :: Test ()
test = scope "anf" . tests $
  [ scope "lift" . tests $
    [ testLift "let\n\
               \  g = m x -> ##Nat.+ x m\n\
               \  m -> g m m"
    , testLift "m n -> let\n\
               \  f acc i = match i with\n\
               \     0 -> acc\n\
               \     _ -> f (##Nat.+ acc n) (##Nat.sub i 1)\n\
               \  f 0 m"
    ]
  , scope "denormalize" . tests $
    [ testANF "1"
    , testANF "1 + 2"
    , testANF "match x with\n\
              \  +1 -> foo\n\
              \  +2 -> bar\n\
              \  +3 -> baz"
    , testANF "1 + match x with\n\
              \  +1 -> foo\n\
              \  +2 -> bar"
    , testANF "(match x with +3 -> foo) + (match x with +2 -> foo)"
    ]
  ]
