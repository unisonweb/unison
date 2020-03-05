{-# language PatternGuards #-}

module Unison.Test.ANF where

import EasyTest

import Unison.ABT.Normalized (Term(TAbs))
import Unison.Pattern (PatternP(..))
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF
import Unison.Var as Var

import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import qualified Unison.Term as Term
import qualified Unison.ABT as ABT
import Unison.Test.Common (tm)

-- testSNF s = ok
--   where
--   t0 = tm s
--   snf = toSuperNormal (const 0) t0

testANF :: String -> Test ()
testANF s
  | t0 == denormalize anf = ok
  | otherwise = crash $ show $ denormalize anf
  where
  t0 = const () `Term.amap` tm s
  anf = anfTerm Set.empty (const 0) t0

denormalize :: Var v => ANormal v -> Term.Term0 v
denormalize (TVar v) = Term.var () v
denormalize (TLit l) = case l of
  I i -> Term.int () i
  N n -> Term.nat () n
  F f -> Term.float () f
  B b -> Term.boolean () b
  T t -> Term.text () t
  C c -> Term.char () c
denormalize (THnd h b) = Term.handle () (Term.var () h) $ denormalize b
denormalize (TLet v bn bo)
  | typeOf v == ANFBlank = ABT.subst v dbn dbo
  | otherwise = Term.let1_ False [(v, dbn)] dbo
  where
  dbn = denormalize $ TTm bn
  dbo = denormalize bo
denormalize (TMatch v cs)
  = Term.match () (ABT.var v) $ denormalizeMatch cs
denormalize (TApp f args) = Term.apps' df (Term.var () <$> args)
  where
  df = case f of
    FVar v -> Term.var () v
    FComb _ -> error "hmm"
    FCon r n -> Term.constructor () (denormalizeRef r) n
    FReq r n -> Term.request () (denormalizeRef r) n
    FPrim _ -> error "hmm" -- Term.ref _

denormalizeRef :: Int -> Reference
denormalizeRef _ = error "denormalizeRef"

denormalizeMatch
  :: Var v => Branched (ANormal v) -> [Term.MatchCase () (Term.Term0 v)]
denormalizeMatch b
  | MatchIntegral m <- b = dcase ipat <$> IMap.toList m
  | MatchData r m <- b = dcase (dpat r) <$> IMap.toList m
  | MatchAbility r m d <- b
  = dpure d : (dcase (epat r) <$> IMap.toList m)
  where
  dpure d
    = Term.MatchCase (EffectPureP () (VarP ())) Nothing . snd $ dbranch d
  dcase p (t, br) = Term.MatchCase (p n t) Nothing dbr
   where (n, dbr) = dbranch br

  ipat _ i = IntP () $ fromIntegral i
  dpat r n t = ConstructorP () r t (replicate n $ VarP ())
  epat r n t = EffectBindP () r t (replicate n $ VarP ()) (VarP ())

  dbranch (TAbs v br) = (n+1, ABT.abs v dbr)
   where (n, dbr) = dbranch br
  dbranch tm = (0, denormalize tm)

test :: Test ()
test = scope "anf" . tests $
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
