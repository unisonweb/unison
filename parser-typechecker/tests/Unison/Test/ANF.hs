{-# language BangPatterns #-}
{-# language PatternGuards #-}

module Unison.Test.ANF where

import EasyTest

import Unison.ABT.Normalized (Term(TAbs))
import Unison.Pattern (PatternP(..))
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF
import Unison.Runtime.MCode (emitCombs)
import Unison.Type as Ty
import Unison.Var as Var

import Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import qualified Data.Set as Set

import qualified Unison.Term as Term
import qualified Unison.ABT as ABT
import Unison.Test.Common (tm)

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (evalState)

-- testSNF s = ok
--   where
--   t0 = tm s
--   snf = toSuperNormal (const 0) t0

runANF :: Var v => ANFM v a -> a
runANF m = evalState (runReaderT m (Set.empty, const 0)) (Set.empty, [])

testANF :: String -> Test ()
testANF s
  | t0 == denormalize anf = ok
  | otherwise = crash $ show $ denormalize anf
  where
  t0 = const () `Term.amap` tm s
  anf = runANF $ anfTerm t0

testLift :: String -> Test ()
testLift s = case cs of (!_, !_, _) -> ok
  where
  cs = emitCombs 0 . superNormalize (const 0) . lamLift $ tm s

denormalize :: Var v => ANormal v -> Term.Term0 v
denormalize (TVar v) = Term.var () v
denormalize (TLit l) = case l of
  I i -> Term.int () i
  N n -> Term.nat () n
  F f -> Term.float () f
  B b -> Term.boolean () b
  T t -> Term.text () t
  C c -> Term.char () c
denormalize (THnd _  _ _ _)
  = error "denormalize handler"
  -- = Term.match () (denormalize b) $ denormalizeHandler h
denormalize (TLet v _ bn bo)
  | typeOf v == ANFBlank = ABT.subst v dbn dbo
  | otherwise = Term.let1_ False [(v, dbn)] dbo
  where
  dbn = denormalize $ TTm bn
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
    FComb _ -> error "hmm"
    FCon r n -> Term.constructor () r n
    FReq r n -> Term.request () (denormalizeRef r) n
    FPrim _ -> error "hmm" -- Term.ref _

denormalizeRef :: Int -> Reference
denormalizeRef _ = error "denormalizeRef"

denormalizeMatch
  :: Var v => Branched (ANormal v) -> [Term.MatchCase () (Term.Term0 v)]
denormalizeMatch b
  | MatchEmpty <- b = []
  | MatchIntegral m df <- b
  = (dcase (ipat Ty.intRef) <$> IMap.toList m) ++ dfcase df
  | MatchData r cs Nothing <- b
  , [(0, ([UN], zb))] <- IMap.toList cs
  , TAbs i (TMatch j (MatchIntegral m df))  <- zb
  , i == j
  = (dcase (ipat r) <$> IMap.toList m) ++ dfcase df
  | MatchData r m df <- b
  = (dcase (dpat r) . fmap snd <$> IMap.toList m) ++ dfcase df
  | MatchRequest hs <- b = denormalizeHandler hs
  where
  dfcase (Just d)
    = [Term.MatchCase (UnboundP ()) Nothing $ denormalize d]
  dfcase Nothing = []

  dcase p (t, br) = Term.MatchCase (p n t) Nothing dbr
   where (n, dbr) = denormalizeBranch br

  ipat r _ i
    | r == Ty.natRef = NatP () $ fromIntegral i
    | otherwise = IntP () $ fromIntegral i
  dpat r n t = ConstructorP () r t (replicate n $ VarP ())

denormalizeBranch (TAbs v br) = (n+1, ABT.abs v dbr)
 where (n, dbr) = denormalizeBranch br
denormalizeBranch tm = (0, denormalize tm)

denormalizeHandler
  :: Var v
  => IntMap (IntMap ([Mem], ANormal v))
  -> [Term.MatchCase () (Term.Term0 v)]
denormalizeHandler cs = dcs
  where
  dcs = IMap.foldMapWithKey rf cs
  rf r rcs = IMap.foldMapWithKey (cf $ backReference r) rcs
  cf r t b = [ Term.MatchCase
                 (EffectBindP () r t (replicate n $ VarP ()) (VarP ()))
                 Nothing
                 db
             ]
   where (n, db) = denormalizeBranch (snd b)

backReference :: Int -> Reference
backReference = error "backReference"

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
