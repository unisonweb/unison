{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The MCode verifier (Unison.Runtime.MCode.Verify): everything the
-- compiler emits for a program verifies, and code with one thing changed
-- about it (a slot index, an arity, a match arm's field count, a Pack's
-- type) is rejected with the rule that caught it.
module Unison.Test.Runtime.MCode.Verify (test) where

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Word (Word64)
import EasyTest
import Unison.Builtin.Decls qualified as Ty
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF
  ( Cacheability (..),
    Code (..),
    lamLift,
    superNormalize,
  )
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.JIT qualified as JIT
import Unison.Runtime.MCode
import Unison.Runtime.MCode.Verify
import Unison.Runtime.Machine
  ( CCache (..),
    Combs,
    baseCCache,
    cacheAdd,
    combInfoOf,
    verifyEnvFrom,
  )
import Unison.Runtime.Pattern (builtinDataSpec, splitPatterns)
import Unison.Runtime.TypeTags (PackedTag (..))
import Unison.Term (unannotate)
import Unison.Test.Common (tm)
import Unison.Util.EnumContainers qualified as EC

mainRef :: Reference
mainRef = Builtin "main"

-- | Compiles a term to MCode through the code cache (which verifies it on
-- the way in) and hands back the main group, its combinators and the
-- verifier's view of the cache.
compiled :: String -> IO (VerifyEnv, Word64, Combs)
compiled s = do
  jit <- JIT.newJIT =<< JIT.readConfig
  cc <- baseCCache jit False
  _ <- cacheAdd (fmap (\g -> CodeRep g Uncacheable) <$> (mainRef, superNormalize ll) : fmap (fmap superNormalize) ctx) cc
  rtm <- readTVarIO (refTm cc)
  rty <- readTVarIO (refTy cc)
  src <- readTVarIO (srcCombs cc)
  let grp = rtm Map.! mainRef
      env =
        verifyEnvFrom
          (flip Map.lookup rty)
          (flip Map.lookup rtm)
          (\(CIx _ g i) -> EC.lookup g src >>= EC.lookup i >>= combInfoOf)
  pure (env, grp, maybe mempty id (EC.lookup grp src))
  where
    (ll, _, _, ctx, _) =
      lamLift mempty
        . splitPatterns builtinDataSpec
        . unannotate
        $ tm s

-- | Changes the entry section of every combinator of the group.
mapEntries :: (GSection CombIx -> GSection CombIx) -> Combs -> Combs
mapEntries f = fmap \(Comb (LamI a fs s c)) -> Comb (LamI a fs (f s) c)

-- | Changes every arm of a branch.
mapArms :: (Arm CombIx -> Arm CombIx) -> GBranch CombIx -> GBranch CombIx
mapArms f = \case
  Test1 v a df -> Test1 v (f a) df
  Test2 v a w b df -> Test2 v (f a) w (f b) df
  TestW df m -> TestW df (fmap f m)
  br -> br

-- | Replaces the first match section a function accepts, anywhere in the
-- section, keeping the rest.
mapFirstMatch :: (GSection CombIx -> Maybe (GSection CombIx)) -> GSection CombIx -> GSection CombIx
mapFirstMatch f = fst . go
  where
    go s = case f s of
      Just s' -> (s', True)
      Nothing -> case s of
        Ins i rest -> let (rest', done) = go rest in (Ins i rest', done)
        Let bnd cix fs body cell ->
          let (bnd', done) = go bnd
           in if done then (Let bnd' cix fs body cell, True) else let (body', d2) = go body in (Let bnd cix fs body' cell, d2)
        DMatch r i br -> let (br', done) = goB br in (DMatch r i br', done)
        NMatch r i br -> let (br', done) = goB br in (NMatch r i br', done)
        Match i br -> let (br', done) = goB br in (Match i br', done)
        _ -> (s, False)
    goB = \case
      Test1 v (Arm n cv) df -> let (cv', d1) = go cv in if d1 then (Test1 v (Arm n cv') df, True) else let (df', d2) = go df in (Test1 v (Arm n cv) df', d2)
      Test2 v (Arm n cv) w (Arm k cw) df -> let (cv', d1) = go cv in if d1 then (Test2 v (Arm n cv') w (Arm k cw) df, True) else let (cw', d2) = go cw in if d2 then (Test2 v (Arm n cv) w (Arm k cw') df, True) else let (df', d3) = go df in (Test2 v (Arm n cv) w (Arm k cw) df', d3)
      TestW df cs -> let (df', d1) = go df in (TestW df' cs, d1)
      br -> (br, False)

-- | Replaces the first instruction a predicate accepts, anywhere in the
-- section, keeping the rest.
mapFirstInstr :: (GInstr CombIx -> Maybe (GInstr CombIx)) -> GSection CombIx -> GSection CombIx
mapFirstInstr f = fst . go
  where
    go = \case
      Ins i rest
        | Just i' <- f i -> (Ins i' rest, True)
        | otherwise -> let (rest', done) = go rest in (Ins i rest', done)
      Let bnd cix fs body cell ->
        let (bnd', done) = go bnd
         in if done then (Let bnd' cix fs body cell, True) else let (body', d2) = go body in (Let bnd cix fs body' cell, d2)
      DMatch r i br -> let (br', done) = goB br in (DMatch r i br', done)
      NMatch r i br -> let (br', done) = goB br in (NMatch r i br', done)
      Match i br -> let (br', done) = goB br in (Match i br', done)
      s -> (s, False)
    goB = \case
      Test1 v (Arm n cv) df -> let (cv', d1) = go cv in if d1 then (Test1 v (Arm n cv') df, True) else let (df', d2) = go df in (Test1 v (Arm n cv) df', d2)
      Test2 v (Arm n cv) w (Arm k cw) df -> let (cv', d1) = go cv in if d1 then (Test2 v (Arm n cv') w (Arm k cw) df, True) else let (cw', d2) = go cw in if d2 then (Test2 v (Arm n cv) w (Arm k cw') df, True) else let (df', d3) = go df in (Test2 v (Arm n cv) w (Arm k cw) df', d3)
      TestW df cs -> let (df', d1) = go df in (TestW df' cs, d1)
      br -> (br, False)

expectRule :: Rule -> Either VerifyError () -> Test ()
expectRule rule = \case
  Left e | veRule e == rule -> ok
  Left e -> crash ("rejected for " ++ show (veRule e) ++ " rather than " ++ show rule ++ ": " ++ veDetail e)
  Right () -> crash ("accepted; expected a rejection for " ++ show rule)

-- a program with a Pack of a builtin type, a Let-bound call, a prim op, a
-- data match that binds fields (h), one that binds none (the if) and a
-- match on an unboxed value in main's own frame (r)
program :: String
program =
  "let\n\
  \  f x = ##Nat.+ x 1\n\
  \  g y = match y with\n\
  \    0 -> ##Nat.drop y 1\n\
  \    _ -> f y\n\
  \  h p = match p with (a, b) -> ##Nat.+ a b\n\
  \  q = h (f 1, g 2)\n\
  \  r = match q with\n\
  \    0 -> 1\n\
  \    _ -> q\n\
  \  if (##Nat.== (f 2) r) then () else ##bug ()"

test :: Test ()
test =
  scope "mcode.verify" . tests $
    [ scope "emitted code verifies" do
        (env, grp, combs) <- io (compiled program)
        expectEqual (Right ()) (verifyGroup env mainRef grp combs),
      scope "a slot index past the frame is caught" do
        (env, grp, combs) <- io (compiled program)
        let broken = mapEntries (mapFirstInstr \case Prim2 op i j -> Just (Prim2 op (i + 1000) j); _ -> Nothing) combs
        expectRule SlotOutOfFrame (verifyGroup env mainRef grp broken),
      scope "a Pack carrying another type's tag is caught" do
        (env, grp, combs) <- io (compiled program)
        let broken = mapEntries (mapFirstInstr \case Pack r (PackedTag t) as -> Just (Pack r (PackedTag (t + 65536)) as); _ -> Nothing) combs
        expectRule PackTag (verifyGroup env mainRef grp broken),
      scope "a match arm binding more fields than the frame holds is caught" do
        (env, grp, combs) <- io (compiled program)
        -- an arm's count is what its arm runs at: one past the frame is
        -- a frame the combinator never declared
        let broken = mapEntries (mapFirstMatch \case DMatch r i br -> Just (DMatch r i (mapArms (\(Arm n s) -> Arm (n + 1000) s) br)); _ -> Nothing) combs
        expectRule FrameTooSmall (verifyGroup env mainRef grp broken),
      scope "an arm of a match on an unboxed value binding fields is caught" do
        (env, grp, combs) <- io (compiled program)
        let broken = mapEntries (mapFirstMatch \case Match i br -> Just (Match i (mapArms (\(Arm n s) -> Arm (n + 1) s) br)); NMatch r i br -> Just (NMatch r i (mapArms (\(Arm n s) -> Arm (n + 1) s) br)); _ -> Nothing) combs
        expectRule MatchArm (verifyGroup env mainRef grp broken),
      scope "a call with the wrong argument count is caught" do
        (env, grp, combs) <- io (compiled program)
        let broken = mapEntries (mapCalls \case Call sk cix c args -> Call sk cix c (VArgN (primArrayFromList (replicate (countArgs args + 1) 0))); s -> s) combs
        expectRule CallArity (verifyGroup env mainRef grp broken),
      scope "a call to a missing combinator is caught" do
        (env, grp, combs) <- io (compiled program)
        let broken = mapEntries (mapCalls \case Call sk (CIx r g i) c args -> Call sk (CIx r (g + 100000) i) c args; s -> s) combs
        expectRule UnknownReference (verifyGroup env mainRef grp broken),
      scope "a frame larger than declared is caught" do
        (env, grp, combs) <- io (compiled program)
        let broken = fmap (\(Comb (LamI a _ s c)) -> Comb (LamI a 0 s c)) combs
        expectRule FrameTooSmall (verifyGroup env mainRef grp broken),
      scope "a value with any field count is accepted" do
        -- a Data value's field count is whatever it was built with: every
        -- reader takes the count from the object (design.md, "What native
        -- code trusts about MCode")
        (env, _, _) <- io (compiled program)
        expectEqual (Right ()) (verifyValue env (ANF.Data Ty.unitRef 0 [ANF.BLit (ANF.Text "x")])),
      scope "a partial application of too many arguments is caught" do
        (env, _, _) <- io (compiled program)
        -- main's group 0 is the main combinator, of arity 0
        expectRule ValueArity (verifyValue env (ANF.Partial (ANF.GR mainRef 0) [ANF.BLit (ANF.Text "x")]))
    ]
  where
    -- the first Call anywhere in the section
    mapCalls f = mapFirstCall f
    mapFirstCall f = fst . go
      where
        go = \case
          s@Call {} -> (f s, True)
          Ins i rest -> let (rest', done) = go rest in (Ins i rest', done)
          Let bnd cix fs body cell ->
            let (bnd', done) = go bnd
             in if done then (Let bnd' cix fs body cell, True) else let (body', d2) = go body in (Let bnd cix fs body' cell, d2)
          DMatch r i br -> let (br', done) = goB br in (DMatch r i br', done)
          NMatch r i br -> let (br', done) = goB br in (NMatch r i br', done)
          Match i br -> let (br', done) = goB br in (Match i br', done)
          s -> (s, False)
        goB = \case
          Test1 v (Arm n cv) df -> let (cv', d1) = go cv in if d1 then (Test1 v (Arm n cv') df, True) else let (df', d2) = go df in (Test1 v (Arm n cv) df', d2)
          Test2 v (Arm n cv) w (Arm k cw) df -> let (cv', d1) = go cv in if d1 then (Test2 v (Arm n cv') w (Arm k cw) df, True) else let (cw', d2) = go cw in if d2 then (Test2 v (Arm n cv) w (Arm k cw') df, True) else let (df', d3) = go df in (Test2 v (Arm n cv) w (Arm k cw) df', d3)
          TestW df cs -> let (df', d1) = go df in (TestW df' cs, d1)
          br -> (br, False)
