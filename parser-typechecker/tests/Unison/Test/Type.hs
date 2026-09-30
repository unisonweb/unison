{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Type where

import Control.Monad.State.Strict (runState, state)
import Data.ByteString qualified as BS
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Either (isLeft, isRight)
import Data.Map.Strict qualified as Map
import EasyTest
import U.Codebase.Reference qualified as Stored
import U.Codebase.Sqlite.Serialization qualified as Serialization
import U.Codebase.Sqlite.Term.Format qualified as Format
import U.Codebase.Term qualified as StoredTerm
import U.Core.ABT qualified as StoredABT
import Unison.ABT qualified as ABT
import Unison.Codebase.SqliteCodebase.Conversions qualified as Convert
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.KindInference qualified as KindInference
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term qualified as Term
import Unison.Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Variance qualified as Variance
import Unison.Var qualified as Var

infixr 1 -->

(-->) :: (Ord v) => Type v () -> Type v () -> Type v ()
(-->) a b = arrow () a b

test :: Test ()
test =
  scope "type" $
    tests
      [ scope "unArrows" $
          let x = arrow () (builtin () "a") (builtin () "b") :: Type Symbol ()
           in case x of
                Arrows' [i, o] ->
                  expect (i == builtin () "a" && o == builtin () "b")
                _ -> crash "unArrows (a -> b) did not return a spine of [a,b]",
        scope "implicit-arrows" implicitArrows,
        scope "implicit-effects" implicitEffects,
        scope "implicit-subtyping" implicitSubtyping,
        scope "subtype" $ do
          let v = Var.named "a"
              v2 = Var.named "b"
              vt = var () v
              vt2 = var () v2
              x = forAll () v (nat () --> effect () [vt, builtin () "eff"] (nat ())) :: Type Symbol ()
              y = forAll () v2 (nat () --> effect () [vt2] (nat ())) :: Type Symbol ()
          expect . not $ Typechecker.isSubtype x y
      ]

implicitArrows :: Test ()
implicitArrows =
  let a = Var.named "a" :: Symbol
      b = Var.named "b"
      c = Var.named "c"
      av = var () a
      bv = var () b
      cv = var () c
      qualified = implicitArrow () av (arrow () bv cv)
      mixed = arrow () av (implicitArrow () bv cv)
      concrete = implicitArrow () (nat ()) (arrow () (text ()) (nat ())) :: Type Symbol ()
      printType = TypePrinter.prettyStr 80 PPE.empty
      hash = Hashing.typeToReference
      stored = Convert.type1to2' localReference concrete :: Format.Type
      dummy = StoredABT.tm () (StoredTerm.Nat 0) :: Format.Term
      encode typ = runPutS (Serialization.putTermAndType (dummy, typ))
      checkKind typ = do
        kinds <- KindInference.inferDecls PPE.empty mempty
        KindInference.kindCheckAnnotations PPE.empty kinds (Term.ann () (Term.nat () 0) typ)
      legacy = Convert.type1to2' localReference (arrow () (nat ()) (text ()) :: Type Symbol ())
      -- Captured using the trunk serializer before adding the implicit-arrow tag.
      legacyBytes = BS.pack [11, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1]
   in tests
        [ scope "distinct-hash" $ expect (hash concrete /= hash (arrow () (nat ()) (arrow () (text ()) (nat ())))),
          scope "alpha-equivalent-hash" $
            expect (hash (forAll () a (implicitArrow () av av)) == hash (forAll () b (implicitArrow () bv bv))),
          scope "mixed-printing" $ expectEqual (printType mixed) "a -> (b => c)",
          scope "leading-printing" $ expectEqual (printType qualified) "a => b -> c",
          scope "multiple-premises" $ expectEqual (printType (implicitArrow () av (implicitArrow () bv cv))) "(a, b) => c",
          scope "argument-printing" $ expectEqual (printType (arrow () (implicitArrow () av bv) cv)) "(a => b) -> c",
          scope "explicit-spine" $ expectEqual (unArrows mixed) (Just [av, implicitArrow () bv cv]),
          scope "physical-arity" $ expectEqual (arity mixed) 2,
          scope "effectful-arity" $ expectEqual (arityIgnoringEffects (implicitArrow () av (effect () [bv] (arrow () bv cv)))) 2,
          scope "is-function" $ expect (isArrow (forAll () a qualified)),
          scope "function-result" $ expectEqual (functionResult mixed) (Just cv),
          scope "edit-function-result" $ expectEqual (editFunctionResult (const av) mixed) (arrow () av (implicitArrow () bv av)),
          scope "variance" $
            expectEqual
              (Variance.collectVariance mempty mempty mixed)
              (Map.fromList [(a, [Variance.Negative]), (b, [Variance.Negative]), (c, [Variance.Positive])]),
          scope "positive-effects" $
            expectEqual
              (removePureEffects False (forAll () b (implicitArrow () av (effect () [bv] cv))))
              (implicitArrow () av cv),
          scope "negative-effects" $
            let typ = forAll () b (implicitArrow () (effect () [bv] av) cv)
             in expectEqual (removePureEffects False typ) typ,
          scope "hashing-roundtrip" $
            let term = Term.lam () ((), a) (Term.lam () ((), b) (Term.var () a))
                (warnings, hashed) = Hashing.hashTermComponents (Map.singleton c (term, concrete, ()))
             in do
                  expect (null warnings)
                  expectEqual ((\(_, tm, ty, _) -> (tm, ty)) <$> Map.lookup c hashed) (Just (Term.ann () term concrete, concrete)),
          scope "storage-roundtrip" $ expectEqual (ABT.amap (const ()) (Convert.type2to1' memoryReference stored)) concrete,
          scope "well-kinded" $ expect (isRight (checkKind concrete)),
          scope "higher-kinded-premise" $ expect (isLeft (checkKind (implicitArrow () (ref () listRef) (nat ()) :: Type Symbol ()))),
          scope "binary-roundtrip" $ expectEqual (snd <$> runGetS Serialization.getTermAndType (encode stored)) (Right stored),
          scope "legacy-encoding" $ expectEqual (encode legacy) legacyBytes,
          scope "legacy-decoding" $ expectEqual (snd <$> runGetS Serialization.getTermAndType legacyBytes) (Right legacy)
        ]
  where
    localReference = \case
      Reference.Builtin "Nat" -> Stored.ReferenceBuiltin 0
      Reference.Builtin "Text" -> Stored.ReferenceBuiltin 1
      r -> error ("Unexpected fixture reference: " <> show r)
    memoryReference = \case
      Stored.ReferenceBuiltin 0 -> Reference.Builtin "Nat"
      Stored.ReferenceBuiltin 1 -> Reference.Builtin "Text"
      r -> error ("Unexpected fixture reference: " <> show r)

implicitEffects :: Test ()
implicitEffects =
  let n = nat () :: Type Symbol ()
      t = text ()
      chain = implicitArrow () n (implicitArrow () t n)
      fresh = state (\next -> (Var.freshenId next (Var.named "e"), next + 1))
      addEffects typ = runState (existentializeArrows fresh typ) 0
      explicitEffect = effect () [builtin () "Test.Ability"] n
   in tests
        [ scope "one-effect-row-per-constraint-context" $ do
            let (result, count) = addEffects chain
            expectEqual count 1
            expectEqual (removeAllEffectVars result) chain
            case result of
              ImplicitArrow' _ (ImplicitArrow' _ (Effect1' _ _)) -> ok
              _ -> crash "effect rows must not split the constraint context",
          scope "mixed-spine" $ do
            let typ = arrow () t chain
                (result, count) = addEffects typ
            expectEqual count 2
            expectEqual (removeAllEffectVars result) typ,
          scope "explicit-effects-preserved" $ do
            let typ = implicitArrow () t explicitEffect
            expectEqual (addEffects typ) (typ, 0),
          scope "purify-constraint-context" $ do
            expectEqual (purifyArrows chain) (implicitArrow () n (implicitArrow () t (effect () [] n)))
            expectEqual (removeEmptyEffects (purifyArrows chain)) chain,
          scope "purify-preserves-explicit-effects" $
            let typ = implicitArrow () t explicitEffect
             in expectEqual (purifyArrows typ) typ
        ]

implicitSubtyping :: Test ()
implicitSubtyping =
  let a = Var.named "a" :: Symbol
      av = var () a
      n = nat ()
      identity = forAll () a (arrow () av av)
      monoIdentity = arrow () n n
      implicit = implicitArrow ()
      pairs ctor =
        [ (ctor n n, ctor n n),
          (ctor monoIdentity n, ctor identity n),
          (ctor identity n, ctor monoIdentity n),
          (ctor n identity, ctor n monoIdentity),
          (ctor n monoIdentity, ctor n identity),
          (forAll () a av, ctor identity n),
          (ctor n identity, forAll () a av)
        ]
   in tests
        [ scope "no-silent-coercion" $ do
            expect (not (Typechecker.isSubtype (implicit n n) (arrow () n n)))
            expect (not (Typechecker.isSubtype (arrow () n n) (implicit n n)))
            expect (not (Typechecker.isSubtype (implicit n n) n))
            expect (not (Typechecker.isSubtype n (implicit n n))),
          scope "same-variance-as-functions" $
            mapM_
              (\((i, j), (x, y)) -> expectEqual (Typechecker.isSubtype i j) (Typechecker.isSubtype x y))
              (zip (pairs implicit) (pairs (arrow ())))
        ]
