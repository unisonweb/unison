{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Decompile
  ( decompile,
    DecompResult,
    DecompError (..),
  )
where

import Data.Map qualified as Map
import Data.Set (singleton)
import Data.Text qualified as DT
import Numeric.Natural (Natural)
import Unison.ABT (substs)
import Unison.Builtin.Decls qualified as DD
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Prelude
import Unison.Reference (Reference, pattern Builtin)
import Unison.Referent (pattern Ref)
import Unison.Referent qualified as Referent
import Unison.Runtime.ANF (maskTags)
import Unison.Runtime.Array (byteArrayToList)
import Unison.Runtime.IOSource (iarrayFromListRef, ibarrayFromBytesRef)
import Unison.Runtime.MCode (CombIx (..))
import Unison.Runtime.Stack
  ( Closure (..),
    Foreign (..),
    HashAlgorithm (..),
    UnboxedTypeTag (..),
    Val (..),
    foreignRef,
    pattern DataC,
    pattern PApV,
  )
import Unison.Term
  ( Term,
    app,
    apps',
    boolean,
    builtin,
    char,
    constructor,
    float,
    int,
    list,
    list',
    nat,
    ref,
    termLink,
    text,
    typeLink,
    pattern LamNamed',
  )
import Unison.Term qualified as Term
import Unison.Type
  ( anyRef,
    booleanRef,
  )
import Unison.Util.Bytes qualified as By
import Unison.Util.Text qualified as Text
import Unison.Var (Var)
import Prelude hiding (lines)

con :: (Var v) => Reference -> Word64 -> Term v ()
con rf ct = constructor () (ConstructorReference rf $ fromIntegral ct)

bug :: (Var v) => Text -> Term v ()
bug msg = app () (builtin () "bug") (text () msg)

err :: DecompError -> a -> (Set DecompError, a)
err err x = (singleton err, x)

data DecompError
  = BadBool !Word64
  | BadUnboxed !UnboxedTypeTag
  | BadForeign !Reference
  | BadData !Reference
  | BadPAp !Reference
  | UnkComb !Reference
  | UnkLocal !Reference !Word64
  | Cont
  | Exn
  | Aff
  deriving (Eq, Ord)

type DecompResult v = (Set DecompError, Term v ())

decompile ::
  forall v.
  (Var v) =>
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Val ->
  DecompResult v
decompile backref topTerms = \case
  CharVal c -> pure (char () c)
  NatVal n -> pure (nat () n)
  IntVal i -> pure (int () (fromIntegral i))
  DoubleVal f -> pure (float () f)
  Val i (UnboxedTypeTag tt) ->
    err (BadUnboxed tt) . nat () $ fromIntegral $ i
  Val _u clos -> case clos of
    DataC rf (maskTags -> ct) []
      | rf == booleanRef -> tag2bool ct
    (DataC rf _ [b])
      | rf == anyRef ->
          app () (builtin () "Any.Any") <$> decompile backref topTerms b
    (DataC rf (maskTags -> ct) vs) ->
      apps' (con rf ct) <$> traverse (decompile backref topTerms) vs
    (RecordC _rr _vals) -> error "TODO: decompilation for records is unimplemented"
    (PApV (CIx rf rt k) _ vs)
      | rf == Builtin "jumpCont" ->
          err Cont $ bug "<Continuation>"
      | Just t <- topTerms rt k ->
          Term.etaReduceEtaVars . substitute t
            <$> traverse (decompile backref topTerms) vs
      | k > 0,
        Just _ <- topTerms rt 0 ->
          err (UnkLocal rf k) $ bug "<Unknown>"
      | Builtin nm <- rf ->
          apps' (builtin () nm) <$> traverse (decompile backref topTerms) vs
      | otherwise -> err (UnkComb rf) $ ref () rf
    (PAp (CIx rf _ _) _ _) ->
      err (BadPAp rf) $ bug "<Unknown>"
    BlackHole -> err Exn $ bug "<Exception>"
    (Captured {}) -> err Cont $ bug "<Continuation>"
    (Affine {}) -> err Aff $ bug "<Affine>"
    (Foreign f) ->
      decompileForeign backref topTerms f

tag2bool :: (Var v) => Word64 -> DecompResult v
tag2bool 0 = pure (boolean () False)
tag2bool 1 = pure (boolean () True)
tag2bool n = err (BadBool n) $ con booleanRef n

substitute :: (Var v) => Term v () -> [Term v ()] -> Term v ()
substitute = align []
  where
    align vts (LamNamed' v bd) (t : ts) = align ((v, t) : vts) bd ts
    align vts tm [] = substs vts tm
    -- this should not happen
    align vts tm ts = apps' (substs vts tm) ts

decompileForeign ::
  (Var v) =>
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Foreign ->
  DecompResult v
decompileForeign backref topTerms = \case
  WrapText t -> pure $ text () (Text.toText t)
  WrapBytes b -> pure $ decompileBytes b
  WrapHashAlgorithm h -> pure $ decompileHashAlgorithm h
  WrapReferent l ->
    pure . termLink () $ case l of
      Ref r -> maybe l Ref $ backref r
      _ -> l
  WrapReference l -> pure $ typeLink () l
  WrapArray a ->
    app () (ref () iarrayFromListRef) . list ()
      <$> traverse (decompile backref topTerms) (toList a)
  WrapByteArray a ->
    pure $
      app
        ()
        (ref () ibarrayFromBytesRef)
        (decompileBytes . By.fromWord8s $ byteArrayToList a)
  WrapSeq s ->
    list' () <$> traverse (decompile backref topTerms) s
  WrapMap m -> do
    let decompileEntry k v = pair <$> decompile backref topTerms k <*> decompile backref topTerms v
    kvs <- traverse (uncurry decompileEntry) (Map.toList m)
    pure $ app () map_fromList (list () kvs)
  WrapNatural n ->
    pure $ app () naturalFromText (text () $ DT.pack (show (n :: Natural)))
  WrapInteger i ->
    pure $ app () integerFromText (text () $ DT.pack (show (i :: Integer)))
  fo -> err (BadForeign r) $ bug text
    where
      r = foreignRef fo
      text
        | Builtin name <- r = "<" <> name <> ">"
        | otherwise = "<Foreign>"

naturalFromText :: (Var v) => Term v ()
naturalFromText =
  case Referent.fromText "##Natural.unsafeFromText" of
    Just r -> Term.fromReferent () r
    Nothing -> error "Natural_unsafeFromText"

integerFromText :: (Var v) => Term v ()
integerFromText =
  case Referent.fromText "##Integer.unsafeFromText" of
    Just r -> Term.fromReferent () r
    Nothing -> error "Integer_unsafeFromText"

map_fromList :: (Var v) => Term v ()
map_fromList =
  case Referent.fromText "#apmvhl40hl48q1s7383g5ev3sh7td8qo374t87bchpnu24sccmnvm13e2a1q0f2p1prm2uk9prfpg598dc9jo23iagact6gmi18vta8" of
    Just r -> Term.fromReferent () r
    Nothing -> error "Map_fromList"

pair :: (Var v) => Term v () -> Term v () -> Term v ()
pair a b =
  Term.apps'
    (Term.fromReferent () DD.pairCtorRef)
    [ a,
      Term.apps' (Term.fromReferent () DD.pairCtorRef) [b, Term.fromReferent () DD.unitCtorRef]
    ]

decompileBytes :: (Var v) => By.Bytes -> Term v ()
decompileBytes =
  app () (builtin () $ fromString "Bytes.fromList")
    . list ()
    . fmap (nat () . fromIntegral)
    . By.toWord8s

decompileHashAlgorithm :: (Var v) => HashAlgorithm -> Term v ()
decompileHashAlgorithm (HashAlgorithm r _) = ref () r
