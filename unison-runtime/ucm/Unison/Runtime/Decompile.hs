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

import Data.List (unsnoc)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (singleton)
import Data.Text qualified as DT
import Data.Vector qualified as V
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
import Unison.Runtime.MCode (CombIx (..), shapeFields)
import Unison.Runtime.Stack
  ( Closure (..),
    Foreign (..),
    HashAlgorithm (..),
    UnboxedTypeTag (..),
    Val (..),
    foreignRef,
    pattern BytesC,
    pattern DataC,
    pattern ListC,
    pattern PApV,
    pattern TextC,
  )
import Unison.Runtime.TypeNumbers qualified as TN
import Unison.Runtime.TypeTags qualified as TT
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
  ( booleanRef,
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
  -- the closure a specialized group was made from (CCache's
  -- specializations), for showing it as @specialize f x@
  (Reference -> Maybe CombIx) ->
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Val ->
  DecompResult v
decompile spec backref topTerms = \case
  CharVal c -> pure (char () c)
  NatVal n -> pure (nat () n)
  IntVal i -> pure (int () (fromIntegral i))
  DoubleVal f -> pure (float () f)
  Val i (UnboxedTypeTag tt) ->
    err (BadUnboxed tt) . nat () $ fromIntegral $ i
  Val _u clos -> case clos of
    DataC t []
      | TT.isBooleanTag t -> tag2bool (maskTags t)
    (DataC t [b])
      | TT.isAnyTag t ->
          app () (builtin () "Any.Any") <$> decompile spec backref topTerms b
    (DataC t vs) ->
      -- the type's reference, from the process's registry
      apps' (con (TN.typeReference (TT.typeTag t)) (maskTags t)) <$> traverse (decompile spec backref topTerms) vs
    (RecordC shape vals) -> do
      vs' <- traverse (decompile spec backref topTerms) vals
      -- The field names are carried by the record's shape, in the same
      -- ascending order as its slots.
      pure . Term.record () . Map.fromList $
        zip (V.toList (shapeFields shape)) (V.toList vs')
    (PApV cix _ vs) -> pap cix vs
    (PAp (CIx rf _ _) _ _) ->
      err (BadPAp rf) $ bug "<Unknown>"
    BlackHole -> err Exn $ bug "<Exception>"
    (Captured {}) -> err Cont $ bug "<Continuation>"
    (Affine {}) -> err Aff $ bug "<Affine>"
    (TextC t) -> pure $ text () (Text.toText t)
    (BytesC b) -> pure $ decompileBytes b
    (ListC s) ->
      list' () . Seq.fromList <$> traverse (decompile spec backref topTerms) (toList s)
    (Foreign f) ->
      decompileForeign spec backref topTerms f
  where
    -- a specialized closure holds the arguments of the closure it was made
    -- from, which is shown as that closure without its last argument,
    -- specialized on that argument
    pap (CIx rf rt k) vs
      | Just ocix <- spec rf,
        Just (fs, x) <- unsnoc vs = do
          f <- pap ocix fs
          x <- decompile spec backref topTerms x
          pure $ apps' (builtin () "specialize") [f, x]
      | rf == Builtin "jumpCont" =
          err Cont $ bug "<Continuation>"
      | Just t <- topTerms rt k =
          Term.etaReduceEtaVars . substitute t
            <$> traverse (decompile spec backref topTerms) vs
      | k > 0,
        Just _ <- topTerms rt 0 =
          err (UnkLocal rf k) $ bug "<Unknown>"
      | Builtin nm <- rf =
          apps' (builtin () nm) <$> traverse (decompile spec backref topTerms) vs
      | otherwise = err (UnkComb rf) $ ref () rf

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
  -- the closure a specialized group was made from (CCache's
  -- specializations), for showing it as @specialize f x@
  (Reference -> Maybe CombIx) ->
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Foreign ->
  DecompResult v
decompileForeign spec backref topTerms = \case
  WrapHashAlgorithm h -> pure $ decompileHashAlgorithm h
  WrapReferent l ->
    pure . termLink () $ case l of
      Ref r -> maybe l Ref $ backref r
      _ -> l
  WrapReference l -> pure $ typeLink () l
  WrapArray a ->
    app () (ref () iarrayFromListRef) . list ()
      <$> traverse (decompile spec backref topTerms) (toList a)
  WrapByteArray a ->
    pure $
      app
        ()
        (ref () ibarrayFromBytesRef)
        (decompileBytes . By.fromWord8s $ byteArrayToList a)
  WrapMap m -> do
    let decompileEntry k v = pair <$> decompile spec backref topTerms k <*> decompile spec backref topTerms v
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
