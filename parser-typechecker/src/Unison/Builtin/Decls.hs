-- | This module contains a bunch of manually-constructor data/effect decls needed by type signatures of builtin terms.
-- It would be great if we could parse them as Unison source, but we have a bootstrapping problem.
--
-- Side note: it looks like some other things have sneaked in here, various utilities
-- and definitions related to the builtin types; but I think they should probably move.
module Unison.Builtin.Decls
  ( builtinDataDecls,
    builtinEffectDecls,

    -- * for type signatures of builtin terms
    exceptionType,
    optionalType,
    unitType,
    pairType,
    failureType,
    eitherType,
    fileModeType,
    bufferModeType,
    seekModeType,
    stdHandleType,

    -- * for Runtime
    exceptionRef,
    eitherRef,
    eitherLeftId,
    eitherRightId,
    failureRef,
    arithmeticFailureRef,
    arrayFailureRef,
    fileModeRef,
    ioFailureRef,
    miscFailureRef,
    runtimeFailureRef,
    stmFailureRef,
    threadKilledFailureRef,
    tlsFailureRef,
    pairRef,
    unitRef,
    optionalRef,
    noneId,
    someId,
    seekModeRef,
    seqViewRef,
    seqViewEmpty,
    seqViewElem,
    stdHandleRef,
    bufferModeRef,
    bufferModeBlockBufferingId,
    bufferModeNoBufferingId,
    bufferModeLineBufferingId,
    bufferModeSizedBlockBufferingId,
    unitTerm,
    tupleTerm,
    pattern TupleTerm',

    -- * for UCM
    testResultType,
    testResultRefId,

    -- ** metadata

    -- isPropagatedRef,
    isPropagatedRefId,
    isPropagatedConstructorId,
    isTestRefId,
    isTestConstructorId,

    -- ** testing
    okConstructorId,
    okConstructorReferent,
    failConstructorId,
    failConstructorReferent,

    -- ** parsing / printing
    docRef,
    docRefId,
    docBlobId,
    docEvaluateId,
    docLinkId,
    docJoinId,
    docSignatureId,
    docSourceId,
    pattern Doc,
    pattern DocRefId,
    pattern DocBlobId,
    pattern DocJoinId,
    pattern DocLink,
    pattern DocSource,
    pattern DocSignature,
    pattern DocEvaluate,
    linkRefId,
    linkTermId,
    linkTypeId,
    thunkArgType,
    pairRefId,
    pairCtorRef,
    pattern Rewrites',
    pattern RewriteCase',
    pattern RewriteTerm',
    pattern RewriteSignature',
    pattern TuplePattern,
    pattern UnitRefId,
    unitRefId,
    unitCtorRef,

    -- *** terms and types
    pattern DocBlob,
    pattern DocJoin,
    pattern LinkTerm,
    pattern LinkType,
    -- pattern TuplePattern,
    -- pattern TupleTerm',
    pattern TupleType',
    delayTerm,
    forceTerm,

    -- ** Server.Doc
    pattern EitherLeft',
    pattern EitherRight',
    pattern OptionalNone',
    pattern OptionalSome',

    -- ** rewrites
    rewrites,
    rewriteTerm,
    rewriteCase,
    rewriteType,
  )
where

import Control.Lens (over, _3)
import Data.List (elemIndex, find)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq)
import Data.Text (Text, unpack)
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration (..), Modifier (Structural, Unique))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hashing.V2.Convert (hashDataDecls)
import Unison.Pattern qualified as Pattern
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term, Term2)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var

lookupDeclRef :: Text -> TypeReference
lookupDeclRef = Reference.DerivedId . lookupDeclRefId

lookupDeclRefId :: Text -> TypeReferenceId
lookupDeclRefId str
  | [(_, d)] <- filter (\(v, _) -> v == Var.named str) decls = d
  | otherwise = error $ "lookupDeclRef: missing \"" ++ unpack str ++ "\""
  where
    decls = [(a, b) | (a, b, _) <- builtinDataDecls]

lookupEffectRef :: Text -> TypeReference
lookupEffectRef = Reference.DerivedId . lookupEffectRefId

lookupEffectRefId :: Text -> TypeReferenceId
lookupEffectRefId str
  | [(_, d)] <- filter (\(v, _) -> v == Var.named str) decls = d
  | otherwise = error $ "lookupEffectRef: missing \"" ++ unpack str ++ "\""
  where
    decls = [(a, b) | (a, b, _) <- builtinEffectDecls]

unitRef, pairRef, optionalRef, eitherRef :: TypeReference
unitRefId, pairRefId, optionalRefId, eitherRefId :: TypeReferenceId
(unitRef, unitRefId) = (Reference.DerivedId unitRefId, lookupDeclRefId "Unit")
(pairRef, pairRefId) = (Reference.DerivedId pairRefId, lookupDeclRefId "Tuple")
(optionalRef, optionalRefId) = (Reference.DerivedId optionalRefId, lookupDeclRefId "Optional")
(eitherRef, eitherRefId) = (Reference.DerivedId eitherRefId, lookupDeclRefId "Either")

testResultRef, linkRef, docRef, ioErrorRef, stdHandleRef :: TypeReference
failureRef, ioFailureRef, tlsFailureRef, arrayFailureRef :: TypeReference
exceptionRef, tlsSignedCertRef, tlsPrivateKeyRef :: TypeReference
isPropagatedRef, isTestRef :: TypeReference
(isPropagatedRef, isPropagatedRefId) = (Reference.DerivedId isPropagatedRefId, lookupDeclRefId "IsPropagated")

(isTestRef, isTestRefId) = (Reference.DerivedId isTestRefId, lookupDeclRefId "IsTest")

(testResultRef, testResultRefId) = (Reference.DerivedId testResultRefId, lookupDeclRefId "Test.Result")

(linkRef, linkRefId) = (Reference.DerivedId linkRefId, lookupDeclRefId "Link")

(docRef, docRefId) = (Reference.DerivedId docRefId, lookupDeclRefId "Doc")

(ioErrorRef, ioErrorRefId) = (Reference.DerivedId ioErrorRefId, lookupDeclRefId "io2.IOError")

(stdHandleRef, stdHandleRefId) = (Reference.DerivedId stdHandleRefId, lookupDeclRefId "io2.StdHandle")

(failureRef, failureRefId) = (Reference.DerivedId failureRefId, lookupDeclRefId "io2.Failure")

(exceptionRef, exceptionRefId) = (Reference.DerivedId exceptionRefId, lookupEffectRefId "Exception")

ioFailureRef = lookupDeclRef "io2.IOFailure"

tlsFailureRef = lookupDeclRef "io2.TlsFailure"

arrayFailureRef = lookupDeclRef "io2.ArrayFailure"

(tlsSignedCertRef, tlsSignedCertRefId) = (Reference.DerivedId tlsSignedCertRefId, lookupDeclRefId "io2.Tls.SignedCert")

tlsPrivateKeyRef = lookupDeclRef "io2.Tls.PrivateKey"

runtimeFailureRef, arithmeticFailureRef, miscFailureRef, stmFailureRef, threadKilledFailureRef :: TypeReference
runtimeFailureRef = lookupDeclRef "io2.RuntimeFailure"
arithmeticFailureRef = lookupDeclRef "io2.ArithmeticFailure"
miscFailureRef = lookupDeclRef "io2.MiscFailure"
stmFailureRef = lookupDeclRef "io2.STMFailure"
threadKilledFailureRef = lookupDeclRef "io2.ThreadKilledFailure"

fileModeRef, filePathRef, bufferModeRef, seekModeRef, seqViewRef :: TypeReference
(fileModeRef, fileModeRefId) = (Reference.DerivedId fileModeRefId, lookupDeclRefId "io2.FileMode")
(filePathRef, filePathRefId) = (Reference.DerivedId filePathRefId, lookupDeclRefId "io2.FilePath")
(bufferModeRef, bufferModeRefId) = (Reference.DerivedId bufferModeRefId, lookupDeclRefId "io2.BufferMode")
(seekModeRef, seekModeRefId) = (Reference.DerivedId seekModeRefId, lookupDeclRefId "io2.SeekMode")
(seqViewRef, seqViewRefId) = (Reference.DerivedId seqViewRefId, lookupDeclRefId "SeqView")

pairCtorRef, unitCtorRef :: Referent
pairCtorRef = Referent.Con (ConstructorReference pairRefId 0) CT.Data
unitCtorRef = Referent.Con (ConstructorReference unitRefId 0) CT.Data

constructorId :: TypeReferenceId -> Text -> Maybe ConstructorId
constructorId ref name = do
  (_, _, dd) <- find (\(_, r, _) -> r == ref) builtinDataDecls
  fmap fromIntegral . elemIndex name $ DD.constructorNames dd

noneId, someId, okConstructorId, failConstructorId, docBlobId, docLinkId, docSignatureId, docSourceId, docEvaluateId, docJoinId, linkTermId, linkTypeId, eitherRightId, eitherLeftId :: ConstructorId
isPropagatedConstructorId, isTestConstructorId, bufferModeNoBufferingId, bufferModeLineBufferingId, bufferModeBlockBufferingId, bufferModeSizedBlockBufferingId :: ConstructorId
seqViewEmpty, seqViewElem :: ConstructorId
noneId = Maybe.fromJust $ constructorId optionalRefId "Optional.None"
someId = Maybe.fromJust $ constructorId optionalRefId "Optional.Some"

isPropagatedConstructorId = Maybe.fromJust $ constructorId isPropagatedRefId "IsPropagated.IsPropagated"

isTestConstructorId = Maybe.fromJust $ constructorId isTestRefId "IsTest.IsTest"

okConstructorId = Maybe.fromJust $ constructorId testResultRefId "Test.Result.Ok"

failConstructorId = Maybe.fromJust $ constructorId testResultRefId "Test.Result.Fail"

docBlobId = Maybe.fromJust $ constructorId docRefId "Doc.Blob"

docLinkId = Maybe.fromJust $ constructorId docRefId "Doc.Link"

docSignatureId = Maybe.fromJust $ constructorId docRefId "Doc.Signature"

docSourceId = Maybe.fromJust $ constructorId docRefId "Doc.Source"

docEvaluateId = Maybe.fromJust $ constructorId docRefId "Doc.Evaluate"

docJoinId = Maybe.fromJust $ constructorId docRefId "Doc.Join"

linkTermId = Maybe.fromJust $ constructorId linkRefId "Link.Term"

linkTypeId = Maybe.fromJust $ constructorId linkRefId "Link.Type"

eitherRightId = Maybe.fromJust $ constructorId eitherRefId "Either.Right"

eitherLeftId = Maybe.fromJust $ constructorId eitherRefId "Either.Left"

seqViewEmpty = Maybe.fromJust $ constructorId seqViewRefId "SeqView.VEmpty"

seqViewElem = Maybe.fromJust $ constructorId seqViewRefId "SeqView.VElem"

bufferModeNoBufferingId = Maybe.fromJust $ constructorId bufferModeRefId "io2.BufferMode.NoBuffering"

bufferModeLineBufferingId = Maybe.fromJust $ constructorId bufferModeRefId "io2.BufferMode.LineBuffering"

bufferModeBlockBufferingId = Maybe.fromJust $ constructorId bufferModeRefId "io2.BufferMode.BlockBuffering"

bufferModeSizedBlockBufferingId = Maybe.fromJust $ constructorId bufferModeRefId "io2.BufferMode.SizedBlockBuffering"

okConstructorReferent, failConstructorReferent :: Referent.Referent
okConstructorReferent = Referent.Con (ConstructorReference testResultRefId okConstructorId) CT.Data
failConstructorReferent = Referent.Con (ConstructorReference testResultRefId failConstructorId) CT.Data

-- rewriteTermRef :: TypeReference
-- rewriteTermRef = lookupDeclRef "RewriteTerm"

rewriteTermRefId :: TypeReferenceId
rewriteTermRefId = lookupDeclRefId "RewriteTerm"

pattern RewriteTerm' :: Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern RewriteTerm' lhs rhs <- (unRewriteTerm -> Just (lhs, rhs))

unRewriteTerm :: Term2 vt at ap v a -> Maybe (Term2 vt at ap v a, Term2 vt at ap v a)
unRewriteTerm (Term.Apps' (Term.Constructor' (ConstructorReference r _)) [lhs, rhs])
  | r == rewriteTermRefId = Just (lhs, rhs)
unRewriteTerm _ = Nothing

rewriteCaseRef :: TypeReference
rewriteCaseRef = lookupDeclRef "RewriteCase"

rewriteCaseRefId :: TypeReferenceId
rewriteCaseRefId = lookupDeclRefId "RewriteCase"

pattern RewriteCase' :: Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern RewriteCase' lhs rhs <- (unRewriteCase -> Just (lhs, rhs))

rewriteCase :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
rewriteCase a tm1 tm2 = Term.app a (Term.app a1 (Term.constructor a1 r) tm1) tm2
  where
    a1 = ABT.annotation tm1
    r = ConstructorReference rewriteCaseRefId 0

rewriteTerm :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
rewriteTerm a tm1 tm2 = Term.app a (Term.app a1 (Term.constructor a1 r) tm1) tm2
  where
    a1 = ABT.annotation tm1
    r = ConstructorReference rewriteTermRefId 0

unRewriteCase :: Term2 vt at ap v a -> Maybe (Term2 vt at ap v a, Term2 vt at ap v a)
unRewriteCase (Term.Apps' (Term.Constructor' (ConstructorReference r _)) [lhs, rhs])
  | r == rewriteCaseRefId = Just (lhs, rhs)
unRewriteCase _ = Nothing

rewriteTypeRefId :: TypeReferenceId
rewriteTypeRefId = lookupDeclRefId "RewriteSignature"

pattern RewriteSignature' :: forall vt at ap v a. [vt] -> Type vt at -> Type vt at -> Term2 vt at ap v a
pattern RewriteSignature' vs lhs rhs <- (unRewriteSignature -> Just (vs, lhs, rhs))

rewriteType :: (Var v, Semigroup a) => a -> [v] -> Type v a -> Type v a -> Term2 v a a v a
rewriteType a vs lhs rhs =
  Term.app
    a
    (Term.constructor la (ConstructorReference rewriteTypeRefId 0))
    ( Term.ann
        a
        (Term.delay a (Term.delay a (unitTerm a)))
        (Type.foralls a vs (Type.arrow (la <> ra) lhs (Type.arrow ra rhs (unitType ra))))
    )
  where
    la = ABT.annotation lhs
    ra = ABT.annotation rhs

unRewriteSignature :: Term2 vt at ap v a -> Maybe ([vt], Type vt at, Type vt at)
unRewriteSignature
  ( Term.App'
      (Term.Constructor' (ConstructorReference r _))
      (Term.Ann' _ (Type.ForallsNamedOpt' vs (Type.Arrow' lhs (Type.Arrow' rhs _unit))))
    )
    | r == rewriteTypeRefId = Just (vs, lhs, rhs)
unRewriteSignature _ = Nothing

rewritesRefId :: TypeReferenceId
rewritesRefId = lookupDeclRefId "Rewrites"

pattern Rewrites' :: [Term2 vt at ap v a] -> Term2 vt at ap v a
pattern Rewrites' ts <- (unRewrites -> Just ts)

rewrites :: (Var v, Monoid a) => a -> [Term2 vt at ap v a] -> Term2 vt at ap v a
rewrites a [] = Term.app a (Term.constructor a (ConstructorReference rewritesRefId 0)) (tupleTerm [])
rewrites a ts@(hd : _) = Term.app a (Term.constructor a1 (ConstructorReference rewritesRefId 0)) (tupleTerm ts)
  where
    a1 = ABT.annotation hd

unRewrites :: Term2 vt at ap v a -> Maybe [Term2 vt at ap v a]
unRewrites (Term.App' (Term.Constructor' (ConstructorReference r _)) tup)
  | r == rewritesRefId, TupleTerm' ts <- tup = Just ts
unRewrites _ = Nothing

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: [(Symbol, Reference.Id, DataDeclaration Symbol ())]
builtinDataDecls = rs1 ++ rs
  where
    rs1 = case hashDataDecls $
      Map.fromList
        [ (v "Link", link)
        ] of
      Right a -> a
      Left e -> error $ "builtinDataDecls: " <> show e
    rs = case hashDataDecls $
      Map.fromList
        [ (v "Unit", unit),
          (v "Tuple", tuple),
          (v "Optional", opt),
          (v "Either", eith),
          (v "Test.Result", tr),
          (v "IsPropagated", isPropagated),
          (v "IsTest", isTest),
          (v "Doc", doc),
          (v "io2.FileMode", fmode),
          (v "io2.BufferMode", bmode),
          (v "io2.SeekMode", smode),
          (v "SeqView", seqview),
          (v "io2.IOError", ioerr),
          (v "io2.StdHandle", stdhnd),
          (v "io2.Failure", failure),
          (v "io2.TlsFailure", tlsFailure),
          (v "io2.IOFailure", ioFailure),
          (v "io2.ArrayFailure", arrayFailure),
          (v "io2.RuntimeFailure", runtimeFailure),
          (v "io2.ArithmeticFailure", arithmeticFailure),
          (v "io2.MiscFailure", miscFailure),
          (v "io2.STMFailure", stmFailure),
          (v "io2.ThreadKilledFailure", threadKilledFailure),
          (v "RewriteTerm", rewriteTerm),
          (v "RewriteSignature", rewriteType),
          (v "RewriteCase", rewriteCase),
          (v "Rewrites", rewrites)
        ] of
      Right a -> a
      Left e -> error $ "builtinDataDecls: " <> show e
    linkRef = case rs1 of
      [(_, linkRef, _)] -> linkRef
      _ -> error "builtinDataDecls: Expected a single linkRef"
    v = Var.named
    var name = Type.var () (v name)
    arr = Type.arrow'
    -- see note on `hashDecls` above for why ctor must be called `Unit.Unit`.
    unit = DataDeclaration Structural () [] [((), v "Unit.Unit", var "Unit")]
    tuple =
      DataDeclaration
        Structural
        ()
        [v "a", v "b"]
        [ ( (),
            v "Tuple.Cons",
            Type.foralls
              ()
              [v "a", v "b"]
              ( var "a"
                  `arr` (var "b" `arr` Type.apps' (var "Tuple") [var "a", var "b"])
              )
          )
        ]
    opt =
      DataDeclaration
        Structural
        ()
        [v "a"]
        [ ( (),
            v "Optional.None",
            Type.foralls () [v "a"] (Type.app' (var "Optional") (var "a"))
          ),
          ( (),
            v "Optional.Some",
            Type.foralls
              ()
              [v "a"]
              (var "a" `arr` Type.app' (var "Optional") (var "a"))
          )
        ]
    eith =
      DataDeclaration
        Structural
        ()
        [v "a", v "b"]
        [ ( (),
            v "Either.Left",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "a" `arr` Type.apps' (var "Either") [var "a", var "b"])
          ),
          ( (),
            v "Either.Right",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "b" `arr` Type.apps' (var "Either") [var "a", var "b"])
          )
        ]
    rewriteCase =
      DataDeclaration
        (Unique "a116f0f1a8d16aba115b7790b09c56820be48798d9fef64fda3ec2325388f769")
        ()
        [v "a", v "b"]
        [ ( (),
            v "RewriteCase.RewriteCase",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "a" `arr` (var "b" `arr` Type.apps' (var "RewriteCase") [var "a", var "b"]))
          )
        ]
    rewriteTerm =
      DataDeclaration
        (Unique "d577219dc862f148bbdbeb78ae977f6a7da22eb44a1b43d484cabd3e4d7e76a1")
        ()
        [v "a", v "b"]
        [ ( (),
            v "RewriteTerm.RewriteTerm",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "a" `arr` (var "b" `arr` Type.apps' (var "RewriteTerm") [var "a", var "b"]))
          )
        ]
    rewriteType =
      DataDeclaration
        (Unique "f9ae4c4263c2f173deeb550dc1f798147c301ea3a6b306810988e4634834507b")
        ()
        [v "a", v "b"]
        [ ( (),
            v "RewriteSignature.RewriteSignature",
            Type.foralls
              ()
              [v "a", v "b"]
              ((var "a" `arr` (var "b" `arr` var "Unit")) `arr` Type.apps' (var "RewriteSignature") [var "a", var "b"])
          )
        ]
    rewrites =
      DataDeclaration
        (Unique "f64795bf31f7eb41e59b31379d6576a4abaca5b4c1bfc0b8c211e608906aff1a")
        ()
        [v "a"]
        [ ( (),
            v "Rewrites.Rewrites",
            Type.foralls
              ()
              [v "a"]
              (var "a" `arr` Type.apps' (var "Rewrites") [var "a"])
          )
        ]
    isTest =
      DataDeclaration
        (Unique "e6dca08b40458b03ca1660cfbdaecaa7279b42d18257898b5fd1c34596aac36f")
        ()
        []
        [((), v "IsTest.IsTest", var "IsTest")]
    isPropagated =
      DataDeclaration
        (Unique "b28d929d0a73d2c18eac86341a3bb9399f8550c11b5f35eabb2751e6803ccc20")
        ()
        []
        [((), v "IsPropagated.IsPropagated", var "IsPropagated")]
    fmode =
      DataDeclaration
        (Unique "3c11ba4f0a5d8fedd427b476cdd2d7673197d11e")
        ()
        []
        [ ((), v "io2.FileMode.Read", var "io2.FileMode"),
          ((), v "io2.FileMode.Write", var "io2.FileMode"),
          ((), v "io2.FileMode.Append", var "io2.FileMode"),
          ((), v "io2.FileMode.ReadWrite", var "io2.FileMode")
        ]
    bmode =
      DataDeclaration
        (Unique "7dd9560d3826c21e5e6a7e08f575b61adcddf849")
        ()
        []
        [ ((), v "io2.BufferMode.NoBuffering", var "io2.BufferMode"),
          ((), v "io2.BufferMode.LineBuffering", var "io2.BufferMode"),
          ((), v "io2.BufferMode.BlockBuffering", var "io2.BufferMode"),
          ( (),
            v "io2.BufferMode.SizedBlockBuffering",
            Type.nat () `arr` var "io2.BufferMode"
          )
        ]
    smode =
      DataDeclaration
        (Unique "453a764f73cb4c7371d9af23b2d5ed646bf9e57c")
        ()
        []
        [ ((), v "io2.SeekMode.AbsoluteSeek", var "io2.SeekMode"),
          ((), v "io2.SeekMode.RelativeSeek", var "io2.SeekMode"),
          ((), v "io2.SeekMode.SeekFromEnd", var "io2.SeekMode")
        ]
    ioerr =
      DataDeclaration
        (Unique "5915e25ac83205f7885395cc6c6c988bc5ec69a1")
        ()
        []
        [ ((), v "io2.IOError.AlreadyExists", var "io2.IOError"),
          ((), v "io2.IOError.NoSuchThing", var "io2.IOError"),
          ((), v "io2.IOError.ResourceBusy", var "io2.IOError"),
          ((), v "io2.IOError.ResourceExhausted", var "io2.IOError"),
          ((), v "io2.IOError.EOF", var "io2.IOError"),
          ((), v "io2.IOError.IllegalOperation", var "io2.IOError"),
          ((), v "io2.IOError.PermissionDenied", var "io2.IOError"),
          ((), v "io2.IOError.UserError", var "io2.IOError")
        ]
    failure =
      DataDeclaration
        (Unique "52ad89274a358b9c802792aa05915e25ac83205f7885395cc6c6c988bc5ec69a1")
        ()
        []
        [ ((), v "io2.Failure.Failure", Type.typeLink () `arr` (Type.text () `arr` (Type.any () `arr` var "io2.Failure")))
        ]

    tlsFailure =
      DataDeclaration
        (Unique "df5ba835130b227ab83d02d1feff5402455a732d613b51dee32230d2f2d067c6")
        ()
        []
        []

    ioFailure =
      DataDeclaration
        (Unique "009cb00e78cac9e47485cc3633c7a363939f63866ea07ab330346a2121d69a83")
        ()
        []
        []

    arrayFailure =
      DataDeclaration
        (Unique "8e877b3a45a3029904dbca9cbd8dda0ec0d147d67bd5b89027a90632c9e927fb")
        ()
        []
        []

    runtimeFailure =
      DataDeclaration
        (Unique "1061ebd9e1b8f99fafecdf02966898fd19151cc14759a7f192e12c5071fb8986")
        ()
        []
        []

    arithmeticFailure =
      DataDeclaration
        (Unique "57eebfd5958d4b07c460293760f241d3e14285740bca78e2149d166e951efc07")
        ()
        []
        []

    miscFailure =
      DataDeclaration
        (Unique "4bd8b3a187c5426d17c30f19efd2851fe6dcfceb302b1dec5df6dfb4214841d9")
        ()
        []
        []

    stmFailure =
      DataDeclaration
        (Unique "0dd9991d6c88424007bfa0b6e55a5211d1a9b6f473ed542dad3b6ecaf94c6941")
        ()
        []
        []

    threadKilledFailure =
      DataDeclaration
        (Unique "e7e479ebb757edcd5acff958b00aa228ac75b0c53638d44cf9d62fca045c33cf")
        ()
        []
        []

    stdhnd =
      DataDeclaration
        (Unique "67bf7a8e517cbb1e9f42bc078e35498212d3be3c")
        ()
        []
        [ ((), v "io2.StdHandle.StdIn", var "io2.StdHandle"),
          ((), v "io2.StdHandle.StdOut", var "io2.StdHandle"),
          ((), v "io2.StdHandle.StdErr", var "io2.StdHandle")
        ]
    seqview =
      DataDeclaration
        Structural
        ()
        [v "a", v "b"]
        [ ( (),
            v "SeqView.VEmpty",
            Type.foralls
              ()
              [v "a", v "b"]
              (Type.apps' (var "SeqView") [var "a", var "b"])
          ),
          ( (),
            v "SeqView.VElem",
            let sv = Type.apps' (var "SeqView") [var "a", var "b"]
             in Type.foralls
                  ()
                  [v "a", v "b"]
                  (var "a" `arr` (var "b" `arr` sv))
          )
        ]
    tr =
      DataDeclaration
        (Unique "70621e539cd802b2ad53105697800930411a3ebc")
        ()
        []
        [ ((), v "Test.Result.Fail", Type.text () `arr` var "Test.Result"),
          ((), v "Test.Result.Ok", Type.text () `arr` var "Test.Result")
        ]
    doc =
      DataDeclaration
        (Unique "c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004")
        ()
        []
        [ ((), v "Doc.Blob", Type.text () `arr` var "Doc"),
          ((), v "Doc.Link", Type.refId () linkRef `arr` var "Doc"),
          ((), v "Doc.Signature", Type.termLink () `arr` var "Doc"),
          ((), v "Doc.Source", Type.refId () linkRef `arr` var "Doc"),
          ((), v "Doc.Evaluate", Type.termLink () `arr` var "Doc"),
          ((), v "Doc.Join", Type.app () (Type.list ()) (var "Doc") `arr` var "Doc")
        ]
    link =
      DataDeclaration
        (Unique "a5803524366ead2d7f3780871d48771e8142a3b48802f34a96120e230939c46bd5e182fcbe1fa64e9bff9bf741f3c04")
        ()
        []
        [ ((), v "Link.Term", Type.termLink () `arr` var "Link"),
          ((), v "Link.Type", Type.typeLink () `arr` var "Link")
        ]

builtinEffectDecls :: [(Symbol, Reference.Id, DD.EffectDeclaration Symbol ())]
builtinEffectDecls =
  case hashDataDecls $ Map.fromList [(v "Exception", exception)] of
    Right a -> over _3 DD.EffectDeclaration <$> a
    Left e -> error $ "builtinEffectDecls: " <> show e
  where
    v = Var.named
    var name = Type.var () (v name)
    arr = Type.arrow'
    self t = Type.cleanupAbilityLists $ Type.effect () [var "Exception"] t
    exception =
      DataDeclaration
        Structural
        ()
        []
        [ ((), v "Exception.raise", Type.forall () (v "x") (failureType () `arr` self (var "x")))
        ]

pattern UnitRefId :: TypeReferenceId
pattern UnitRefId <- (unUnitRef -> True)

pattern PairRefId :: TypeReferenceId
pattern PairRefId <- (unPairRef -> True)

pattern EitherRefId :: TypeReferenceId
pattern EitherRefId <- ((==) eitherRefId -> True)

pattern OptionalRef :: TypeReferenceId
pattern OptionalRef <- (unOptionalRef -> True)

pattern OptionalNone' :: ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern OptionalNone' <- Term.Constructor' (ConstructorReference OptionalRef ((==) noneId -> True))

pattern OptionalSome' ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern OptionalSome' d <- Term.App' (Term.Constructor' (ConstructorReference OptionalRef ((==) someId -> True))) d

pattern TupleType' :: (Var v) => [Type v a] -> Type v a
pattern TupleType' ts <- (unTupleType -> Just ts)

pattern TupleTerm' :: [Term2 vt at ap v a] -> Term2 vt at ap v a
pattern TupleTerm' xs <- (unTupleTerm -> Just xs)

pattern TuplePattern :: [Pattern.Pattern loc] -> Pattern.Pattern loc
pattern TuplePattern ps <- (unTuplePattern -> Just ps)

pattern EitherLeft' :: Term2 vt at ap v a -> Term2 vt at ap v a
pattern EitherLeft' tm <- (unLeftTerm -> Just tm)

pattern EitherRight' :: Term2 vt at ap v a -> Term2 vt at ap v a
pattern EitherRight' tm <- (unRightTerm -> Just tm)

pattern EitherLeftId :: ConstructorId
pattern EitherLeftId <- ((==) eitherLeftId -> True)

pattern EitherRightId :: ConstructorId
pattern EitherRightId <- ((==) eitherRightId -> True)

unLeftTerm,
  unRightTerm ::
    Term.Term2 vt at ap v a ->
    Maybe (Term.Term2 vt at ap v a)
unRightTerm t = case t of
  Term.App' (Term.Constructor' (ConstructorReference EitherRefId EitherRightId)) tm ->
    Just tm
  _ -> Nothing
unLeftTerm t = case t of
  Term.App' (Term.Constructor' (ConstructorReference EitherRefId EitherLeftId)) tm ->
    Just tm
  _ -> Nothing

-- some pattern synonyms to make pattern matching on some of these constants more pleasant
pattern DocRefId :: TypeReferenceId
pattern DocRefId <- ((== docRefId) -> True)

pattern DocJoin ::
  Seq (ABT.Term (Term.F typeVar typeAnn patternAnn) v a) ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocJoin segs <- Term.App' (Term.Constructor' (ConstructorReference DocRefId DocJoinId)) (Term.List' segs)

pattern DocBlob :: Text -> ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocBlob txt <- Term.App' (Term.Constructor' (ConstructorReference DocRefId DocBlobId)) (Term.Text' txt)

pattern DocLink ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocLink link <- Term.App' (Term.Constructor' (ConstructorReference DocRefId DocLinkId)) link

pattern DocSource ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocSource link <- Term.App' (Term.Constructor' (ConstructorReference DocRefId DocSourceId)) link

pattern DocSignature ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocSignature link <- Term.App' (Term.Constructor' (ConstructorReference DocRefId DocSignatureId)) link

pattern DocEvaluate ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocEvaluate link <- Term.App' (Term.Constructor' (ConstructorReference DocRefId DocEvaluateId)) link

pattern Doc :: ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern Doc <- Term.App' (Term.Constructor' (ConstructorReference DocRefId _)) _

pattern DocSignatureId :: ConstructorId
pattern DocSignatureId <- ((== docSignatureId) -> True)

pattern DocBlobId :: ConstructorId
pattern DocBlobId <- ((== docBlobId) -> True)

pattern DocLinkId :: ConstructorId
pattern DocLinkId <- ((== docLinkId) -> True)

pattern DocSourceId :: ConstructorId
pattern DocSourceId <- ((== docSourceId) -> True)

pattern DocEvaluateId :: ConstructorId
pattern DocEvaluateId <- ((== docEvaluateId) -> True)

pattern DocJoinId :: ConstructorId
pattern DocJoinId <- ((== docJoinId) -> True)

pattern LinkTermId :: ConstructorId
pattern LinkTermId <- ((== linkTermId) -> True)

pattern LinkTypeId :: ConstructorId
pattern LinkTypeId <- ((== linkTypeId) -> True)

pattern LinkRefId :: TypeReferenceId
pattern LinkRefId <- ((== linkRefId) -> True)

pattern LinkTerm :: ABT.Term (Term.F typeVar typeAnn patternAnn) v a -> ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern LinkTerm tm <- Term.App' (Term.Constructor' (ConstructorReference LinkRefId LinkTermId)) tm

pattern LinkType ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern LinkType ty <- Term.App' (Term.Constructor' (ConstructorReference LinkRefId LinkTypeId)) ty

unitType,
  pairType,
  optionalType,
  testResultType,
  eitherType,
  ioErrorType,
  fileModeType,
  filePathType,
  bufferModeType,
  seekModeType,
  stdHandleType,
  failureType,
  thunkArgType,
  exceptionType ::
    (Ord v) => a -> Type v a
unitType a = Type.refId a unitRefId
-- used for the type of the argument to force a thunk
thunkArgType = unitType
pairType a = Type.refId a pairRefId
testResultType a = Type.app a (Type.list a) (Type.refId a testResultRefId)
optionalType a = Type.refId a optionalRefId
eitherType a = Type.refId a eitherRefId
ioErrorType a = Type.refId a ioErrorRefId
fileModeType a = Type.refId a fileModeRefId
filePathType a = Type.refId a filePathRefId
bufferModeType a = Type.refId a bufferModeRefId
seekModeType a = Type.refId a seekModeRefId
stdHandleType a = Type.refId a stdHandleRefId
failureType a = Type.refId a failureRefId
exceptionType a = Type.refId a exceptionRefId

tlsSignedCertType :: (Var v) => a -> Type v a
tlsSignedCertType a = Type.refId a tlsSignedCertRefId

unitTerm :: (Var v) => a -> Term2 vt at ap v a
unitTerm ann = Term.constructor ann (ConstructorReference unitRefId 0)

tupleConsTerm ::
  (Ord v, Semigroup a) =>
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
tupleConsTerm hd tl =
  Term.apps' (Term.constructor (ABT.annotation hd) (ConstructorReference pairRefId 0)) [hd, tl]

tupleTerm :: (Var v, Monoid a) => [Term2 vt at ap v a] -> Term2 vt at ap v a
tupleTerm = foldr tupleConsTerm (unitTerm mempty)

-- delayed terms are just lambdas that take a single `()` arg
-- `force` calls the function
forceTerm :: (Var v) => a -> a -> Term v a -> Term v a
forceTerm a au e = Term.app a e (unitTerm au)

delayTerm :: (Var v) => a -> Term v a -> Term v a
delayTerm a = Term.lam a $ Var.typed Var.Delay

unTupleTerm ::
  Term.Term2 vt at ap v a ->
  Maybe [Term.Term2 vt at ap v a]
unTupleTerm t = case t of
  Term.Apps' (Term.Constructor' (ConstructorReference PairRefId 0)) [fst, snd] ->
    (fst :) <$> unTupleTerm snd
  Term.Constructor' (ConstructorReference UnitRefId 0) -> Just []
  _ -> Nothing

unTupleType :: (Var v) => Type v a -> Maybe [Type v a]
unTupleType t = case t of
  Type.Apps' (Type.RefId' PairRefId) [fst, snd] -> (fst :) <$> unTupleType snd
  Type.RefId' UnitRefId -> Just []
  _ -> Nothing

unTuplePattern :: Pattern.Pattern loc -> Maybe [Pattern.Pattern loc]
unTuplePattern p = case p of
  Pattern.Constructor _ (ConstructorReference PairRefId 0) [fst, snd] -> (fst :) <$> unTuplePattern snd
  Pattern.Constructor _ (ConstructorReference UnitRefId 0) [] -> Just []
  _ -> Nothing

unUnitRef, unPairRef, unOptionalRef :: TypeReferenceId -> Bool
unUnitRef = (== unitRefId)
unPairRef = (== pairRefId)
unOptionalRef = (== optionalRefId)
