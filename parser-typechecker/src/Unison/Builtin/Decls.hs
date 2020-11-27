{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Builtin.Decls where

import Data.List (elemIndex, find)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Unison.ABT as ABT
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration
  ( DataDeclaration (..),
    Modifier (Structural, Unique),
    hashDecls,
  )
import qualified Unison.DataDeclaration as DD
import qualified Unison.Pattern as Pattern
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import Unison.Term (ConstructorId, Term, Term2)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Var (Var)
import qualified Unison.Var as Var

lookupDeclRef :: Text -> Reference
lookupDeclRef str
  | [(_, d, _)] <- filter (\(v, _, _) -> v == Var.named str) decls =
    Reference.DerivedId d
  | otherwise =
    error $ "lookupDeclRef: missing \"" ++ unpack str ++ "\""
  where
    decls = builtinDataDecls @Symbol

unitRef, pairRef, optionalRef, eitherRef :: Reference
unitRef = lookupDeclRef "Unit"
pairRef = lookupDeclRef "Tuple"
optionalRef = lookupDeclRef "Optional"
eitherRef = lookupDeclRef "Either"

testResultRef, linkRef, docRef, ioErrorRef, stdHandleRef, failureRef :: Reference
testResultRef = lookupDeclRef "Test.Result"
linkRef = lookupDeclRef "Link"
docRef = lookupDeclRef "Doc"
ioErrorRef = lookupDeclRef "io2.IOError"
stdHandleRef = lookupDeclRef "io2.StdHandle"
failureRef = lookupDeclRef "io2.Failure"

fileModeRef, filePathRef, bufferModeRef, seekModeRef, seqViewRef :: Reference
fileModeRef = lookupDeclRef "io2.FileMode"
filePathRef = lookupDeclRef "io2.FilePath"
bufferModeRef = lookupDeclRef "io2.BufferMode"
seekModeRef = lookupDeclRef "io2.SeekMode"
seqViewRef = lookupDeclRef "SeqView"

pairCtorRef, unitCtorRef :: Referent
pairCtorRef = Referent.Con pairRef 0 CT.Data
unitCtorRef = Referent.Con unitRef 0 CT.Data

constructorId :: Reference -> Text -> Maybe Int
constructorId ref name = do
  (_, _, dd) <- find (\(_, r, _) -> Reference.DerivedId r == ref) (builtinDataDecls @Symbol)
  elemIndex name $ DD.constructorNames dd

okConstructorId, failConstructorId, docBlobId, docLinkId, docSignatureId, docSourceId, docEvaluateId, docJoinId, linkTermId, linkTypeId :: ConstructorId
Just okConstructorId = constructorId testResultRef "Test.Result.Ok"
Just failConstructorId = constructorId testResultRef "Test.Result.Fail"
Just docBlobId = constructorId docRef "Doc.Blob"
Just docLinkId = constructorId docRef "Doc.Link"
Just docSignatureId = constructorId docRef "Doc.Signature"
Just docSourceId = constructorId docRef "Doc.Source"
Just docEvaluateId = constructorId docRef "Doc.Evaluate"
Just docJoinId = constructorId docRef "Doc.Join"
Just linkTermId = constructorId linkRef "Link.Term"
Just linkTypeId = constructorId linkRef "Link.Type"

okConstructorReferent, failConstructorReferent :: Referent.Referent
okConstructorReferent = Referent.Con testResultRef okConstructorId CT.Data
failConstructorReferent = Referent.Con testResultRef failConstructorId CT.Data

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: Var v => [(v, Reference.Id, DataDeclaration v ())]
builtinDataDecls = rs1 ++ rs
  where
    rs1 = case hashDecls $
      Map.fromList
        [ (v "Link", link)
        ] of
      Right a -> a
      Left e -> error $ "builtinDataDecls: " <> show e
    rs = case hashDecls $
      Map.fromList
        [ (v "Unit", unit),
          (v "Tuple", tuple),
          (v "Optional", opt),
          (v "Either", eith),
          (v "Test.Result", tr),
          (v "Doc", doc),
          (v "io2.FileMode", fmode),
          (v "io2.BufferMode", bmode),
          (v "io2.SeekMode", smode),
          (v "SeqView", seqview),
          (v "io2.IOError", ioerr),
          (v "io2.StdHandle", stdhnd),
          (v "io2.Failure", failure),
          (v "io2.TlsFailure", tlsFailure)
        ] of
      Right a -> a
      Left e -> error $ "builtinDataDecls: " <> show e
    [(_, linkRef, _)] = rs1
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
        [ ((), v "io2.Failure.Failure", (Type.typeLink () `arr` (Type.text () `arr` var "io2.Failure")))
        ]
    tlsFailure =
      DataDeclaration
        (Unique "df5ba835130b227ab83d02d1feff5402455a732d613b51dee32230d2f2d067c6")
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
          ((), v "Doc.Join", Type.app () (Type.vector ()) (var "Doc") `arr` var "Doc")
        ]
    link =
      DataDeclaration
        (Unique "a5803524366ead2d7f3780871d48771e8142a3b48802f34a96120e230939c46bd5e182fcbe1fa64e9bff9bf741f3c04")
        ()
        []
        [ ((), v "Link.Term", Type.termLink () `arr` var "Link"),
          ((), v "Link.Type", Type.typeLink () `arr` var "Link")
        ]

builtinEffectDecls :: [(v, Reference.Id, DD.EffectDeclaration v ())]
builtinEffectDecls = []

pattern UnitRef <- (unUnitRef -> True)

pattern PairRef <- (unPairRef -> True)

pattern OptionalRef <- (unOptionalRef -> True)

pattern TupleType' ts <- (unTupleType -> Just ts)

pattern TupleTerm' xs <- (unTupleTerm -> Just xs)

pattern TuplePattern ps <- (unTuplePattern -> Just ps)

-- some pattern synonyms to make pattern matching on some of these constants more pleasant
pattern DocRef <- ((== docRef) -> True)

pattern DocJoin segs <- Term.App' (Term.Constructor' DocRef DocJoinId) (Term.Sequence' segs)

pattern DocBlob txt <- Term.App' (Term.Constructor' DocRef DocBlobId) (Term.Text' txt)

pattern DocLink link <- Term.App' (Term.Constructor' DocRef DocLinkId) link

pattern DocSource link <- Term.App' (Term.Constructor' DocRef DocSourceId) link

pattern DocSignature link <- Term.App' (Term.Constructor' DocRef DocSignatureId) link

pattern DocEvaluate link <- Term.App' (Term.Constructor' DocRef DocEvaluateId) link

pattern Doc <- Term.App' (Term.Constructor' DocRef _) _

pattern DocSignatureId <- ((== docSignatureId) -> True)

pattern DocBlobId <- ((== docBlobId) -> True)

pattern DocLinkId <- ((== docLinkId) -> True)

pattern DocSourceId <- ((== docSourceId) -> True)

pattern DocEvaluateId <- ((== docEvaluateId) -> True)

pattern DocJoinId <- ((== docJoinId) -> True)

pattern LinkTermId <- ((== linkTermId) -> True)

pattern LinkTypeId <- ((== linkTypeId) -> True)

pattern LinkRef <- ((== linkRef) -> True)

pattern LinkTerm tm <- Term.App' (Term.Constructor' LinkRef LinkTermId) tm

pattern LinkType ty <- Term.App' (Term.Constructor' LinkRef LinkTypeId) ty

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
  failureType ::
    Ord v => a -> Type v a
unitType a = Type.ref a unitRef
pairType a = Type.ref a pairRef
testResultType a = Type.app a (Type.vector a) (Type.ref a testResultRef)
optionalType a = Type.ref a optionalRef
eitherType a = Type.ref a eitherRef
ioErrorType a = Type.ref a ioErrorRef
fileModeType a = Type.ref a fileModeRef
filePathType a = Type.ref a filePathRef
bufferModeType a = Type.ref a bufferModeRef
seekModeType a = Type.ref a seekModeRef
stdHandleType a = Type.ref a stdHandleRef
failureType a = Type.ref a failureRef

unitTerm :: Var v => a -> Term v a
unitTerm ann = Term.constructor ann unitRef 0

tupleConsTerm ::
  (Ord v, Semigroup a) =>
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
tupleConsTerm hd tl =
  Term.apps' (Term.constructor (ABT.annotation hd) pairRef 0) [hd, tl]

tupleTerm :: (Var v, Monoid a) => [Term v a] -> Term v a
tupleTerm = foldr tupleConsTerm (unitTerm mempty)

-- delayed terms are just lambdas that take a single `()` arg
-- `force` calls the function
forceTerm :: Var v => a -> a -> Term v a -> Term v a
forceTerm a au e = Term.app a e (unitTerm au)

delayTerm :: Var v => a -> Term v a -> Term v a
delayTerm a = Term.lam a $ Var.named "()"

unTupleTerm ::
  Term.Term2 vt at ap v a ->
  Maybe [Term.Term2 vt at ap v a]
unTupleTerm t = case t of
  Term.Apps' (Term.Constructor' PairRef 0) [fst, snd] ->
    (fst :) <$> unTupleTerm snd
  Term.Constructor' UnitRef 0 -> Just []
  _ -> Nothing

unTupleType :: Var v => Type v a -> Maybe [Type v a]
unTupleType t = case t of
  Type.Apps' (Type.Ref' PairRef) [fst, snd] -> (fst :) <$> unTupleType snd
  Type.Ref' UnitRef -> Just []
  _ -> Nothing

unTuplePattern :: Pattern.Pattern loc -> Maybe [Pattern.Pattern loc]
unTuplePattern p = case p of
  Pattern.Constructor _ PairRef 0 [fst, snd] -> (fst :) <$> unTuplePattern snd
  Pattern.Constructor _ UnitRef 0 [] -> Just []
  _ -> Nothing

unUnitRef, unPairRef, unOptionalRef :: Reference -> Bool
unUnitRef = (== unitRef)
unPairRef = (== pairRef)
unOptionalRef = (== optionalRef)
