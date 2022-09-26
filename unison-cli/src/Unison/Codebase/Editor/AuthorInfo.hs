{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.AuthorInfo where

import Crypto.Random (getRandomBytes)
import Data.ByteString (unpack)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Unison.ABT as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.Hashing.V2.Convert as H
import Unison.Prelude (MonadIO, Word8)
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.IOSource as IOSource
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Var (Var)
import qualified Unison.Var as Var
import UnliftIO (liftIO)

data AuthorInfo v a = AuthorInfo
  {guid, author, copyrightHolder :: (Reference.Id, Term v a, Type v a)}

createAuthorInfo :: forall m v a. MonadIO m => Var v => a -> Text -> m (AuthorInfo v a)
createAuthorInfo a t = createAuthorInfo' . unpack <$> liftIO (getRandomBytes 32)
  where
    createAuthorInfo' :: [Word8] -> AuthorInfo v a
    createAuthorInfo' bytes =
      let (guidRef, guidTerm) =
            hashAndWrangle "guid" guidType $
              Term.app
                a
                (Term.constructor a (ConstructorReference guidTypeRef 0))
                ( Term.app
                    a
                    (Term.builtin a "Bytes.fromList")
                    (Term.list a (map (Term.nat a . fromIntegral) bytes))
                )

          (authorRef, authorTerm) =
            hashAndWrangle "author" authorType $
              Term.apps
                (Term.constructor a (ConstructorReference authorTypeRef 0))
                [ (a, Term.ref a (Reference.DerivedId guidRef)),
                  (a, Term.text a t)
                ]

          (chRef, chTerm) =
            hashAndWrangle "copyrightHolder" chType $
              Term.apps
                (Term.constructor a (ConstructorReference chTypeRef 0))
                [ (a, Term.ref a (Reference.DerivedId guidRef)),
                  (a, Term.text a t)
                ]
       in AuthorInfo
            (guidRef, guidTerm, guidType)
            (authorRef, authorTerm, authorType)
            (chRef, chTerm, chType)
    hashAndWrangle ::
      Text ->
      Type v a ->
      Term v a ->
      (Reference.Id, Term v a)
    hashAndWrangle v typ tm =
      case Foldable.toList $ H.hashTermComponents (Map.singleton (Var.named v) (ABT.annotation tm {- Is there a better option here? -}, tm, typ)) of
        [(_ann, id, tm, _tp)] -> (id, tm)
        _ -> error "hashAndWrangle: Expected a single definition."
    (chType, chTypeRef) = (Type.ref a chTypeRef, IOSource.copyrightHolderRef)
    (authorType, authorTypeRef) = (Type.ref a authorTypeRef, IOSource.authorRef)
    (guidType, guidTypeRef) = (Type.ref a guidTypeRef, IOSource.guidRef)
