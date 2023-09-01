{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.AuthorInfo
  ( AuthorInfo (..),
    createAuthorInfo,
  )
where

import Crypto.Random (getRandomBytes)
import Data.ByteString (unpack)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Text (Text)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Hashing.V2.Convert qualified as H
import Unison.Prelude (MonadIO, Word8)
import Unison.Reference qualified as Reference
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var
import UnliftIO (liftIO)

data AuthorInfo v a = AuthorInfo
  {guid, author, copyrightHolder :: (Reference.Id, Term v a, Type v a)}

createAuthorInfo :: forall m v a. (MonadIO m) => (Var v) => a -> Text -> m (AuthorInfo v a)
createAuthorInfo a t = createAuthorInfo' . unpack <$> liftIO (getRandomBytes 32)
  where
    createAuthorInfo' :: [Word8] -> AuthorInfo v a
    createAuthorInfo' bytes =
      AuthorInfo
        (guidRef, guidTerm, guidType)
        (authorRef, authorTerm, authorType)
        (chRef, chTerm, chType)
      where
        (guidRef, guidTerm) =
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
              [ (a, Term.refId a guidRef),
                (a, Term.text a t)
              ]

        (chRef, chTerm) =
          hashAndWrangle "copyrightHolder" chType $
            Term.apps
              (Term.constructor a (ConstructorReference chTypeRef 0))
              [ (a, Term.refId a guidRef),
                (a, Term.text a t)
              ]
        (chType, chTypeRef) = (Type.refId a chTypeRef, IOSource.copyrightHolderRef)
        (authorType, authorTypeRef) = (Type.refId a authorTypeRef, IOSource.authorRef)
        (guidType, guidTypeRef) = (Type.refId a guidTypeRef, IOSource.guidRef)
        hashAndWrangle ::
          Text ->
          Type v a ->
          Term v a ->
          (Reference.Id, Term v a)
        hashAndWrangle v typ tm =
          case Foldable.toList $ H.hashTermComponents (Map.singleton (Var.named v) (tm, typ, ())) of
            [(id, tm, _tp, ())] -> (id, tm)
            _ -> error "hashAndWrangle: Expected a single definition."
