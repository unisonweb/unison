{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.AuthorInfo where

import Crypto.Random (getRandomBytes)
import Data.ByteString (unpack)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Unison.Hashing.V2.Convert as H
import Unison.Prelude (MonadIO, Word8)
import qualified Unison.Reference as Reference
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
      let [(guidRef, guidTerm)] =
            hashAndWrangle "guid" $
              Term.app
                a
                (Term.constructor a guidTypeRef 0)
                ( Term.app
                    a
                    (Term.builtin a "Bytes.fromList")
                    (Term.list a (map (Term.nat a . fromIntegral) bytes))
                )

          [(authorRef, authorTerm)] =
            hashAndWrangle "author" $
              Term.apps
                (Term.constructor a authorTypeRef 0)
                [ (a, Term.ref a (Reference.DerivedId guidRef)),
                  (a, Term.text a t)
                ]

          [(chRef, chTerm)] =
            hashAndWrangle "copyrightHolder" $
              Term.apps
                (Term.constructor a chTypeRef 0)
                [ (a, Term.ref a (Reference.DerivedId guidRef)),
                  (a, Term.text a t)
                ]
       in AuthorInfo
            (guidRef, guidTerm, guidType)
            (authorRef, authorTerm, authorType)
            (chRef, chTerm, chType)
    hashAndWrangle v tm =
      Foldable.toList $
        H.hashTermComponents
          (Map.fromList [(Var.named v, tm)])
    (chType, chTypeRef) = (Type.ref a chTypeRef, unsafeParse copyrightHolderHash)
    (authorType, authorTypeRef) = (Type.ref a authorTypeRef, unsafeParse authorHash)
    (guidType, guidTypeRef) = (Type.ref a guidTypeRef, unsafeParse guidHash)
    unsafeParse = either error id . Reference.fromText
    guidHash = "#rc29vdqe019p56kupcgkg07fkib86r3oooatbmsgfbdsgpmjhsh00l307iuts3r973q5etb61vbjkes42b6adb3mkorusvmudiuorno"
    copyrightHolderHash = "#jeaknsbobmr6pdj9bga290pj1qckqsemiu1qkg7l9s6p88ot111218jkoe6l19hjpdqctpd0c87capaf3j5qlcim1uh1pq23pu0ebsg"
    authorHash = "#i8f8ru3p8ijof9r26lskplmjj45rle8jdh31n62cef2r0tbj6fgjkcu2ljh4m44lo16if0fcdp7eb5fqo1iard47l4cllo7g244kmo0"
