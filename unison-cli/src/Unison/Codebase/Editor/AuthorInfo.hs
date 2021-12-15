{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.AuthorInfo where

import Crypto.Random (getRandomBytes)
import Data.ByteString (unpack)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Unison.Hashing.V2.Convert as H
import Unison.Prelude (MonadIO, Word8)
import Unison.ConstructorReference (GConstructorReference(..))
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
            hashAndWrangle "guid" guidType $
              Term.app
                a
                (Term.constructor a (ConstructorReference guidTypeRef 0))
                ( Term.app
                    a
                    (Term.builtin a "Bytes.fromList")
                    (Term.list a (map (Term.nat a . fromIntegral) bytes))
                )

          [(authorRef, authorTerm)] =
            hashAndWrangle "author" authorType $
              Term.apps
                (Term.constructor a (ConstructorReference authorTypeRef 0))
                [ (a, Term.ref a (Reference.DerivedId guidRef)),
                  (a, Term.text a t)
                ]

          [(chRef, chTerm)] =
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
    hashAndWrangle :: Text
                  -> Type v a -> Term v a
                  -> [(Reference.Id, Term v a)]
    hashAndWrangle v typ tm =
      Foldable.toList $ fmap (\(id,tm,_tp) -> (id,tm)) $
        H.hashTermComponents
          (Map.singleton (Var.named v) (tm, typ))
    (chType, chTypeRef) = (Type.ref a chTypeRef, unsafeParse copyrightHolderHash)
    (authorType, authorTypeRef) = (Type.ref a authorTypeRef, unsafeParse authorHash)
    (guidType, guidTypeRef) = (Type.ref a guidTypeRef, unsafeParse guidHash)
    unsafeParse = either error id . Reference.fromText
    guidHash = "#fg4sicimattgf7p2u0i752v6rvf4b6kmrtf0ovja81uptfqmcpe9vdo4vhb2ts2o8somo8t1cvv52lhlv5isr0ovf7h93h3sslnkrlo"
    copyrightHolderHash = "#fodu3l22att8et4eiqhtog2p45bh2njh8qfm7mbe1rt4eg2scbgebedagqlo3g1l7ofm3na45julc6174v9cuu62qvo4toaf776kfj8"
    authorHash = "#9akbttgrmjs8tmmfe3m1e9vg2nc0ufok4vjqcs0lm16ut8j3bu0osejahgb87i05k92md983o8rsfb44715indh4oruqeq105hec8l8"
