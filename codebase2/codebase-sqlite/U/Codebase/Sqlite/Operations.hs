{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Operations where

import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor ((<&>))
import qualified Data.Vector as Vector
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import U.Codebase.Sqlite.Queries (DB)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Serialization as S
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Sqlite.Term.Format as S
import qualified U.Codebase.Term as C
import qualified U.Codebase.Term as C.Term
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Hash as H
import U.Util.Serialization (getFromBytes)

loadTermComponentByHash :: DB m => Base32Hex -> m (Maybe [C.Term Symbol])
loadTermComponentByHash = error "todo"

m :: (a -> f (Maybe b)) -> a -> MaybeT f b
m = fmap MaybeT

m' :: (Functor f, Show a) => String -> (a -> f (Maybe b)) -> a -> MaybeT f b
m' msg f a = MaybeT do
  f a <&> \case
    Nothing -> error $ "nothing: " ++ msg ++ " " ++ show a
    Just b -> Just b

loadTermByHash :: DB m => C.Reference.Id -> m (Maybe (C.Term Symbol))
loadTermByHash (C.Reference.Id h i) = runMaybeT do
  -- retrieve the blob
  (localIds, term) <-
    m'
      ("getFromBytes $ S.lookupTermElement " ++ show i)
      (fmap pure $ getFromBytes $ S.lookupTermElement i)
      <=< m' "Q.loadObjectById" Q.loadObjectById
      <=< m' "Q.objectIdByAnyHash" Q.objectIdByAnyHash
      $ H.toBase32Hex h

  -- look up the text and hashes that are used by the term
  texts <- traverse (m' "Q.loadTextById" Q.loadTextById) $ LocalIds.textLookup localIds
  hashes <- traverse (m' "Q.loadPrimaryHashByObjectId" Q.loadPrimaryHashByObjectId) $ LocalIds.objectLookup localIds

  -- substitute the text and hashes back into the term
  let substText (S.LocalTextId w) = texts Vector.! fromIntegral w
      substHash (S.LocalDefnId w) = H.fromBase32Hex $ hashes Vector.! fromIntegral w
      substTermRef = bimap substText (fmap substHash)
      substTypeRef = bimap substText substHash
      substTermLink = bimap substTermRef substTypeRef
      substTypeLink = substTypeRef
  pure (C.Term.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id term)


-- loadLocallyIndexedComponentByHash
