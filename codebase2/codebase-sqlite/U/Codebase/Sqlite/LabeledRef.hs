{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module U.Codebase.Sqlite.LabeledRef where

import U.Codebase.Sqlite.Referent (Referent)
import U.Codebase.Sqlite.Reference (Reference)
import Database.SQLite.Simple (FromRow(..))
import Control.Applicative
import qualified U.Codebase.Referent as Referent
import U.Codebase.Sqlite.ObjectType (ObjectType)
import Database.SQLite.Simple.FromRow (field)
import Unison.Prelude
import qualified U.Codebase.Sqlite.ObjectType as OT

data LabeledRef =
      TypeReference Reference
    | TermReference Reference
    | ConstructorReference Referent
    deriving Show

-- | Get the untagged Type or Term 'Reference' from a LabeledRef
toReference :: LabeledRef -> Reference
toReference = \case
  TypeReference ref -> ref
  TermReference ref -> ref
  ConstructorReference (Referent.Ref ref) -> ref
  ConstructorReference (Referent.Con ref _) -> ref

-- SELECT optional builtin_text_id, object_id, pos, object_type, optional constructor_id
instance FromRow LabeledRef where
  fromRow = do
    ref <- fromRow @Reference
    refType <- field @ObjectType
    case refType of
      -- TermComponent
      OT.TermComponent -> pure $ TermReference ref
      -- DeclComponent
      OT.DeclComponent -> do
        optional field <&> \case
          Nothing -> TypeReference ref
          -- This is dubious, we don't know whether it's a constructor or effect here.
          Just conId -> ConstructorReference $ Referent.Con ref conId
      _ -> empty
