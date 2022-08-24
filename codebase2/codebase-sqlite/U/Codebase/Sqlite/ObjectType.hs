module U.Codebase.Sqlite.ObjectType where

import Unison.Sqlite (FromField (..), SQLData (SQLInteger), ToField (..))

-- | Don't reorder these, they are part of the database,
--  and the ToField and FromField implementation currently
--  depends on the derived Enum implementation.
data ObjectType
  = TermComponent -- 0
  | DeclComponent -- 1
  -- Namespaces are no longer stored in the object table
  | DeprecatedNamespace -- 2
  | Patch -- 3
  deriving (Eq, Ord, Show, Enum)

{-# DEPRECATED DeprecatedNamespace "Namespaces are no longer kept in the object table" #-}

instance ToField ObjectType where
  toField = SQLInteger . fromIntegral . fromEnum

instance FromField ObjectType where
  fromField = fmap toEnum . fromField
