module U.Codebase.Sqlite.TempEntityType where

import Unison.Sqlite (FromField (..), SQLData (SQLInteger), ToField (..))

-- | Don't reorder these, they are part of the database,
--  and the ToField and FromField implementation currently
--  depends on the derived Enum implementation.
data TempEntityType
  = TermComponentType -- 0
  | DeclComponentType -- 1
  | NamespaceType -- 2
  | PatchType -- 3
  | CausalType -- 4
  deriving (Eq, Ord, Show, Enum)

instance ToField TempEntityType where
  toField = SQLInteger . fromIntegral . fromEnum

instance FromField TempEntityType where
  fromField = fmap toEnum . fromField
