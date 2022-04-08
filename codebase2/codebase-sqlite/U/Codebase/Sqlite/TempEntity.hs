module U.Codebase.Sqlite.TempEntity where

data TempEntity text noSyncHash hash
  = TC (TermFormat' text hash)
  | DC (DeclComponent text hash)
  | P (Patch text noSyncHash hash)
  | N (Namespace text hash)
  | C (Causal hash)
  deriving stock (Show, Eq, Ord)

type TempTermFormat text hash =
  TermFormat

data TempDeclFormat text hash
  = Decl [(LocalIds' text hash, ByteString)]

type TempPatchFormat text noSyncHash hash =
  SyncPatchFormat' hash text noSyncHash hash
