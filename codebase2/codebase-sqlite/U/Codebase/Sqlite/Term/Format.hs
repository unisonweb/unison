{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Term.Format where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import U.Codebase.Reference (Reference')
import U.Codebase.Referent (Referent')
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalIds', LocalTextId, WatchLocalIds)
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Term as Term
import qualified U.Core.ABT as ABT
import qualified U.Codebase.Type as Type
import qualified U.Codebase.Sqlite.Reference as Sqlite
import U.Codebase.Sqlite.DbId (ObjectId, TextId)

-- |
-- * Builtin terms are represented as local text ids.
-- * Non-builtin terms are represented as local definition ids, with an added distinguished element (here @Nothing@)
--   which represents a self-reference.
type TermRef = Reference' LocalTextId (Maybe LocalDefnId)

-- |
-- * Builtin types are represented as a local text id.
-- * Non-builtin types are represented by a local definition id.
type TypeRef = Reference' LocalTextId LocalDefnId

type TermLink = Referent' TermRef TypeRef
type TypeLink = TypeRef

-- | A 'LocallyIndexedComponent' is a vector that has one element per member of the component (invariant: 1+).
--
-- Each element is a term, which is represented as:
--
--   * Lookup vectors that map local ids to database ids for texts and objects referenced by the term.
--   * The term itself, with internal references to local ids (offsets into the lookup vectors).
--   * The term's type, also with internal references to local id.
type LocallyIndexedComponent = LocallyIndexedComponent' TextId ObjectId
newtype LocallyIndexedComponent' t d =
  LocallyIndexedComponent (Vector (LocalIds' t d, Term, Type))
  deriving Show

{-
message = "hello, world"     -> ABT { ... { Term.F.Text "hello, world" } }    -> hashes to (#abc, 0)
program = printLine message  -> ABT { ... { Term.F.App (ReferenceBuiltin ##io.PrintLine) (Reference #abc 0) } } -> hashes to (#def, 0)

text table =
  id         text
  -- ------------
   1 hello, world
   2      message
   3      program
   4         Text
   5           IO
   6         Unit
   7 io.PrintLine

hash table =
  id base32
  -- ------
  10    abc
  11    def

hash_object table =
  hash_id object_id hash_version
  ------- --------- ------------
       10        20            2

object table =
  { 20 ->
      LocallyIndexedComponent [
        (localIds = LocalIds {
            text = [1,4]
            defs = []
          },
        term = ABT { ... { Term.F.Text (LocalTextId 0) } },
        type = ABT { ... { Term.FT.Ref (Builtin (LocalTextId 1)) }}
        )
      ],

    21 ->
      LocallyIndexedComponent [
        (localIds = LocalIds {
            text = [7,5,6]
            defs = [20]
          },
        term = ABT { ... { Term.F.App (ReferenceBuiltin (LocalTextId 7) (ReferenceId (LocalDefnId 0) 0) } },
        type = ABT { ... { Term.FT.App (Term.FT.Ref (Builtin (LocalTextId 0))) (Term.FT.Ref (Builtin (LocalTextId 1))) } }
        )
      ],
  }
-}

type F =
  Term.F' LocalTextId TermRef TypeRef TermLink TypeLink Symbol

type FT = Type.F' TypeRef

type Term = ABT.Term F Symbol ()
type Type = ABT.Term FT Symbol ()

-- * Type of Term
-- Maybe these should have a LocalIds index too; or share one with the term?
type FTT = Type.F' Sqlite.Reference
type TypeOfTerm = ABT.Term FTT Symbol ()

data TermFormat
  = Term LocallyIndexedComponent

data WatchResultFormat
  = WatchResult WatchLocalIds Term

data SyncWatchResultFormat
  = SyncWatchResult WatchLocalIds ByteString
