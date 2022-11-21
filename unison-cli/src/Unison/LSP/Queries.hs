-- | Rewrites of some codebase queries, but which check the scratch file for info first.
module Unison.LSP.Queries
  ( getTypeOfReferent,
    getTypeDeclaration,
  )
where

import Control.Monad.Reader
import qualified Data.Set as Set
import Language.LSP.Types
import qualified Unison.Codebase as Codebase
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import Unison.LSP.FileAnalysis (getFileSummary)
import Unison.LSP.Types
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation3 as R3
import qualified Unison.Util.Relation4 as R4

-- | Gets the type of a reference from either the parsed file or the codebase.
getTypeOfReferent :: Uri -> Referent -> MaybeT Lsp (Type Symbol Ann)
getTypeOfReferent fileUri ref = do
  getFromFile <|> getFromCodebase
  where
    getFromFile = do
      FileSummary {termSummary} <- getFileSummary fileUri
      case ref of
        Referent.Ref (Reference.Builtin {}) -> empty
        Referent.Ref (Reference.DerivedId termRefId) ->
          do
            termSummary
            & R4.lookupD2 (Just termRefId)
            & R3.d3s
            & Set.toList
            & fmap (MaybeT . pure)
            & altSum
        Referent.Con (ConstructorReference r0 cid) _type -> do
          case r0 of
            Reference.DerivedId r -> do
              decl <- getTypeDeclaration fileUri r
              MaybeT . pure $ DD.typeOfConstructor (either DD.toDataDecl id decl) cid
            Reference.Builtin _ -> empty
    getFromCodebase = do
      Env {codebase} <- ask
      MaybeT . liftIO $ Codebase.runTransaction codebase $ Codebase.getTypeOfReferent codebase ref

-- | Gets a decl from either the parsed file or the codebase.
getTypeDeclaration :: Uri -> Reference.Id -> MaybeT Lsp (Decl Symbol Ann)
getTypeDeclaration fileUri refId = do
  getFromFile <|> getFromCodebase
  where
    getFromFile :: MaybeT Lsp (Decl Symbol Ann)
    getFromFile = do
      FileSummary {dataDeclSummary, effectDeclSummary} <- getFileSummary fileUri
      let datas = Set.toList . R.ran $ R3.lookupD2 refId dataDeclSummary
      let effects = Set.toList . R.ran $ R3.lookupD2 refId effectDeclSummary
      MaybeT . pure . listToMaybe $ fmap Right datas <> fmap Left effects

    getFromCodebase = do
      Env {codebase} <- ask
      MaybeT . liftIO $ Codebase.runTransaction codebase $ Codebase.getTypeDeclaration codebase refId
