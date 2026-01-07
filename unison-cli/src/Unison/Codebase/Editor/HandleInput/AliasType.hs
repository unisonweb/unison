module Unison.Codebase.Editor.HandleInput.AliasType (handleAliasType) where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output
import Unison.Codebase.Path (Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.DataDeclaration.ConstructorId qualified as ConstructorId
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Backend qualified as Backend
import Unison.ShortHash qualified as SH
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set

-- Cases to test:
--
--
-- [x] Aliasing local type with constructors.
-- [x] Aliasing local type without constructors.
-- [x] Aliasing builtin in lib.
-- [ ] Aliasing non-builtin in lib with one name.

handleAliasType :: Bool -> Either SH.ShortHash (HQ'.HashQualified (Path.Split Path')) -> Path.Split Path' -> Cli ()
handleAliasType force src' dest' = do
  env <- ask
  pp <- Cli.getCurrentProjectPath
  projectNamespace <- Cli.getCurrentProjectRoot
  projectNamespace0 <- Cli.getCurrentProjectRoot0

  (srcType, maybeSrcConstructors) <-
    case src' of
      Right name -> do
        -- Resolve the name relative to the project root
        let hqPathToType :: HQ'.HashQualified (Path.Split Path.Path)
            hqPathToType =
              over (mapped . _1) (Path.unabsolute . Path.resolve pp.absPath) name

        let pathToType :: Path.Split Path.Path
            pathToType =
              HQ'.toName hqPathToType

        let actualTypeName :: Name
            actualTypeName =
              Path.nameFromSplit pathToType

        -- Look up all types at the path
        let types :: Set TypeReference
            types =
              BranchUtil.getType hqPathToType projectNamespace0

        -- Fail if there are 0, or 2+
        typ <-
          Set.asSingleton types & onNothing do
            Cli.returnEarly
              if Set.null types
                then TypeNotFound name
                else DeleteNameAmbiguous 10 name Set.empty types

        constructorNames :: Maybe (ConstructorType, [Maybe Name]) <-
          case Reference.toId typ of
            Nothing -> pure Nothing
            Just typId -> do
              -- expectDeclNumConstructors :: TypeReferenceId -> Sqlite.Transaction Int,
              if Name.beginsWithSegment actualTypeName NameSegment.libSegment
                then do
                  (declType, numConstructors) <-
                    Cli.runTransaction do
                      (,)
                        <$> Codebase.getDeclType env.codebase typ
                        <*> Codebase.expectDeclNumConstructors env.codebase typId
                  let constructorReferents :: [Referent]
                      constructorReferents =
                        numConstructors
                          & ConstructorId.fromNumConstructors
                          & map (\cid -> Referent.Con (ConstructorReference typ cid) declType)
                  pure case BranchUtil.getBranch pathToType projectNamespace0 of
                    Just namespaceUnderneathType ->
                      let bestNameForConstructorReferent :: Referent -> Maybe Name
                          bestNameForConstructorReferent =
                            let terms = Branch.deepTerms (Branch.head namespaceUnderneathType)
                             in \constructorReferent ->
                                  terms
                                    & Relation.lookupDom constructorReferent
                                    & Set.toList
                                    & List.sortOn Name.countSegments
                                    & listToMaybe
                       in Just (declType, map bestNameForConstructorReferent constructorReferents)
                    Nothing -> Nothing
                else case Branch.asUnconflicted projectNamespace0 of
                  Right defns -> do
                    (declType, declNameLookup) <-
                      Cli.runTransaction do
                        (,)
                          <$> Codebase.getDeclType env.codebase typ
                          <*> Codebase.getBranchPartialDeclNameLookup
                            env.codebase
                            (Branch.namespaceHash projectNamespace)
                            defns
                    declNameLookup.declToConstructors
                      & Map.lookup actualTypeName
                      & maybe [] (map (>>= Name.stripNamePrefix actualTypeName))
                      & (declType,)
                      & Just
                      & pure
                  Left _ -> pure Nothing

        pure (typ, constructorNames)
      Left hash -> do
        types <- Cli.runTransaction (Backend.typeReferencesByShortHash hash)
        typ <-
          Set.asSingleton types & onNothing do
            Cli.returnEarly do
              if Set.null types
                then (TypeNotFound' hash)
                else HashAmbiguous hash (Set.map Referent.Ref types)
        pure (typ, Nothing)

  let dest :: Path.Split Path.Absolute
      dest =
        over _1 (Path.resolve pp.absPath) dest'

  let destTypes :: Set TypeReference
      destTypes =
        BranchUtil.getType (HQ'.NameOnly (over _1 Path.unabsolute dest)) projectNamespace0

  when (not force && not (Set.null destTypes)) do
    Cli.returnEarly (TypeAlreadyExists dest' destTypes)

  let destConstructors :: [(Path.Split Path.Absolute, Referent)]
      destConstructors =
        case maybeSrcConstructors of
          Just (declType, srcConstructors) ->
            srcConstructors
              & zip [(0 :: ConstructorId) ..]
              & mapMaybe \(cid, maybeConstructorName) -> do
                constructorName <- maybeConstructorName
                Just
                  ( Path.resolve
                      dest
                      (Path.splitFromName constructorName),
                    Referent.Con (ConstructorReference srcType cid) declType
                  )
          Nothing -> []

  -- TODO bail if any constructor name already exists

  Cli.stepManyAt
    pp.branch
    ( ( if force
          then "debug.alias.type.force "
          else "alias.type "
      )
        <> either SH.toText (HQ'.toTextWith (Path.toText . Path.unsplit)) src'
        <> " "
        <> into @Text (Path.unsplit dest)
    )
    ( BranchUtil.makeAddTypeName dest srcType
        : map (\(p, r) -> BranchUtil.makeAddTermName p r) destConstructors
    )

  Cli.respond Success
