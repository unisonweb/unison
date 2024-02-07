-- | Small combinators that pretty-print small types in a canonical way for human consumption, such as hashes, file
-- paths, and project names.
module Unison.Cli.Pretty
  ( displayBranchHash,
    prettyAbsolute,
    prettyAbsoluteStripProject,
    prettyBase32HexWithHash,
    prettyBase32Hex,
    prettyBranchId,
    prettyCausalHash,
    prettyDeclPair,
    prettyDeclTriple,
    prettyFilePath,
    prettyHash,
    prettyHash32,
    prettyHumanReadableTime,
    prettyLabeledDependencies,
    prettyPath',
    prettyProjectAndBranchName,
    prettyBranchName,
    prettyProjectBranchName,
    prettyProjectName,
    prettyProjectNameSlash,
    prettyNamespaceKey,
    prettyReadGitRepo,
    prettyReadRemoteNamespace,
    prettyReadRemoteNamespaceWith,
    prettyRelative,
    prettyRemoteBranchInfo,
    prettyRepoInfo,
    prettySCH,
    prettySemver,
    prettyShareLink,
    prettySharePath,
    prettySlashProjectBranchName,
    prettyTermName,
    prettyTypeName,
    prettyTypeResultHeader',
    prettyTypeResultHeaderFull',
    prettyURI,
    prettyUnisonFile,
    prettyWhichBranchEmpty,
    prettyWriteGitRepo,
    prettyWriteRemoteNamespace,
    shareOrigin,
    unsafePrettyTermResultSigFull',
    DisplayObjectPrinter.prettyTermDisplayObjects,
    DisplayObjectPrinter.prettyTypeDisplayObjects,
  )
where

import Control.Lens hiding (at)
import Control.Monad.Writer (Writer, mapWriter, runWriter)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format.Human (HumanTimeLocale (..), defaultHumanTimeLocale, humanReadableTimeI18N')
import Network.URI (URI)
import Network.URI qualified as URI
import Network.URI.Encode qualified as URI
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Util.Base32Hex (Base32Hex)
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Cli.ProjectUtils (projectBranchPathPrism)
import Unison.Cli.Share.Projects.Types qualified as Share
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRepo,
    ReadRemoteNamespace,
    ShareUserHandle (..),
    WriteGitRepo,
    WriteRemoteNamespace (..),
    WriteShareRemoteNamespace (..),
    shareUserHandleToText,
  )
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Core.Project (ProjectBranchName)
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.LabeledDependency as LD
import Unison.Name (Name)
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectName, Semver (..))
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Server.SearchResult' qualified as SR'
import Unison.Sync.Types qualified as Share
import Unison.Syntax.DeclPrinter (AccessorName)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.DisplayObject (DisplayObject (BuiltinObject, MissingObject, UserObject))
import Unison.Syntax.DisplayObjectPrinter qualified as DisplayObjectPrinter
import Unison.Syntax.HashQualified qualified as HQ (unsafeFromVar)
import Unison.Syntax.NamePrinter (prettyHashQualified, styleHashQualified')
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Pretty qualified as P
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as WK

type Pretty = P.Pretty P.ColorText

prettyURI :: URI -> Pretty
prettyURI = P.bold . P.blue . P.shown

prettyReadRemoteNamespace :: ReadRemoteNamespace Share.RemoteProjectBranch -> Pretty
prettyReadRemoteNamespace =
  prettyReadRemoteNamespaceWith \remoteProjectBranch ->
    into @Text (ProjectAndBranch (remoteProjectBranch ^. #projectName) (remoteProjectBranch ^. #branchName))

prettyReadRemoteNamespaceWith :: (a -> Text) -> ReadRemoteNamespace a -> Pretty
prettyReadRemoteNamespaceWith printProject =
  P.group . P.blue . P.text . RemoteRepo.printReadRemoteNamespace printProject

prettyWriteRemoteNamespace :: WriteRemoteNamespace (ProjectAndBranch ProjectName ProjectBranchName) -> Pretty
prettyWriteRemoteNamespace =
  P.group . P.blue . P.text . RemoteRepo.printWriteRemoteNamespace

shareOrigin :: Text
shareOrigin = "https://share.unison-lang.org"

prettyRepoInfo :: Share.RepoInfo -> Pretty
prettyRepoInfo (Share.RepoInfo repoInfo) =
  P.blue (P.text repoInfo)

prettyShareLink :: WriteShareRemoteNamespace -> Pretty
prettyShareLink WriteShareRemoteNamespace {repo, path} =
  let encodedPath =
        Path.toList path
          & fmap (URI.encodeText . NameSegment.toText)
          & Text.intercalate "/"
   in P.green . P.text $ shareOrigin <> "/@" <> shareUserHandleToText repo <> "/p/code/latest/namespaces/" <> encodedPath

prettySharePath :: Share.Path -> Pretty
prettySharePath =
  prettyRelative
    . Path.Relative
    . Path.fromList
    . coerce @[Text] @[NameSegment]
    . toList
    . Share.pathSegments

prettyFilePath :: FilePath -> Pretty
prettyFilePath fp =
  P.blue (P.string fp)

prettyPath' :: Path.Path' -> Pretty
prettyPath' p' =
  if Path.isCurrentPath p'
    then "the current namespace"
    else P.blue (P.shown p')

prettyNamespaceKey :: Either Path' (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) -> Pretty
prettyNamespaceKey = \case
  Left path -> prettyPath' path
  Right (ProjectAndBranch project branch) ->
    prettyProjectAndBranchName (ProjectAndBranch (project ^. #name) (branch ^. #name))

prettyBranchId :: Input.AbsBranchId -> Pretty
prettyBranchId = \case
  Left sch -> prettySCH sch
  Right absPath -> prettyAbsolute $ absPath

prettyRelative :: Path.Relative -> Pretty
prettyRelative = P.blue . P.shown

prettyAbsolute :: Path.Absolute -> Pretty
prettyAbsolute = P.blue . P.shown

prettySCH :: (IsString s) => ShortCausalHash -> P.Pretty s
prettySCH hash = P.group $ "#" <> P.text (SCH.toText hash)

prettyCausalHash :: (IsString s) => CausalHash -> P.Pretty s
prettyCausalHash hash = P.group $ "#" <> P.text (Hash.toBase32HexText . unCausalHash $ hash)

prettyBase32Hex :: (IsString s) => Base32Hex -> P.Pretty s
prettyBase32Hex = P.text . Base32Hex.toText

prettyBase32HexWithHash :: (IsString s) => Base32Hex -> P.Pretty s
prettyBase32HexWithHash b = P.group $ "#" <> prettyBase32Hex b

prettyHash :: (IsString s) => Hash.Hash -> P.Pretty s
prettyHash = prettyBase32HexWithHash . Hash.toBase32Hex

prettyHash32 :: (IsString s) => Hash32 -> P.Pretty s
prettyHash32 = prettyBase32HexWithHash . Hash32.toBase32Hex

prettyProjectName :: ProjectName -> Pretty
prettyProjectName =
  P.green . P.text . into @Text

-- | 'prettyProjectName' with a trailing slash.
prettyProjectNameSlash :: ProjectName -> Pretty
prettyProjectNameSlash project =
  P.group (prettyProjectName project <> P.hiBlack "/")

prettyProjectBranchName :: ProjectBranchName -> Pretty
prettyProjectBranchName =
  P.blue . P.text . into @Text

prettySemver :: Semver -> Pretty
prettySemver (Semver x y z) =
  P.group (P.num x <> "." <> P.num y <> "." <> P.num z)

-- | Like 'prettyProjectBranchName', but with a leading forward slash. This is used in some outputs to
-- encourage/advertise an unambiguous syntax for project branches, as there's an ambiguity with single-segment relative
-- paths.
--
-- Not all project branches are printed such: for example, when listing all branches of a project, we probably don't
-- need or want to prefix every one with a forward slash.
prettySlashProjectBranchName :: ProjectBranchName -> Pretty
prettySlashProjectBranchName branch =
  P.group (P.hiBlack "/" <> prettyProjectBranchName branch)

prettyProjectAndBranchName :: ProjectAndBranch ProjectName ProjectBranchName -> Pretty
prettyProjectAndBranchName (ProjectAndBranch project branch) =
  P.group (prettyProjectName project <> P.hiBlack "/" <> prettyProjectBranchName branch)

prettyBranchName :: ProjectAndBranch ProjectName ProjectBranchName -> Pretty
prettyBranchName (ProjectAndBranch _ branch) = prettySlashProjectBranchName branch

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms#0
-- Optional.None, Maybe.Nothing : Maybe a
unsafePrettyTermResultSigFull' ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  SR'.TermResult' v a ->
  Pretty
unsafePrettyTermResultSigFull' ppe = \case
  SR'.TermResult' hq (Just typ) r aliases ->
    P.lines
      [ P.hiBlack "-- " <> greyHash (HQ.fromReferent r),
        P.group $
          P.commas (fmap greyHash $ hq : map HQ'.toHQ (toList aliases))
            <> " : "
            <> P.syntaxToColor (TypePrinter.prettySyntax ppe typ),
        mempty
      ]
  _ -> error "Don't pass Nothing"
  where
    greyHash = styleHashQualified' id P.hiBlack

prettyTypeResultHeader' :: (Var v) => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeader' (SR'.TypeResult' name dt r _aliases) =
  prettyDeclTriple (name, r, dt)

-- produces:
-- -- #5v5UtREE1fTiyTsTK2zJ1YNqfiF25SkfUnnji86Lms
-- type Optional
-- type Maybe
prettyTypeResultHeaderFull' :: (Var v) => SR'.TypeResult' v a -> Pretty
prettyTypeResultHeaderFull' (SR'.TypeResult' name dt r aliases) =
  P.lines stuff <> P.newline
  where
    stuff =
      (P.hiBlack "-- " <> greyHash (HQ.fromReference r))
        : fmap
          (\name -> prettyDeclTriple (name, r, dt))
          (name : map HQ'.toHQ (toList aliases))
      where
        greyHash = styleHashQualified' id P.hiBlack

prettyDeclTriple ::
  (Var v) =>
  (HQ.HashQualified Name, Reference.Reference, DisplayObject () (DD.Decl v a)) ->
  Pretty
prettyDeclTriple (name, _, displayDecl) = case displayDecl of
  BuiltinObject _ -> P.hiBlack "builtin " <> P.hiBlue "type " <> P.blue (P.syntaxToColor $ prettyHashQualified name)
  MissingObject _ -> mempty -- these need to be handled elsewhere
  UserObject decl -> P.syntaxToColor $ DeclPrinter.prettyDeclHeader name decl

prettyDeclPair ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  (Reference, DisplayObject () (DD.Decl v a)) ->
  Pretty
prettyDeclPair ppe (r, dt) = prettyDeclTriple (PPE.typeName ppe r, r, dt)

prettyTermName :: PPE.PrettyPrintEnv -> Referent -> Pretty
prettyTermName ppe r =
  P.syntaxToColor $
    prettyHashQualified (PPE.termName ppe r)

prettyTypeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
prettyTypeName ppe r =
  P.syntaxToColor $
    prettyHashQualified (PPE.typeName ppe r)

prettyReadGitRepo :: ReadGitRepo -> Pretty
prettyReadGitRepo = \case
  RemoteRepo.ReadGitRepo {url} -> P.blue (P.text url)

prettyWriteGitRepo :: WriteGitRepo -> Pretty
prettyWriteGitRepo RemoteRepo.WriteGitRepo {url} = P.blue (P.text url)

-- prettyWriteRepo :: WriteRepo -> Pretty
-- prettyWriteRepo = \case
--   RemoteRepo.WriteRepoGit RemoteRepo.WriteGitRepo {url} -> P.blue (P.text url)
--   RemoteRepo.WriteRepoShare s -> P.blue (P.text (RemoteRepo.printShareRepo s))

-- | Pretty-print a 'WhichBranchEmpty'.
prettyWhichBranchEmpty :: WhichBranchEmpty -> Pretty
prettyWhichBranchEmpty = \case
  WhichBranchEmptyHash hash -> P.shown hash
  WhichBranchEmptyPath path -> prettyPath' path

-- | Displays a full, non-truncated Branch.CausalHash to a string, e.g. #abcdef
displayBranchHash :: CausalHash -> String
displayBranchHash = ("#" <>) . Text.unpack . Hash.toBase32HexText . unCausalHash

prettyHumanReadableTime :: UTCTime -> UTCTime -> Pretty
prettyHumanReadableTime now time =
  P.green . P.string $ humanReadableTimeI18N' terseTimeLocale now time
  where
    terseTimeLocale =
      defaultHumanTimeLocale
        { justNow = "now",
          secondsAgo = \f -> (++ " secs" ++ dir f),
          oneMinuteAgo = \f -> "a min" ++ dir f,
          minutesAgo = \f -> (++ " mins" ++ dir f),
          oneHourAgo = \f -> "an hour" ++ dir f,
          aboutHoursAgo = \f x -> "about " ++ x ++ " hours" ++ dir f,
          at = \_ t -> t,
          daysAgo = \f -> (++ " days" ++ dir f),
          weekAgo = \f -> (++ " week" ++ dir f),
          weeksAgo = \f -> (++ " weeks" ++ dir f),
          onYear = \dt -> dt,
          dayOfWeekFmt = "%A, %-l:%M%p",
          thisYearFmt = "%b %e",
          prevYearFmt = "%b %e, %Y"
        }

    dir True = " from now"
    dir False = " ago"

prettyRemoteBranchInfo :: (URI, ProjectName, ProjectBranchName) -> Pretty
prettyRemoteBranchInfo (host, remoteProject, remoteBranch) =
  -- Special-case Unison Share since we know its project branch URLs
  if URI.uriToString id host "" == "https://api.unison-lang.org"
    then
      P.hiBlack . P.text $
        "https://share.unison-lang.org/"
          <> into @Text remoteProject
          <> "/code/"
          <> into @Text remoteBranch
    else
      prettyProjectAndBranchName (ProjectAndBranch remoteProject remoteBranch)
        <> " on "
        <> P.hiBlack (P.shown host)

stripProjectBranchInfo :: Path.Absolute -> Maybe Path.Path
stripProjectBranchInfo = fmap snd . preview projectBranchPathPrism

prettyAbsoluteStripProject :: Path.Absolute -> Pretty
prettyAbsoluteStripProject path =
  P.blue case stripProjectBranchInfo path of
    Just p -> P.shown p
    Nothing -> P.shown path

prettyLabeledDependencies :: PPE.PrettyPrintEnv -> Set LabeledDependency -> Pretty
prettyLabeledDependencies ppe lds =
  P.syntaxToColor (P.sep ", " (ld <$> toList lds))
  where
    ld = \case
      LD.TermReferent r -> prettyHashQualified (PPE.termNameOrHashOnly ppe r)
      LD.TypeReference r -> "type " <> prettyHashQualified (PPE.typeNameOrHashOnly ppe r)

prettyUnisonFile :: forall v a. (Var v, Ord a) => PPED.PrettyPrintEnvDecl -> UF.UnisonFile v a -> P.Pretty P.ColorText
prettyUnisonFile ppe uf@(UF.UnisonFileId datas effects terms watches) =
  P.sep "\n\n" (map snd . sortOn fst $ prettyEffects <> prettyDatas <> catMaybes prettyTerms <> prettyWatches)
  where
    prettyEffects = map prettyEffectDecl (Map.toList effects)
    (prettyDatas, accessorNames) = runWriter $ traverse prettyDataDecl (Map.toList datas)
    prettyTerms = map (prettyTerm accessorNames) terms
    prettyWatches = Map.toList watches >>= \(wk, tms) -> map (prettyWatch . (wk,)) tms

    prettyEffectDecl :: (v, (Reference.Id, DD.EffectDeclaration v a)) -> (a, P.Pretty P.ColorText)
    prettyEffectDecl (n, (r, et)) =
      (DD.annotation . DD.toDataDecl $ et, st $ DeclPrinter.prettyDecl ppe' (rd r) (hqv n) (Left et))
    prettyDataDecl :: (v, (Reference.Id, DD.DataDeclaration v a)) -> Writer (Set AccessorName) (a, P.Pretty P.ColorText)
    prettyDataDecl (n, (r, dt)) =
      (DD.annotation dt,) . st <$> (mapWriter (second Set.fromList) $ DeclPrinter.prettyDeclW ppe' (rd r) (hqv n) (Right dt))
    prettyTerm :: Set (AccessorName) -> (v, a, Term v a) -> Maybe (a, P.Pretty P.ColorText)
    prettyTerm skip (n, a, tm) =
      if traceMember isMember then Nothing else Just (a, pb hq tm)
      where
        traceMember =
          if Debug.shouldDebug Debug.Update
            then trace (show hq ++ " -> " ++ if isMember then "skip" else "print")
            else id
        isMember = Set.member hq skip
        hq = hqv n
    prettyWatch :: (String, (v, a, Term v a)) -> (a, P.Pretty P.ColorText)
    prettyWatch (wk, (n, a, tm)) = (a, go wk n tm)
      where
        go wk v tm = case wk of
          WK.RegularWatch
            | Var.UnnamedWatch _ _ <- Var.typeOf v ->
                "> " <> P.indentNAfterNewline 2 (TermPrinter.pretty sppe tm)
          WK.RegularWatch -> "> " <> pb (hqv v) tm
          WK.TestWatch -> "test> " <> st (TermPrinter.prettyBindingWithoutTypeSignature sppe (hqv v) tm)
          w -> P.string w <> "> " <> pb (hqv v) tm
    st = P.syntaxToColor
    sppe = PPED.suffixifiedPPE ppe'
    pb v tm = st $ TermPrinter.prettyBinding sppe v tm
    ppe' = PPED.PrettyPrintEnvDecl dppe dppe `PPED.addFallback` ppe
    dppe = PPE.makePPE (PPE.hqNamer 8 (UF.toNames uf)) PPE.dontSuffixify
    rd = Reference.DerivedId
    hqv v = HQ.unsafeFromVar v
