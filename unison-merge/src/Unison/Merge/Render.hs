module Unison.Merge.Render
  ( renderUnisonFiles,
  )
where

import Control.Lens (mapped)
import Data.Align (align)
import Data.Bifoldable (bifoldMap)
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Data.Zip (unzip)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge.ThreeWay (GThreeWay, ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Updated (GUpdated (..), Updated)
import Unison.Name (Name)
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Prelude hiding (unzip)

renderUnisonFiles ::
  TwoWay Text ->
  GThreeWay PartialDeclNameLookup DeclNameLookup ->
  ThreeWay (DefnsF (Map Name) Referent TypeReference) ->
  ThreeWay (DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)) ->
  Updated Names ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay (DefnsF Set Name Name) ->
  (Pretty ColorText, ThreeWay (Pretty ColorText))
renderUnisonFiles authors declNameLookups defnsByName hydratedDefns libdepsNames conflicts dependents =
  let pped :: PrettyPrintEnvDecl
      pped =
        makePrettyPrintEnv
          (Names.fromUnconflicted <$> defnsByName)
          libdepsNames

      renderedConflicts :: TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
      renderedDependents :: TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
      (renderedConflicts, renderedDependents) =
        renderConflictsAndDependents
          (ThreeWay.gforgetLca declNameLookups)
          (ThreeWay.forgetLca hydratedDefns)
          conflicts
          dependents
          pped

      renderedLcaConflicts :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
      renderedLcaConflicts =
        renderLcaConflicts
          declNameLookups.lca
          hydratedDefns.lca
          conflicts
          pped

      unparsedFile :: Pretty ColorText
      unparsedFile =
        makePrettyUnisonFile authors renderedConflicts renderedDependents

      unparsedSoloFiles :: ThreeWay (Pretty ColorText)
      unparsedSoloFiles =
        ThreeWay
          { alice = renderedConflicts.alice,
            bob = renderedConflicts.bob,
            lca = renderedLcaConflicts
          }
          <&> \conflicts -> makePrettySoloUnisonFile conflicts renderedDependents
   in (unparsedFile, unparsedSoloFiles)

-- Create a PPE that uses Alice's names whenever possible, falling back to Bob's names only when Alice doesn't have any,
-- and falling back to the LCA after that.
--
-- This results in a file that "looks familiar" to Alice (the one merging in Bob's changes), and avoids superfluous
-- textual conflicts that would arise from preferring Bob's names for Bob's code (where his names differ).
--
-- The LCA names are not used unless we need to render LCA definitions for a mergetool, but we add them to the PPE in
-- all cases anyway. If this is very expensive, we could consider omitting them in the case that no mergetool is
-- configured.
--
-- Note that LCA names can make name quality slightly worse. For example, "foo.bar" might exist in the LCA, but deleted
-- in Alice and Bob, and nonetheless prevent some "qux.bar" from rendering as "bar". That seems fine.
makePrettyPrintEnv :: ThreeWay Names -> Updated Names -> PrettyPrintEnvDecl
makePrettyPrintEnv defns libdeps =
  PPED.makePPED
    ( PPE.namer
        ( Names.preferring
            -- Here it might be slightly more comfortable to Alice if we prefer her names and _her_ libdeps, not the
            -- combined Alice+Bob libdep, because that might bring in a Bob name that Alice isn't yet familiar with
            -- (even though it will be in her merge result at the end). However, that would require a bit of simple
            -- refactoring (just need to delay the combining of libdeps until at least here), and doesn't seem worth it
            -- over this quick fix of just "prefer Alice + any libdep name over names that only Bob's project has".
            (Names.preferring (defns.alice <> libdeps.new) defns.bob)
            (defns.lca <> libdeps.old)
        )
    )
    (PPE.suffixifyByName (fold defns <> libdeps.new))

renderConflictsAndDependents ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)) ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay (DefnsF Set Name Name) ->
  PrettyPrintEnvDecl ->
  ( TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)),
    TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText))
  )
renderConflictsAndDependents declNameLookups hydratedDefns conflicts dependents ppe =
  unzip $
    ( \declNameLookup (conflicts, dependents) ->
        let render needsGuid = renderDefnsForUnisonFile declNameLookup ppe needsGuid . over (#terms . mapped) snd
         in (render uniqueTypeConflictsWithDifferentGuids conflicts, render Set.empty dependents)
    )
      <$> declNameLookups
      <*> hydratedConflictsAndDependents
  where
    hydratedConflictsAndDependents ::
      TwoWay
        ( DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann),
          DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann)
        )
    hydratedConflictsAndDependents =
      ( \as bs cs ->
          ( zipDefnsWith Map.restrictKeys Map.restrictKeys as bs,
            zipDefnsWith Map.restrictKeys Map.restrictKeys as cs
          )
      )
        <$> hydratedDefns
        <*> conflicts
        <*> dependents

    uniqueTypeConflictsWithDifferentGuids :: Set Name
    uniqueTypeConflictsWithDifferentGuids =
      TwoWay.twoWay
        ( \(aliceConflicts, _) (bobConflicts, _) ->
            getConst
              ( Map.mergeA
                  Map.dropMissing
                  Map.dropMissing
                  ( Map.zipWithAMatched
                      \name (_, decl1) (_, decl2) ->
                        Const
                          case ( DataDeclaration.modifier (DataDeclaration.asDataDecl decl1),
                                 DataDeclaration.modifier (DataDeclaration.asDataDecl decl2)
                               ) of
                            (DataDeclaration.Unique guid1, DataDeclaration.Unique guid2) | guid1 /= guid2 -> Set.singleton name
                            _ -> Set.empty
                  )
                  aliceConflicts.types
                  bobConflicts.types
              )
        )
        hydratedConflictsAndDependents

renderLcaConflicts ::
  PartialDeclNameLookup ->
  DefnsF (Map Name) (TermReferenceId, (Term Symbol Ann, Type Symbol Ann)) (TypeReferenceId, Decl Symbol Ann) ->
  TwoWay (DefnsF Set Name Name) ->
  PrettyPrintEnvDecl ->
  DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
renderLcaConflicts partialDeclNameLookup hydratedDefns conflicts ppe =
  let hydratedConflicts = zipDefnsWith Map.restrictKeys Map.restrictKeys hydratedDefns (fold conflicts)
   in renderDefnsForUnisonFile
        declNameLookup
        ppe
        Set.empty
        (over (#terms . mapped) snd hydratedConflicts)
  where
    -- We allow the LCA of a merge to have missing constructor names, yet we do need to render *something* in a file
    -- for a mergetool (if one is configured). So, we make the partial decl name lookup total by making bogus
    -- constructor names as necessary.
    declNameLookup :: DeclNameLookup
    declNameLookup =
      DeclNameLookup
        { constructorToDecl = partialDeclNameLookup.constructorToDecl,
          declToConstructors =
            makeTotal <$> partialDeclNameLookup.declToConstructors
        }
      where
        makeTotal :: [Maybe Name] -> [Name]
        makeTotal names0 =
          case sequence names0 of
            Just names -> names
            Nothing ->
              snd $
                List.mapAccumL
                  makeSomethingUp
                  (foldMap (maybe Set.empty Set.singleton) names0)
                  names0
          where
            makeSomethingUp :: Set Name -> Maybe Name -> (Set Name, Name)
            makeSomethingUp taken = \case
              Just name -> (taken, name)
              Nothing ->
                let name = freshen 0 "Unnamed"
                    !taken1 = Set.insert name taken
                 in (taken1, name)
              where
                freshen :: Int -> Text -> Name
                freshen i name0
                  | Set.member name taken = freshen (i + 1) name0
                  | otherwise = name
                  where
                    name :: Name
                    name =
                      Name.unsafeParseText (name0 <> if i == 0 then Text.empty else Text.pack (show i))

makePrettyUnisonFile ::
  TwoWay Text ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettyUnisonFile authors conflicts dependents =
  fold
    [ conflicts
        -- Merge the two maps together into one, remembering who authored what
        & TwoWay.twoWay (zipDefnsWith align align)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each conflict, types then terms (even though a type can conflict with a term, in which case they
        -- would not be adjacent in the file), with an author comment above each conflicted thing
        & ( let f =
                  foldMap \case
                    This x -> alice x
                    That y -> bob y
                    These x y -> alice x <> bob y
                  where
                    alice = prettyBinding (Just (Pretty.text authors.alice))
                    bob = prettyBinding (Just (Pretty.text authors.bob))
             in bifoldMap f f
          ),
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      let thereAre defns = TwoWay.or (not . defnsAreEmpty <$> defns)
       in if thereAre conflicts && thereAre dependents
            then
              fold
                [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
                  "-- conflicted definitions above.\n\n"
                ]
            else mempty,
      makePrettyDependents dependents
    ]
  where
    prettyBinding maybeComment binding =
      fold
        [ case maybeComment of
            Nothing -> mempty
            Just comment -> "-- " <> comment <> "\n",
          binding,
          "\n\n"
        ]

makePrettySoloUnisonFile ::
  DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText) ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettySoloUnisonFile conflicts dependents =
  fold
    [ conflicts
        & inAlphabeticalOrder
        & let f = foldMap (<> "\n\n") in bifoldMap f f,
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      if not (defnsAreEmpty conflicts) && TwoWay.or (not . defnsAreEmpty <$> dependents)
        then
          fold
            [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
              "-- conflicted definitions.\n\n"
            ]
        else mempty,
      -- Include all dependents when invoking this function with alice/bob/lca conflicts, because we don't want any diff
      -- here – we want the mergetool to copy over all dependents after resolving the real conflicts above the fold.
      makePrettyDependents dependents
    ]

makePrettyDependents :: TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) -> Pretty ColorText
makePrettyDependents =
  -- Merge dependents together into one map (they are disjoint)
  TwoWay.twoWay (zipDefnsWith Map.union Map.union)
    >>>
    -- Sort alphabetically
    inAlphabeticalOrder
    -- Render each dependent, types then terms, without bothering to comment attribution
    >>> (let f = foldMap (<> "\n\n") in bifoldMap f f)

inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
inAlphabeticalOrder =
  bimap f f
  where
    f = map snd . sortAlphabeticallyOn fst . Map.toList
