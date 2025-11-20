module Unison.Codebase.Editor.HandleInput.Dependencies
  ( handleDependencies,
  )
where

import Control.Arrow ((***))
import Data.Bifoldable (binull)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQName)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (Reference)
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2)

handleDependencies :: HQ.HashQualified Name -> Cli ()
handleDependencies hq = do
  dependentsRefs <- resolveHQName hq

  when (binull dependentsRefs) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  namespace <- Cli.getCurrentProjectRoot0
  let ppe =
        let names = Branch.toNames namespace
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  dependencies <- do
    Cli.runTransaction $ Codebase.directDependencies dependentsRefs

  let dependencyNames ::
        DefnsF
          []
          (HQ.HashQualified Name, HQ.HashQualified Name)
          (HQ.HashQualified Name, HQ.HashQualified Name)
      dependencyNames =
        bimap
          (f (Referent.fromTermReference >>> PPE.termNames ppe))
          (f (PPE.typeNames ppe))
          dependencies
        where
          f ::
            (Reference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]) ->
            Set Reference ->
            [(HQ.HashQualified Name, HQ.HashQualified Name)]
          f g =
            Set.toList
              -- Pick the best name for a reference (with `listToMaybe`), else use the ref (if nameless)
              >>> map (\x -> maybe (Left x) Right (listToMaybe (g x)))
              >>> partitionEithers
              -- Sort the named references alphabetically, then stick the hash-only ones at the end
              >>> h

          h ::
            ([Reference], [(HQ'.HashQualified Name, HQ'.HashQualified Name)]) ->
            [(HQ.HashQualified Name, HQ.HashQualified Name)]
          h (nameless, named) =
            concat
              [ named
                  & Name.sortByText (fst >>> HQ'.toText)
                  & map (HQ'.toHQ *** HQ'.toHQ),
                nameless
                  & map (\x -> let y = HQ.fromReference x in (y, y))
              ]

  -- Set numbered args
  (dependencyNames.types ++ dependencyNames.terms)
    & map (SA.HashQualified . fst)
    & Cli.setNumberedArgs

  let dependentsNames :: DefnsF2 Set HQ.HashQualified Name Name
      dependentsNames =
        bimap
          (Set.map (PPE.termNameOrHashOnly ppe))
          (Set.map (PPE.typeNameOrHashOnly ppe))
          dependentsRefs

  Cli.respond (ListDependencies dependentsNames dependencyNames)
