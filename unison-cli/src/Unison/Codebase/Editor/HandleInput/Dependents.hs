module Unison.Codebase.Editor.HandleInput.Dependents
  ( handleDependents,
  )
where

import Data.Bifoldable (bifoldMap, binull)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQName)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Util.Defns (Defns (..), DefnsF)

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  refs <- resolveHQName hq

  when (binull refs) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  namespace <- Cli.getCurrentProjectRoot0
  dependents <- Cli.runTransaction $ Codebase.dependentsWithinBranchScope namespace refs

  let namespaceWithoutLibdeps = Branch.deleteLibdeps namespace
  let ppeWithoutLibdeps =
        let names = Branch.toNames namespaceWithoutLibdeps
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  let dependentNames ::
        DefnsF
          []
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
      dependentNames =
        bimap
          (f (Referent.fromTermReferenceId >>> PPE.termNames ppeWithoutLibdeps))
          (f (Reference.fromId >>> PPE.typeNames ppeWithoutLibdeps))
          dependents
        where
          f g =
            Set.toList
              >>> mapMaybe (g >>> listToMaybe)
              >>> Name.sortByText (fst >>> HQ'.toText)
  pure dependentNames

  -- Set numbered args
  (dependentNames.types ++ dependentNames.terms)
    & map (SA.HashQualified . HQ'.toHQ . fst)
    & Cli.setNumberedArgs

  let lds = bifoldMap (Set.map LD.referent) (Set.map LD.typeRef) refs

  let ppe =
        let names = Branch.toNames namespace
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  Cli.respond (ListDependents ppe lds dependentNames)
