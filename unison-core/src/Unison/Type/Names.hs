module Unison.Type.Names
  ( bindNames,
  )
where

import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names.ResolutionResult qualified as Names
import Unison.Names.ResolvesTo (ResolvesTo (..), partitionResolutions)
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Type
import Unison.Type qualified as Type
import Unison.Util.List qualified as List
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)

bindNames ::
  forall a v.
  (Var v) =>
  (v -> Name) ->
  (Name -> v) ->
  Set v ->
  Names ->
  Type v a ->
  Names.ResolutionResult v a (Type v a)
bindNames unsafeVarToName nameToVar localVars namespaceNames ty =
  let -- Identify the unresolved variables in the type: those whose names aren't an *exact* match for some locally-bound
      -- type.
      --
      -- For example:
      --
      --   type Foo.Bar = ...
      --   type Baz.Qux = ...
      --   type Whatever = Whatever Foo.Bar Qux
      --                            ^^^^^^^ ^^^
      --                               |    this variable *is* unresolved: it doesn't match any locally-bound type exactly
      --                               |
      --                            this variable is *not* unresolved: it matches locally-bound `Foo.Bar` exactly
      unresolvedVars :: [(v, a)]
      unresolvedVars =
        ABT.freeVarOccurrences localVars ty

      -- For each unresolved variable, look up what it might refer to:
      --
      --   1. An exact match in the namespace.
      --   2. A suffix match in the namespace.
      --   3. A suffix match in the local names.
      resolvedVars :: [(v, a, (Set TypeReference, Set TypeReference), Set Name)]
      resolvedVars =
        map
          ( \(v, a) ->
              let name = unsafeVarToName v
               in (v, a, getNamespaceMatches name, getLocalMatches name)
          )
          unresolvedVars

      checkAmbiguity ::
        (v, a, (Set TypeReference, Set TypeReference), Set Name) ->
        Either (Seq (Names.ResolutionFailure v a)) (v, ResolvesTo TypeReference)
      checkAmbiguity (v, a, (exactNamespaceMatches, suffixNamespaceMatches), localMatches) =
        case (Set.size exactNamespaceMatches, Set.size suffixNamespaceMatches, Set.size localMatches) of
          (1, _, _) -> good (ResolvesToNamespace (Set.findMin exactNamespaceMatches))
          (n, _, _) | n > 1 -> bad (Names.Ambiguous namespaceNames exactNamespaceMatches Set.empty)
          (_, 0, 0) -> bad Names.NotFound
          (_, 1, 0) -> good (ResolvesToNamespace (Set.findMin suffixNamespaceMatches))
          (_, 0, 1) -> good (ResolvesToLocal (Set.findMin localMatches))
          _ -> bad (Names.Ambiguous namespaceNames suffixNamespaceMatches localMatches)
        where
          bad = Left . Seq.singleton . Names.TypeResolutionFailure v a
          good = Right . (v,)
   in List.validate checkAmbiguity resolvedVars <&> \resolutions ->
        let (namespaceResolutions, localResolutions) = partitionResolutions resolutions
         in ty
              -- Apply namespace resolutions (replacing "Foo" with #Foo where "Foo" refers to namespace)
              & bindExternal namespaceResolutions
              -- Apply local resolutions (replacing "Foo" with "Full.Name.Foo" where "Full.Name.Foo" is in local vars)
              & ABT.substsInheritAnnotation [(v, Type.var () (nameToVar name)) | (v, name) <- localResolutions]
  where
    localNames :: Set Name
    localNames =
      Set.map unsafeVarToName localVars

    getNamespaceMatches :: Name -> (Set TypeReference, Set TypeReference)
    getNamespaceMatches name =
      ( Names.lookupHQType Names.ExactName (HQ.NameOnly name) namespaceNamesLessLocalNames,
        Names.lookupHQType Names.IncludeSuffixes (HQ.NameOnly name) namespaceNamesLessLocalNames
      )
      where
        namespaceNamesLessLocalNames =
          over #types (Relation.subtractDom localNames) namespaceNames

    getLocalMatches :: Name -> Set Name
    getLocalMatches =
      (`Name.searchBySuffix` Relation.fromList (map (\name -> (name, name)) (Set.toList localNames)))
