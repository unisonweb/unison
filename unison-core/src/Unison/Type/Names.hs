module Unison.Type.Names
  ( bindNames,
  )
where

import Data.Sequence qualified as Seq
import Data.Set.NonEmpty qualified as NES
import Unison.ABT qualified as ABT
import Unison.HashQualified qualified as HQ
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.Type
import Unison.Util.List qualified as List
import Unison.Util.Set qualified as Set
import Unison.Var (Var)

-- | @bindNames varToName keepFree names type@ walks over all all free variable occurrences in @type@ (except those in
-- the given set @keepFree@), and replaces each one with its associated reference, found in @names@.
bindNames ::
  (Var v) =>
  (v -> Name.Name) ->
  Set v ->
  Names.Names ->
  Type v a ->
  Names.ResolutionResult v a (Type v a)
bindNames unsafeVarToName keepFree ns0 t =
  let ns = Names.NamesWithHistory ns0 mempty
      fvs = ABT.freeVarOccurrences keepFree t
      rs = [(v, a, Names.lookupHQType (HQ.fromName (unsafeVarToName v)) ns) | (v, a) <- fvs]
      ok (v, a, rs) =
        case Set.asSingleton rs of
          Just r -> Right (v, r)
          Nothing -> notOk case NES.nonEmptySet rs of
            Nothing -> Names.NotFound
            Just rs' -> Names.Ambiguous ns0 rs'
        where
          notOk err =
            Left (Seq.singleton (Names.TypeResolutionFailure v a err))
   in List.validate ok rs <&> \es -> bindExternal es t
