module Unison.Type.Names where

import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NES
import Unison.ABT qualified as ABT
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.Type
import Unison.Util.List qualified as List
import Unison.Var (Var)

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
      rs = [(v, a, Names.lookupHQType (Name.convert $ unsafeVarToName v) ns) | (v, a) <- fvs]
      ok (v, a, rs) =
        if Set.size rs == 1
          then pure (v, Set.findMin rs)
          else case NES.nonEmptySet rs of
            Nothing -> Left (pure (Names.TypeResolutionFailure v a Names.NotFound))
            Just rs' -> Left (pure (Names.TypeResolutionFailure v a (Names.Ambiguous ns0 rs')))
   in List.validate ok rs <&> \es -> bindExternal es t
