module Unison.UnisonFile.Summary
  ( FileSummary (..),
    allWatches,
    allTypeDecls,
    mkFileSummary,
    fileDefLocations,
  )
where

import Control.Lens
import Data.Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.DataDeclaration qualified as DD
import Unison.Names (Names)
import Unison.Parser.Ann
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Symbol
import Unison.Symbol qualified as Symbol
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Var qualified as Var
import Unison.WatchKind (pattern TestWatch)
import Unison.WatchKind qualified as WK

-- | A file that parses might not always type-check, but often we just want to get as much
-- information as we have available. This provides a type where we can summarize the
-- information available in a Unison file.
--
-- If the file typechecked then all the Ref Ids and types will be filled in, otherwise
-- they will be Nothing.
data FileSummary = FileSummary
  { dataDeclsBySymbol :: Map Symbol (Reference.Id, DD.DataDeclaration Symbol Ann),
    dataDeclsByReference :: Map Reference.Id (Map Symbol (DD.DataDeclaration Symbol Ann)),
    effectDeclsBySymbol :: Map Symbol (Reference.Id, DD.EffectDeclaration Symbol Ann),
    effectDeclsByReference :: Map Reference.Id (Map Symbol (DD.EffectDeclaration Symbol Ann)),
    termsBySymbol :: Map Symbol (Ann, Maybe Reference.Id, Term Symbol Ann, Maybe (Type Symbol Ann)),
    termsByReference :: Map (Maybe Reference.Id) (Map Symbol (Ann, Term Symbol Ann, Maybe (Type Symbol Ann))),
    testWatchSummary :: [(Ann, Maybe Symbol, Maybe Reference.Id, Term Symbol Ann, Maybe (Type Symbol Ann))],
    exprWatchSummary :: [(Ann, Maybe Symbol, Maybe Reference.Id, Term Symbol Ann, Maybe (Type Symbol Ann), Maybe WK.WatchKind)],
    fileNames :: Names
  }
  deriving stock (Show)

allWatches :: FileSummary -> [(Ann, Maybe Symbol, Maybe Reference.Id, Term Symbol Ann, Maybe (Type Symbol Ann), Maybe WK.WatchKind)]
allWatches FileSummary {testWatchSummary, exprWatchSummary} =
  exprWatchSummary
    <> (testWatchSummary <&> \(ann, sym, refId, tm, typ) -> (ann, sym, refId, tm, typ, Just WK.TestWatch))

allTypeDecls :: FileSummary -> Map Symbol (Reference.Id, Either (DD.EffectDeclaration Symbol Ann) (DD.DataDeclaration Symbol Ann))
allTypeDecls FileSummary {dataDeclsBySymbol, effectDeclsBySymbol} =
  let dataDecls = dataDeclsBySymbol <&> \(refId, dd) -> (refId, Right dd)
      effectDecls = effectDeclsBySymbol <&> \(refId, ed) -> (refId, Left ed)
   in dataDecls <> effectDecls

-- | Summarize the information available to us from the current state of the file.
-- See 'FileSummary' for more information.
mkFileSummary :: Maybe (UF.UnisonFile Symbol Ann) -> Maybe (UF.TypecheckedUnisonFile Symbol Ann) -> Maybe FileSummary
mkFileSummary parsed typechecked = case (parsed, typechecked) of
  (Nothing, Nothing) -> Nothing
  (_, Just tf@(UF.TypecheckedUnisonFileId {dataDeclarationsId', effectDeclarationsId', hashTermsId})) ->
    let (trms, testWatches, exprWatches) =
          hashTermsId & ifoldMap \sym (ann, ref, wk, trm, typ) ->
            case wk of
              Nothing -> (Map.singleton sym (ann, Just ref, trm, getUserTypeAnnotation sym <|> Just typ), mempty, mempty)
              Just TestWatch -> (mempty, [(ann, assertUserSym sym, Just ref, trm, getUserTypeAnnotation sym <|> Just typ)], mempty)
              Just wk -> (mempty, mempty, [(ann, assertUserSym sym, Just ref, trm, getUserTypeAnnotation sym <|> Just typ, Just wk)])
     in Just $
          FileSummary
            { dataDeclsBySymbol = dataDeclarationsId',
              dataDeclsByReference = declsRefMap dataDeclarationsId',
              effectDeclsBySymbol = effectDeclarationsId',
              effectDeclsByReference = declsRefMap effectDeclarationsId',
              termsBySymbol = trms,
              termsByReference = termsRefMap trms,
              testWatchSummary = testWatches,
              exprWatchSummary = exprWatches,
              fileNames = UF.typecheckedToNames tf
            }
  (Just uf@(UF.UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches}), _) ->
    let trms =
          terms & foldMap \(sym, ann, trm) ->
            (Map.singleton sym (ann, Nothing, trm, Nothing))
        (testWatches, exprWatches) =
          watches & ifoldMap \wk tms ->
            tms & foldMap \(v, ann, trm) ->
              case wk of
                TestWatch -> ([(ann, assertUserSym v, Nothing, trm, Nothing)], mempty)
                _ -> (mempty, [(ann, assertUserSym v, Nothing, trm, Nothing, Just wk)])
     in Just $
          FileSummary
            { dataDeclsBySymbol = dataDeclarationsId,
              dataDeclsByReference = declsRefMap dataDeclarationsId,
              effectDeclsBySymbol = effectDeclarationsId,
              effectDeclsByReference = declsRefMap effectDeclarationsId,
              termsBySymbol = trms,
              termsByReference = termsRefMap trms,
              testWatchSummary = testWatches,
              exprWatchSummary = exprWatches,
              fileNames = UF.toNames uf
            }
  where
    declsRefMap :: (Ord v, Ord r) => Map v (r, a) -> Map r (Map v a)
    declsRefMap m =
      m
        & Map.toList
        & fmap (\(v, (r, a)) -> (r, Map.singleton v a))
        & Map.fromListWith (<>)
    termsRefMap :: (Ord v, Ord r) => Map v (ann, r, a, b) -> Map r (Map v (ann, a, b))
    termsRefMap m =
      m
        & Map.toList
        & fmap (\(v, (ann, r, a, b)) -> (r, Map.singleton v (ann, a, b)))
        & Map.fromListWith (<>)
    -- Gets the user provided type annotation for a term if there is one.
    -- This type sig will have Ann's within the file if it exists.
    getUserTypeAnnotation :: Symbol -> Maybe (Type Symbol Ann)
    getUserTypeAnnotation v = do
      UF.UnisonFileId {terms, watches} <- parsed
      trm <- (terms <> fold watches) ^? folded . filteredBy (_1 . only v) . _3
      typ <- Term.getTypeAnnotation trm
      pure typ

    -- \| If a symbol is a 'User' symbol, return (Just sym), otherwise return Nothing.
    assertUserSym :: Symbol -> Maybe Symbol
    assertUserSym sym = case sym of
      Symbol.Symbol _ (Var.User {}) -> Just sym
      _ -> Nothing

-- | Compute the location of user defined definitions within the file
fileDefLocations :: FileSummary -> Map Symbol (Set Ann)
fileDefLocations fs@FileSummary {dataDeclsBySymbol, effectDeclsBySymbol, termsBySymbol} =
  fold
    [ dataDeclsBySymbol <&> \(_, decl) ->
        decl
          & DD.annotation
          & Set.singleton,
      effectDeclsBySymbol <&> \(_, decl) ->
        decl
          & DD.toDataDecl
          & DD.annotation
          & Set.singleton,
      (allWatches fs)
        & foldMap \(ann, maySym, _id, _trm, _typ, _wk) ->
          case maySym of
            Nothing -> mempty
            Just sym -> Map.singleton sym (Set.singleton ann),
      termsBySymbol <&> \(ann, _id, _trm, _typ) -> Set.singleton ann
    ]
