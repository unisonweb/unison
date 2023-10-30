module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,
  -- doSlurpAdds,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Semialign (alignWith, unzip, zip)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Data.Tuple.Strict (T2 (..))
import GHC.Clock (getMonotonicTime)
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.BranchV3 (BranchV3 (..), CausalBranchV3)
import U.Codebase.BranchV3 qualified as BranchV3
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.Queries qualified as Queries
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Term (Term)
import Unison.Builtin qualified as Builtins
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment, typecheckTerm)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as V1 (Branch (..), Branch0)
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Codebase.Causal qualified as V1 (Causal)
import Unison.Codebase.Causal qualified as V1.Causal
import Unison.Codebase.Causal.Type qualified as V1.Causal
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Merge2 qualified as Merge2
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Merge2 (MergeOutput)
import Unison.Merge2 qualified as Merge2
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Reference (TermReference)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Server.Backend qualified as Backend
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toText, unsafeFromVar)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as V1 (Term)
import Unison.Term qualified as V1.Term
import Unison.Type qualified as V1 (Type)
import Unison.Type qualified as V1.Type
import Unison.UnisonFile.Type (TypecheckedUnisonFile (TypecheckedUnisonFileId, dataDeclarationsId', effectDeclarationsId', hashTermsId), UnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Cache qualified as Cache
import Unison.Util.Map qualified as Map
import Unison.Util.Nametree
  ( Defns (..),
    Nametree (..),
    flattenNametree,
    traverseNametreeWithName,
    unflattenNametree,
  )
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star3 (Star3)
import Unison.Util.Star3 qualified as Star3
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip)

-- | Handle an @update@ command.
handleUpdate2 :: Text -> Input -> Set Name -> Cli ()
handleUpdate2 description input _requestedNames = do
  Cli.Env {codebase} <- ask
  unisonFile0 <- Cli.expectLatestTypecheckedFile
  currentPath' <- Cli.getCurrentPath
  db@Merge2.MergeDatabase {loadCausal, loadDeclNumConstructors, loadDeclType} <- Merge2.makeMergeDatabase codebase
  () <-
    Cli.runTransactionWithRollback \abort0 -> do
      -- Helper used throughout: abort this transaction with an output message.
      let abort :: Merge2.PreconditionViolation -> Transaction void
          abort =
            Merge2.mergePreconditionViolationToOutput db >=> abort0

      -- Load causals
      codebaseCausal <- Merge2.step "load codebase causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute currentPath')

      -- Load shallow branches
      codebaseBranch <- Merge2.step "load codebase shallow branch" $ Causal.value codebaseCausal

      -- Load deep definitions
      -- Merge2.NamespaceInfo codebaseCausalTree codebaseConstructorDeclRel codebaseDefns <-
      
      (codebaseCausalTree, codebaseConstructorDeclRel, codebaseDefns) <- do
        (definitions0, causalHashes) <- Merge2.step "load codebase definitions" $
          unzip <$> Merge2.loadNamespaceInfo abort (codebaseCausal ^. #causalHash) codebaseBranch
        (declNames, definitions1) <- Merge2.assertNamespaceSatisfiesPreconditions db abort wundefined codebaseBranch definitions0
        pure (causalHashes, declNames, definitions1)

      -- compute defns for updated codebase
      -- Map v (a {- ann for whole binding -}, TermReferenceId, Maybe WatchKind, Term v a, Type v a)
      let updatedDefns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
          updatedDefns =
            let T2 updatedTypes updatedTerms0 =
                  Map.foldlWithKey'
                    ( \(T2 typs terms) k v ->
                        T2
                          (BiMultimap.insert (Reference.ReferenceDerived v) (fromSymbol k) typs)
                          (BiMultimap.withoutRan (BiMultimap.lookupDom (fromSymbol k) codebaseConstructorDeclRel) terms)
                    )
                    (T2 codebaseTypes codebaseTerms)
                    ((fst <$> dataDeclarationsId' unisonFile0) <> (fst <$> effectDeclarationsId' unisonFile0))
                Defns codebaseTerms codebaseTypes = codebaseDefns
                updatedTerms1 =
                  Map.foldlWithKey'
                    (\b k (_, trefid, _, _, _) -> BiMultimap.insert (Referent.Ref (Reference.ReferenceDerived trefid)) (fromSymbol k) b)
                    updatedTerms0
                    (hashTermsId unisonFile0)
                
             in Defns updatedTerms1 updatedTypes

          fromSymbol :: Symbol -> Name
          fromSymbol = Name.unsafeFromVar

      pure ()
  undefined

--   -- check pre-reqs?
--   -- all dependencies are named
--   -- look for dependents by name in the namespace (whatToTypecheck?)
--   -- fill UnisonFile and typecheck
--   -- if typechecking fails, output to scratch file
--   -- if typechecking succeeds, write definitions to codebase and update namespace.
--   Cli.Env {codebase} <- ask
--   currentPath' <- Cli.getCurrentPath
--   slurpCheckNames <- Branch.toNames <$> Cli.getCurrentBranch0
--   sr <- getSlurpResultForUpdate requestedNames slurpCheckNames
--   let addsAndUpdates :: SlurpComponent
--       addsAndUpdates = Slurp.updates sr <> Slurp.adds sr
--       fileNames :: Names
--       fileNames = UF.typecheckedToNames (Slurp.originalFile sr)
--       -- todo: display some error if typeEdits or termEdits itself contains a loop
--       typeEdits3 :: [(Name, Reference, Reference)]
--       typeEdits3 = do
--         v <- Set.toList (SC.types (updates sr))
--         let n = Name.unsafeFromVar v
--         let oldRefs0 = Names.typesNamed slurpCheckNames n
--         let newRefs = Names.typesNamed fileNames n
--         case (,) <$> NESet.nonEmptySet oldRefs0 <*> Set.asSingleton newRefs of
--           Nothing -> error (reportBug "E722145" ("bad (old,new) names: " ++ show (oldRefs0, newRefs)))
--           Just (oldRefs, newRef) -> do
--             oldRef <- Foldable.toList oldRefs
--             [(n, oldRef, newRef)]
--       typeEdits2 :: [(Name, Reference)]
--       typeEdits2 = map (\(name, _old, new) -> (name, new)) typeEdits3
--       termEdits3 :: [(Name, Reference, Reference)]
--       termEdits3 = do
--         v <- Set.toList (SC.terms (updates sr))
--         let n = Name.unsafeFromVar v
--         let oldRefs0 = Names.refTermsNamed slurpCheckNames n
--         let newRefs = Names.refTermsNamed fileNames n
--         case (,) <$> NESet.nonEmptySet oldRefs0 <*> Set.asSingleton newRefs of
--           Nothing -> error (reportBug "E936103" ("bad (old,new) names: " ++ show (oldRefs0, newRefs)))
--           Just (oldRefs, newRef) -> do
--             oldRef <- Foldable.toList oldRefs
--             [(n, oldRef, newRef)]
--       termEdits2 = map (\(name, _old, new) -> (name, new)) typeEdits3
--       termDeprecations :: [(Name, Referent)]
--       termDeprecations =
--         [ (n, r)
--           | (_, oldTypeRef, _) <- typeEdits3,
--             (n, r) <- Names.constructorsForType oldTypeRef slurpCheckNames
--         ]
--   when (Slurp.hasAddsOrUpdates sr) $ do
--     -- take a look at the `updates` from the SlurpResult
--     -- and make a patch diff to record a replacement from the old to new references
--     Cli.stepManyAtMNoSync
--       ( [ ( Path.unabsolute currentPath',
--             pure . doSlurpUpdates typeEdits2 termEdits2 termDeprecations
--           ),
--           ( Path.unabsolute currentPath',
--             pure . doSlurpAdds addsAndUpdates (Slurp.originalFile sr)
--           )
--         ]
--       )
--     Cli.runTransaction
--       . Codebase.addDefsToCodebase codebase
--       . Slurp.filterUnisonFile sr
--       $ Slurp.originalFile sr
--   ppe <- prettyPrintEnvDecl =<< displayNames (Slurp.originalFile sr)
--   Cli.respond $ SlurpOutput input (PPE.suffixifiedPPE ppe) sr
--   Cli.syncRoot description

-- -- | given a set of names to add to a namespace, compute the slurpresult, status for the various names
-- getSlurpResultForUpdate :: Set Name -> Names -> Cli SlurpResult
-- getSlurpResultForUpdate requestedNames slurpCheckNames = do
--   let slurp :: TypecheckedUnisonFile Symbol Ann -> SlurpResult
--       slurp file =
--         Slurp.slurpFile file (Set.map Name.toVar requestedNames) Slurp.UpdateOp slurpCheckNames

--   let termRefToNames :: TermReferenceId -> Set Symbol
--       termRefToNames =
--         Set.map Name.toVar . Names.namesForReferent slurpCheckNames . Referent.fromTermReferenceId

--   let nameToTermRefs :: Symbol -> Set TermReference
--       nameToTermRefs = Names.refTermsNamed slurpCheckNames . Name.unsafeFromVar

--   slurp1 <- do
--     Cli.Env {codebase} <- ask
--     unisonFile0 <- Cli.expectLatestTypecheckedFile

--     -- Here, we identify whether there are any "implicit terms", which are terms that should be brought into the latest
--     -- typechecked Unsion file and re-typechecked, which also re-discovers components.
--     --
--     -- The running example here will be one in which the current namespace already has the following terms stored.
--     -- (For simplicity, we will ignore the limitation that mutually recursive definitions must be top-level arrows;
--     -- pretend this is Haskell's let).
--     --
--     --   ping = pong + 1   (reference = #pingpong.ping)
--     --   pong = ping + 2   (reference = #pingpong.pong)
--     --   wham = pong + 3   (reference = #wham)
--     --
--     -- The user re-defines "ping" in their scratch file thus:
--     --
--     --   ping = wham + 4   (reference = #newping)
--     --
--     -- Note that, pre-update, we had two components [ping,pong] and [wham]. But after the update, since the new `ping`
--     -- refers to the old `wham`, which refers to the old `pong`, which refers to the old `ping`, we really want to end
--     -- up with a single component in the end, [ping,pong,wham].

--     -- First, compute an initial slurp, which will identify the initial set of definitions we are updating ({"ping"}).
--     -- Also, this will be the slurp that we fall back on, in case re-typechecking another Unison file with implicit
--     -- terms in it fails.
--     let slurp0 = slurp unisonFile0

--     -- Grab some interim info out of the original slurp.
--     --
--     -- Running example:
--     --
--     --   "ping" => (#newping, Nothing, <#wham + 4>, <Nat>)
--     let nameToInterimInfo :: Map Symbol (Ann, TermReferenceId, Maybe WatchKind, Term Symbol Ann, Type Symbol Ann)
--         nameToInterimInfo =
--           UF.hashTermsId (Slurp.originalFile slurp0)

--     -- Get the set of names that are being updated.
--     --
--     -- Running example:
--     --
--     --   { "ping" }
--     let namesBeingUpdated :: Set Symbol
--         namesBeingUpdated =
--           SC.terms (Slurp.updates slurp0)

--     -- Associate each such name with the set of old (already in the codebase) and new (in the scratch file) references
--     -- that it's associated with.
--     --
--     -- Running example:
--     --
--     --   "ping" => ({ #pingpong.ping }, #newping)
--     let updatedNameToRefs :: Map Symbol (Set TermReference, TermReferenceId)
--         updatedNameToRefs =
--           Map.fromSet
--             ( \name ->
--                 case Map.lookup name nameToInterimInfo of
--                   Nothing -> error (reportBug "E798907" "no interim ref for name")
--                   Just (_, interimRef, _, _, _) -> (nameToTermRefs name, interimRef)
--             )
--             namesBeingUpdated

--     -- Identify all of the implicit terms, which are:
--     --
--     --   - Both:
--     --     - Either:
--     --         - (A) A component-mate of a term that's being updated.
--     --         - Both:
--     --           - (B) A dependent of a term that's being updated.
--     --           - (C) A dependency of the new term.
--     --     - (D) Not being updated.
--     --
--     -- FIXME Additionally, we have one more temporary requirement.
--     --
--     --   - (E) The term has at least one unambiguous (unconflicted) name in the codebase.
--     --
--     -- This works around two fixable issues:
--     --
--     --   1. If the term has no names in the namespace, then we can't successfully put it into the Unison file anyway and
--     --      forward it through the typecheck + slurp process, because slurping currently assumes that it can freely
--     --      convert back-and-forth between vars and names.
--     --
--     --   2. If the term has only ambiguous/conflicted names, then putting one of them in the Unison file and proceeding
--     --      to do an update would have the undesirable side-effect of resolving the conflict.
--     --
--     -- FIXME don't bother for type-changing updates
--     --
--     -- In our running example, the full list of component-mates (A) of the terms being updated is:
--     --
--     --   [ #pingpong.ping, #pingpong.pong ]
--     --
--     -- And because #pingpong.ping is being updated, due to (D), only #pingpong.pong remains.
--     --
--     -- Furthermore, #wham is both a dependent of #pingpong (B), and a dependency of #newping, so it too is an implicit
--     -- term.
--     --
--     -- Running example:
--     --
--     --   #pingpong.pong => (<#pingpong.ping + 2>, "pong")
--     --   #wham          => (<#pingpong.pong + 3>, "wham")
--     implicitTerms :: Map TermReferenceId (Term Symbol Ann, Symbol) <-
--       liftIO do
--         Codebase.withConnection codebase \conn ->
--           Sqlite.runTransaction conn do
--             -- Running example:
--             --
--             --   #pingpong => #newping
--             let oldHashToInterimHash :: Map Hash Hash
--                 oldHashToInterimHash =
--                   updatedNameToRefs & foldMap \(oldRefs, interimRef) ->
--                     let interimHash = Reference.idToHash interimRef
--                      in Map.fromList $
--                           oldRefs
--                             & Set.toList
--                             & mapMaybe Reference.toHash
--                             & map (,interimHash)

--             let hashToImplicitTerms :: Hash -> Sqlite.Transaction (Map TermReferenceId (Term Symbol Ann, Symbol))
--                 hashToImplicitTerms hash = do
--                   -- Running example (for `oldHash` iteration):
--                   --
--                   --   [ (<#pingpong.pong + 1>, <Nat>),
--                   --     (<#pingpong.ping + 2>, <Nat>)
--                   --   ]
--                   terms <- Codebase.unsafeGetTermComponent codebase hash
--                   let keep :: TermReferenceId -> Maybe Symbol
--                       keep ref =
--                         if notBeingUpdated names
--                           then Foldable.find nameIsUnconflicted names -- (E)
--                           else Nothing
--                         where
--                           -- (D) After getting the entire component of `oldHash`, which has at least one member being
--                           -- updated, we want to keep only the members that are *not* being updated (i.e. those who have
--                           -- no name that is being updated).
--                           --
--                           -- Running example, first time through (processing #pingpong.ping):
--                           --
--                           --   Set.disjoint { "ping" } { "ping" } is false, so don't add to the map.
--                           --
--                           -- Running example, second time through (processing #pingpong.pong):
--                           --
--                           --   Set.disjoint { "ping" } { "pong" } is true, so add
--                           --   #pingpong.pong => (<#pingpong.ping + 2>, { "pong" })) to the map.
--                           notBeingUpdated = Set.disjoint namesBeingUpdated
--                           nameIsUnconflicted name = Set.size (nameToTermRefs name) == 1
--                           names = termRefToNames ref
--                   pure $
--                     terms
--                       -- Running example:
--                       --
--                       --   [ (#pingpong.ping, (<#pingpong.pong + 1>, <Nat>)),
--                       --     (#pingpong.pong, (<#pingpong.ping + 2>, <Nat>))
--                       --   ]
--                       & Reference.componentFor hash
--                       & List.foldl'
--                         ( \acc (ref, (term, _typ)) ->
--                             case keep ref of
--                               Nothing -> acc
--                               Just name -> Map.insert ref (term, name) acc
--                         )
--                         Map.empty

--             if Map.null oldHashToInterimHash
--               then pure Map.empty
--               else do
--                 Sqlite.savepoint do
--                   -- Compute the actual interim decl/term components in the latest typechecked file. These aren't quite
--                   -- given in the unison file structure itself - in the `topLevelComponents'` field we have the
--                   -- components in some arbitrary order (I *think*), each tagged with its stringy name, and in the
--                   -- `hashTermsId` field we have all of the individual terms organized by reference.
--                   let interimDeclComponents :: [(Hash, [Decl Symbol Ann])]
--                       interimDeclComponents =
--                         decls UF.dataDeclarationsId' Right ++ decls UF.effectDeclarationsId' Left
--                         where
--                           decls ::
--                             (TypecheckedUnisonFile Symbol Ann -> Map Symbol (TypeReferenceId, decl)) ->
--                             (decl -> Decl Symbol Ann) ->
--                             [(Hash, [Decl Symbol Ann])]
--                           decls project inject =
--                             slurp0
--                               & Slurp.originalFile
--                               & project
--                               & Map.elems
--                               & recomponentize
--                               & over (mapped . _2 . mapped) inject
--                       interimTermComponents :: [(Hash, [(Term Symbol Ann, Type Symbol Ann)])]
--                       interimTermComponents =
--                         nameToInterimInfo
--                           & Map.elems
--                           & map (\(_ann, ref, _wk, term, typ) -> (ref, (term, typ)))
--                           & componentize
--                           & uncomponentize

--                   -- Insert each interim component into the codebase proper. Note: this relies on the codebase interface
--                   -- being smart enough to handle out-of-order components (i.e. inserting a dependent before a
--                   -- dependency). That's currently how the codebase interface works, but maybe in the future it'll grow
--                   -- a precondition that components can only be inserted after their dependencies.
--                   for_ interimDeclComponents \(hash, decls) -> Codebase.putTypeDeclarationComponent codebase hash decls
--                   for_ interimTermComponents \(hash, terms) -> Codebase.putTermComponent codebase hash terms

--                   terms <-
--                     let interimHashes :: Set Hash
--                         interimHashes = Set.fromList (map fst interimTermComponents)
--                      in Map.toList oldHashToInterimHash & foldMapM \(oldHash, interimHash) -> do
--                           hashes <-
--                             Queries.loadObjectIdForAnyHash oldHash >>= \case
--                               -- better would be to short-circuit all the way to the user and say, actually we can't
--                               -- perform this update at all, due to some intervening delete (e.g. some sort of
--                               -- hard-reset or garbage collection on the codebase)
--                               Nothing -> pure Set.empty
--                               Just oldOid -> do
--                                 interimOid <- Queries.expectObjectIdForPrimaryHash interimHash
--                                 betweenOids <- Queries.getDependenciesBetweenTerms interimOid oldOid
--                                 (Set.\\ interimHashes) <$> Set.traverse Queries.expectPrimaryHashByObjectId betweenOids
--                           foldMapM hashToImplicitTerms (oldHash : Set.toList hashes)
--                   pure (Left terms) -- left = rollback to savepoint
--     if Map.null implicitTerms
--       then pure slurp0
--       else do
--         -- We found some implicit terms, so it's time to:
--         --
--         --   1. Reconstruct a new unison file out of the latest typechecked unison file, plus all of the implicit terms,
--         --      taking care to adjust their references to each other, so that the proper components are discovered.
--         --
--         --   2. Re-typecheck, and if it that succeeds, use the resulting typechecked unison file and slurp.

--         -- Remap the references contained in each implicit term, then add back in the slurped interim terms and unhash
--         -- the collection. The unhashing process will invent a fresh variable name for each term.
--         --
--         -- Running example:
--         --
--         --   #newping       => ("fresh1", <"fresh3" + 4>)
--         --   #pingpong.pong => ("fresh2", <"fresh1" + 2>)
--         --   #wham          => ("fresh3", <"fresh2" + 3>)
--         let refToGeneratedNameAndTerm :: Map TermReferenceId (Symbol, Term Symbol Ann)
--             refToGeneratedNameAndTerm =
--               -- Running example:
--               --
--               --   #pingpong.pong => (<#pingpong.ping + 2>, { "pong" })
--               --   #wham          => (<#pingpong.pong + 3>, { "wham" })
--               implicitTerms
--                 -- Running example:
--                 --
--                 --   #pingpong.pong => <#newping + 2>
--                 --   #wham          => <#pingpong.pong + 3>
--                 & Map.map (\(term, _names) -> rewrite term)
--                 -- Running example:
--                 --
--                 --   #newping       => <#wham + 4>
--                 --   #pingpong.pong => <#newping + 2>
--                 --   #wham          => <#pingpong.pong + 3>
--                 & Map.union interimRefToTerm
--                 & Term.unhashComponent
--               where
--                 -- Running example:
--                 --
--                 --   #newping => <#wham + 4>
--                 interimRefToTerm :: Map TermReferenceId (Term Symbol Ann)
--                 interimRefToTerm =
--                   Map.remap (\(_var, (_ann, ref, _wk, term, _typ)) -> (ref, term)) nameToInterimInfo
--                 -- Running example: apply the following reference mapping everwhere in a term:
--                 --
--                 --   #pingpong.ping -> #newping
--                 --   ref            -> ref
--                 rewrite :: Term Symbol Ann -> Term Symbol Ann
--                 rewrite =
--                   rewriteTermReferences (foldMap toMapping updatedNameToRefs)
--                   where
--                     toMapping ::
--                       (Set TermReference, TermReferenceId) ->
--                       Map TermReference TermReferenceId
--                     toMapping (oldRefs, interimRef) =
--                       foldMap (\oldRef -> Map.singleton oldRef interimRef) oldRefs

--         let unisonFile :: UnisonFile Symbol Ann
--             unisonFile =
--               UnisonFileId
--                 { dataDeclarationsId = UF.dataDeclarationsId' (Slurp.originalFile slurp0),
--                   effectDeclarationsId = UF.effectDeclarationsId' (Slurp.originalFile slurp0),
--                   -- Running example:
--                   --
--                   --   fresh1 = fresh3 + 4
--                   --   fresh2 = fresh1 + 2
--                   --   fresh3 = fresh2 + 3
--                   terms =
--                     Map.elems refToGeneratedNameAndTerm <&> \(v, term) ->
--                       (v, External, term),
--                   -- In the context of this update, whatever watches were in the latest typechecked Unison file are
--                   -- irrelevant, so we don't need to copy them over.
--                   watches = Map.empty
--                 }
--         typecheckingEnv <-
--           liftIO do
--             Codebase.runTransaction codebase do
--               computeTypecheckingEnvironment FileParsers.ShouldUseTndr'No codebase [] unisonFile
--         case Result.result (FileParsers.synthesizeFile typecheckingEnv unisonFile) of
--           Just file0 -> do
--             -- Map each name generated by unhashing back to the name it should have in the Unison file we're going to
--             -- typecheck.
--             --
--             -- Running example:
--             --
--             --   "fresh1" -> "ping"
--             --   "fresh2" -> "pong"
--             --   "fresh3" -> "wham"
--             let generatedNameToName :: Map Symbol Symbol
--                 generatedNameToName =
--                   refToGeneratedNameAndTerm & Map.remap \(ref, (generatedName, _term)) ->
--                     ( generatedName,
--                       case Map.lookup ref interimRefToName of
--                         Just name -> name
--                         Nothing ->
--                           case Map.lookup ref implicitTerms of
--                             Just (_term, name) -> name
--                             Nothing -> error (reportBug "E836680" "ref not interim nor implicit")
--                     )
--                   where
--                     -- Associate each term name being updated with its interim reference.
--                     --
--                     -- Running example:
--                     --
--                     --   #newping => "ping"
--                     interimRefToName :: Map TermReferenceId Symbol
--                     interimRefToName =
--                       Map.remap (\(name, (_ann, ref, _wk, _term, _typ)) -> (ref, name)) nameToInterimInfo

--             let renameTerm ::
--                   (Symbol, Ann, Term Symbol Ann, Type Symbol Ann) ->
--                   (Symbol, Ann, Term Symbol Ann, Type Symbol Ann)
--                 renameTerm (generatedName, ann, term, typ) =
--                   ( case Map.lookup generatedName generatedNameToName of
--                       Just name -> name
--                       Nothing -> error (reportBug "E440546" "no name for generated name"),
--                     ann,
--                     ABT.renames generatedNameToName term,
--                     typ
--                   )

--             let file1 :: TypecheckedUnisonFile Symbol Ann
--                 file1 =
--                   UF.typecheckedUnisonFile
--                     (file0 ^. #dataDeclarationsId')
--                     (file0 ^. #effectDeclarationsId')
--                     ((file0 ^. #topLevelComponents') & over (mapped . mapped) renameTerm)
--                     ((file0 ^. #watchComponents) & over (mapped . _2 . mapped) renameTerm)

--             pure (slurp file1)
--           _ -> pure slurp0

--   pure slurp1

-- rewriteTermReferences :: (Ord v) => Map TermReference TermReferenceId -> Term v a -> Term v a
-- rewriteTermReferences mapping =
--   ABT.rebuildUp \term ->
--     case term of
--       Term.Ref ref0 ->
--         case Map.lookup ref0 mapping of
--           Nothing -> term
--           Just ref1 -> Term.Ref (Reference.fromId ref1)
--       _ -> term

-- -- updates the namespace for adding `slurp`
-- doSlurpAdds ::
--   forall m.
--   (Monad m) =>
--   SlurpComponent ->
--   TypecheckedUnisonFile Symbol Ann ->
--   (Branch0 m -> Branch0 m)
-- doSlurpAdds slurp uf = Branch.batchUpdates (typeActions <> termActions)
--   where
--     typeActions = map doType . toList $ SC.types slurp
--     termActions =
--       map doTerm . toList $
--         SC.terms slurp <> UF.constructorsForDecls (SC.types slurp) uf
--     names = UF.typecheckedToNames uf
--     tests = Set.fromList $ view _1 <$> UF.watchesOfKind WK.TestWatch (UF.discardTypes uf)
--     (isTestType, isTestValue) = IOSource.isTest
--     md v =
--       if Set.member v tests
--         then Metadata.singleton isTestType isTestValue
--         else Metadata.empty
--     doTerm :: Symbol -> (Path, Branch0 m -> Branch0 m)
--     doTerm v = case toList (Names.termsNamed names (Name.unsafeFromVar v)) of
--       [] -> errorMissingVar v
--       [r] ->
--         let split = Path.splitFromName (Name.unsafeFromVar v)
--          in BranchUtil.makeAddTermName split r (md v)
--       wha ->
--         error $
--           "Unison bug, typechecked file w/ multiple terms named "
--             <> Var.nameStr v
--             <> ": "
--             <> show wha
--     doType :: Symbol -> (Path, Branch0 m -> Branch0 m)
--     doType v = case toList (Names.typesNamed names (Name.unsafeFromVar v)) of
--       [] -> errorMissingVar v
--       [r] ->
--         let split = Path.splitFromName (Name.unsafeFromVar v)
--          in BranchUtil.makeAddTypeName split r Metadata.empty
--       wha ->
--         error $
--           "Unison bug, typechecked file w/ multiple types named "
--             <> Var.nameStr v
--             <> ": "
--             <> show wha
--     errorMissingVar v = error $ "expected to find " ++ show v ++ " in " ++ show uf

-- -- | create a function that adds/replaces the name mappings in a Branch0
-- doSlurpUpdates ::
--   (Monad m) =>
--   [(Name, TypeReference)] ->
--   [(Name, TermReference)] ->
--   [(Name, Referent)] ->
--   (Branch0 m -> Branch0 m)
-- doSlurpUpdates typeEdits termEdits deprecated b0 =
--   Branch.batchUpdates (typeActions <> termActions <> deprecateActions) b0
--   where
--     typeActions = join . map doType $ typeEdits
--     termActions = join . map doTerm $ termEdits
--     deprecateActions = join . map doDeprecate $ deprecated
--       where
--         doDeprecate (n, r) = [BranchUtil.makeDeleteTermName (Path.splitFromName n) r]

--     -- we copy over the metadata on the old thing
--     -- todo: if the thing being updated, m, is metadata for something x in b0
--     -- update x's md to reference `m`
--     doType :: (Name, TypeReference) -> [(Path, Branch0 m -> Branch0 m)]
--     doType (n, new) =
--       let split = Path.splitFromName n
--        in [ BranchUtil.makeReplaceTypeNames split new
--           ]
--     doTerm :: (Name, TermReference) -> [(Path, Branch0 m -> Branch0 m)]
--     doTerm (n, new) =
--       [ BranchUtil.makeReplaceTermNames split (Referent.Ref new)
--       ]
--       where
--         split = Path.splitFromName n

-- recomponentize :: [(Reference.Id, a)] -> [(Hash, [a])]
-- recomponentize =
--   uncomponentize . componentize

-- -- Misc. helper: convert a component in listy-form to mappy-form.
-- componentize :: [(Reference.Id, a)] -> Map Hash (Map Reference.Pos a)
-- componentize =
--   foldl' step Map.empty
--   where
--     step :: Map Hash (Map Reference.Pos a) -> (Reference.Id, a) -> Map Hash (Map Reference.Pos a)
--     step acc (Reference.Id hash pos, x) =
--       Map.upsert
--         ( \case
--             Nothing -> Map.singleton pos x
--             Just acc1 -> Map.insert pos x acc1
--         )
--         hash
--         acc

-- -- Misc. helper: convert a component in mappy-form to listy-form.
-- uncomponentize :: Map Hash (Map Reference.Pos a) -> [(Hash, [a])]
-- uncomponentize =
--   over (mapped . _2) Map.elems . Map.toList

-- type DeepRefs = Defns (Map Name Referent) (Map Name TypeReference)

-- type UpdateNames = Defns (Set Name) (Set Name)

-- whatToTypecheck :: UpdateNames -> DeepRefs -> Transaction DeepRefsId'
-- whatToTypecheck fileUpdates namespace = do
--   let -- \| Find the `Reference.Id`s that comprise a namespace.
--       -- The results of the search will be a subset of these references.
--       -- Constructors show up as decl Ids.
--       makeScope dr = Set.fromList $ doTerms (dr ^. #terms) <> doTypes (dr ^. #types)
--         where
--           doTerms :: Map Name Referent -> [Reference.Id]
--           doTerms = mapMaybe Referent.toReferenceId . Map.elems
--           doTypes :: Map Name TypeReference -> [Reference.Id]
--           doTypes = mapMaybe Reference.toId . Map.elems

--   let -- \| Returns things from `scope` that have the same names as things in `updates` updates
--       -- the return type should be "things that can be dependencies in the db".
--       -- This implementation returns a decl reference in place of a constructor reference.
--       getByCorrespondingName :: DeepRefs -> UpdateNames -> Set Reference
--       getByCorrespondingName scope updates = Set.fromList $ map doTerm (updates ^. #terms & toList) <> map doType (updates ^. #types & toList)
--         where
--           -- \| doTerms will return a TypeReference from a Constructor
--           doTerm :: Name -> Reference
--           doTerm name = fromMaybe err $ Referent.toReference <$> Map.lookup name (scope ^. #terms)
--             where
--               err = error $ "delete / update conflict on term " ++ Name.toString name
--           doType :: Name -> Reference
--           doType name = fromMaybe err $ Map.lookup name (scope ^. #types)
--             where
--               err = error $ "delete / update conflict on type " ++ Name.toString name

--   -- these dependents are just term/decl ids
--   dependentsOfUpdates <- Ops.dependentsWithinScope (makeScope namespace) (getByCorrespondingName namespace fileUpdates)

--   -- filter the namespace to just the refs in question
--   let namesForRef :: forall r. Ord r => Map Name r -> Set r -> Map Name r
--       namesForRef names defs = Map.filter (flip Set.member defs) names

--   let filterDependents :: forall a b. (Ord a, Eq b) => b -> Map a b -> Set a
--       filterDependents rt m = Set.fromList [r | (r, t) <- Map.toList m, t == rt]

--   let -- \| terms to typecheck. they'll be Ids because that's all we have in the database as dependents.
--       latestTermDependents :: Map Name TermReferenceId
--       latestTermDependents = uncurry namesForRef (setup namespace dependentsOfUpdates)
--         where
--           setup :: DeepRefs -> Map Reference.Id ReferenceType -> (Map Name TermReferenceId, Set TermReferenceId)
--           setup dr dependents = (dropCtorsAndBuiltins (dr ^. #terms), filterDependents RtTerm dependents)
--           dropCtorsAndBuiltins = Map.mapMaybe Referent.toReferenceId

--   let -- \| decls to typecheck
--       latestTypeDependents :: Map Name TypeReferenceId
--       latestTypeDependents = uncurry namesForRef (setup namespace dependentsOfUpdates)
--         where
--           setup :: DeepRefs -> Map Reference.Id ReferenceType -> (Map Name TypeReferenceId, Set TypeReferenceId)
--           setup dr dependents = (dropBuiltins (dr ^. #types), filterDependents RtType dependents)
--           dropBuiltins = Map.mapMaybe \case Reference.ReferenceDerived r -> Just r; _ -> Nothing

--   pure $ Defns latestTermDependents latestTypeDependents
