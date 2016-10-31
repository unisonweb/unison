{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase where

-- import Data.Bytes.Serial (Serial)
import Control.Monad
import Control.Applicative
import Data.Aeson.TH
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import Data.Text (Text)
import Unison.Builtin (Builtin(..))
import Unison.Codebase.Store (Store)
import Unison.Metadata (Metadata)
import Unison.Note (Noted(..),Note(..))
import Unison.Paths (Path)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.TermEdit (Action)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Codebase.Store as Store
import qualified Unison.Hash as Hash
import qualified Unison.Interpreter as Interpreter
import qualified Unison.Metadata as Metadata
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Unison.Paths as Paths
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TermEdit as TermEdit
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Typechecker.Components as Components
import qualified Unison.Var as Var
-- import Debug.Trace

-- watch :: Show a => String -> a -> a
-- watch msg a = traceShow (msg ++ ": " ++ show a) a

-- | The results of a search.
-- On client, only need to repeat the query if we modify a character
-- at one of the examined positions OR if we add a character to a search
-- that previously returned incomplete results. Appending characters to a
-- search that returned complete results just filters down the set and
-- can be done client-side, assuming the client has the full result set.
data SearchResults v h e =
  SearchResults
    { query :: Metadata.Query
    , references :: [(h, Metadata v h)]
    , matches :: ([e], Int)
    , illTypedMatches :: ([e], Int)
    , positionsExamined :: [Int] }

data LocalInfo e t =
  LocalInfo
    { localTerm :: e
    , localType :: t
    , localAdmissibleType :: t
    , localVariables :: [e]
    , localOverapplications :: [Int]
    , localVariableApplications :: [e] }

deriveJSON defaultOptions ''SearchResults
deriveJSON defaultOptions ''LocalInfo

-- | The Unison code API:
--   * `m` is the monad
--   * `v` is the type of variables
--   * `h` is the type of hashes
--   * `t` is for type
--   * `e` is for term (mnemonic "expression")
data Codebase m v h t e = Codebase {
  -- | Obtain the type of the given subterm, assuming the path is valid
  admissibleTypeAt :: e -> Path -> Noted m t,
  -- | Create a new term and provide its metadata
  createTerm :: e -> Metadata v h -> Noted m h,
  -- | Create a new type and provide its metadata
  createType :: t -> Metadata v h -> Noted m h,
  -- | Lookup the direct dependencies of @k@, optionally limited to the given set
  dependencies :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Lookup the set of terms/types depending directly on the given @k@, optionally limited to the given set
  dependents :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Modify the given subterm, which may fail. First argument is the root path.
  -- Second argument is path relative to the root.
  -- Returns (root path, original e, edited e, new cursor position)
  editTerm :: Path -> Path -> Action v -> e -> Noted m (Maybe (Path,e,e,Path)),
  -- | Return information about local types and and variables in scope
  localInfo :: e -> Path -> Noted m (LocalInfo e t),
  -- | Access the metadata for the term and/or types identified by @k@
  metadatas :: [h] -> Noted m (Map h (Metadata v h)),
  -- | Search for a term, optionally constrained to be of the given type
  search :: e -> Path -> Int -> Metadata.Query -> Maybe t -> Noted m (SearchResults v h e),
  -- | Lookup the source of the term identified by @h@
  terms :: [h] -> Noted m (Map h e),
  -- | Lookup the dependencies of @h@, optionally limited to those that intersect the given set
  transitiveDependencies :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Lookup the set of terms or types which depend on the given @k@, optionally limited to those that intersect the given set
  transitiveDependents :: Maybe (Set h) -> h -> Noted m (Set h),
  -- | Lookup the source of the type identified by @h@
  types :: [h] -> Noted m (Map h t),
  -- | Obtain the type of the given subterm, assuming the path is valid
  typeAt :: e -> Path -> Noted m t,
  -- | Update the metadata associated with the given term or type
  updateMetadata :: h -> Metadata v h -> Noted m ()
}

addBuiltins :: Monad f
            => [Builtin v]
            -> Store f v
            -> Codebase f v Reference.Reference (Type v) (Term v)
            -> f ()
addBuiltins builtins store code = Note.run $
  forM_ builtins $ \(Builtin r _ t md) -> do
    updateMetadata code r md
    Store.annotateTerm store r t

make :: (Show v, Alternative f, Monad f, Var v)
     => (Term v -> Reference)
     -> Store f v
     -> Codebase f v Reference.Reference (Type v) (Term v)
make hash store =
  let
    readTypeOf = Store.typeOfTerm store

    admissibleTypeAt e loc =
      Typechecker.admissibleTypeAt readTypeOf loc e

    createTerm e md = do
      t <- Typechecker.synthesize readTypeOf e
      -- todo: a bit of fanciness to allow annotations that don't constrain type to not participate
      -- in the hash, perhaps add a Bool flag to Ann ctor
      let r = hash e
      pure r <* case r of
        Reference.Builtin _ ->
          Store.writeMetadata store r md -- can't change builtin types, just metadata
        Reference.Derived h -> do
          new <- (False <$ Store.readTerm store h) <|> pure True
          md0 <- (Just <$> Store.readMetadata store r) <|> pure Nothing
          Store.writeMetadata store r (Metadata.combine md0 md)
          when new $ do
            Store.writeTerm store h e
            Store.annotateTerm store r t

    createType _ _ = error "todo - createType"

    dependencies _ (Reference.Builtin _) = pure Set.empty
    dependencies limit (Reference.Derived h) = let trim = maybe id Set.intersection limit in do
      e <- Store.readTerm store h
      pure $ trim (Set.map Reference.Derived (Term.dependencies e))

    dependents limit h = do
      hs <- Store.hashes store limit
      hs' <- mapM (\h -> (,) h <$> dependencies Nothing h)
                  (Set.toList hs)
      pure $ Set.fromList [x | (x,deps) <- hs', Set.member h deps]

    edit rootPath path action e = pure $ do
      e <- Paths.atTerm rootPath e
      (newPath, e') <- TermEdit.interpret path action e
      pure (rootPath, e, e', newPath)

    metadatas hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> Store.readMetadata store h) hs)

    localInfo e loc = do
      current <- Typechecker.typeAt readTypeOf loc e
      admissible <- Typechecker.admissibleTypeAt readTypeOf loc e
      locals <- Typechecker.locals readTypeOf loc e
      annotatedLocals <- pure $ map (\(v,t) -> Term.var v `Term.ann` t) locals
      let f focus = maybe (pure False)
                          (\e -> Typechecker.wellTyped readTypeOf e)
                          (Paths.modifyTerm (const (Term.wrapV focus)) loc e)
      let fi (e,_) = f e
      let currentApplies = maybe [] (\e -> TermEdit.applications e admissible) (Paths.atTerm loc e) `zip` [0..]
      matchingCurrentApplies <- case Paths.atTerm loc e of
        -- if we're pointing to a Var, matchingCurrentApplies is redundant with `matchingLocals`
        Just (Term.Var' _) -> pure []
        _ -> map snd <$> filterM fi currentApplies
      subterm <- maybe (fail "invalid path") pure (Paths.atTerm loc e)
      matchingLocals <- filterM f (locals >>= (\(v,t) -> TermEdit.applications (Term.var v) t))
      pure $ LocalInfo subterm current admissible annotatedLocals matchingCurrentApplies matchingLocals

    search e loc limit query _ =
      let
        typeOk focus = maybe (pure False)
                             (\e -> Typechecker.wellTyped readTypeOf e)
                             (Paths.modifyTerm (const (Term.wrapV focus)) loc e)
        elaborate h = (\t -> TermEdit.applications (Term.ref h) t) <$> readTypeOf h
        queryOk e = do mds <- traverse (Store.readMetadata store) (Set.toList (Term.dependencies' e))
                       pure $ any (Metadata.matches query) mds
        trim rs =
          let rs' = sortBy (comparing fst) (map (\e -> (negate (Term.countBlanks e), e)) rs)
          in (map snd (take limit rs'), length (drop limit rs'))
      in
      do
        hs <- Store.hashes store Nothing
        tmatches <- do es <- traverse elaborate (Set.toList hs)
                       filterM typeOk (join es)
        qmatches <- filterM queryOk tmatches
        qmatches' <- filterM queryOk (map Term.ref (Set.toList hs))
        illtypedQmatches <-
          -- return type annotated versions of ill-typed terms
          let welltypedRefs = Set.fromList (map hash qmatches)
              terms = filter (\r -> Set.notMember (hash r) welltypedRefs) qmatches'
          in zipWith Term.ann terms <$> traverse (Typechecker.synthesize readTypeOf) terms
        mds <- mapM (\h -> (,) h <$> Store.readMetadata store h)
                    (Set.toList (Set.unions (map Term.dependencies' (illtypedQmatches ++ qmatches))))
        pure $ SearchResults
          query
          mds
          (trim qmatches)
          (trim illtypedQmatches)
          (Metadata.queryPositions query)

    readTermRef (Reference.Derived h) = Store.readTerm store h
    readTermRef r = pure (Term.ref r)

    terms hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> readTermRef h) hs)

    transitiveDependencies = error "todo"

    transitiveDependents = error "todo"

    types hs =
      Map.fromList <$> sequence (map (\h -> (,) h <$> readTypeOf h) hs)

    typeAt ctx loc =
      Typechecker.typeAt readTypeOf loc ctx

    updateMetadata = Store.writeMetadata store
  in Codebase
       admissibleTypeAt
       createTerm
       createType
       dependencies
       dependents
       edit
       localInfo
       metadatas
       search
       terms
       transitiveDependencies
       transitiveDependents
       types
       typeAt
       updateMetadata

-- | Declare a group of bindings and add them to the codebase.
-- Bindings may be in any order and may refer to each other.
-- They are broken into strongly connected components before
-- being added, and any free variables are resolved using the
-- existing metadata store of the codebase.
declare :: (Monad m, Var v)
        => [(v, Term v)]
        -> Codebase m v Reference (Type v) (Term v) -> Noted m ()
declare bindings code = do
  unresolved <- declareCheckAmbiguous hooks0 bindings code
  case unresolved of
    [] -> pure ()
    _ -> fail (Text.unpack $ unresolvedNamesErrorMessage unresolved)

unresolvedNamesErrorMessage :: Var v => [(v, [Term v])] -> Text
unresolvedNamesErrorMessage vs =
  let unknown = [ v | (v, []) <- vs ]
      ambiguous = [ (v, (h:t)) | (v, (h:t)) <- vs ]
      render (v, tms) = Var.name v `mappend` " resolves to " `mappend` Text.pack (show tms)
  in (if null unknown then ""
      else "unresolved names:" `mappend` (Text.intercalate ", " $ Var.name <$> unknown) `mappend` "\n")
     `mappend`
     (if null ambiguous then ""
      else "ambiguous names:\n" `mappend` (Text.unlines $ map render ambiguous))

-- | Like `declare`, but takes a `String`
declare' :: (Monad m, Var v)
         => String
         -> Codebase m v Reference (Type v) (Term v) -> Noted m ()
declare' bindings code = do
  bs <- case Parser.run TermParser.moduleBindings bindings TypeParser.s0 of
    Parser.Fail err _ -> Noted (pure $ Left (Note err))
    Parser.Succeed bs _ _ -> pure bs
  declare bs code

data Hooks m v h =
  Hooks { nameShadowing :: [Term v] -> (v, Term v) -> m ()
        , renamedOldDefinition :: v -> v -> m ()
        , ambiguousReferences :: [(v, [Term v])] -> (v, Term v) -> m ()
        , finishedDeclaring :: (v, Term v) -> h -> m ()
        , handleShadowing :: HandleShadowing }

-- | Controls how situation is handled if name assigned to a new declaration
-- shadows an existing declaration
data HandleShadowing
  = FailIfShadowed -- fails with an error
  | RenameOldIfShadowed -- renames the old definition (appends first few hash characters)
  | AllowShadowed -- allows the shadowing (users of that name will have to disambiguate via hash)

hooks0 :: Applicative m => Hooks m v h
hooks0 =
  Hooks (\_ _ -> pure ())
        (\_ _ -> pure ())
        (\_ _ -> pure ())
        (\_ _ -> pure ())
        FailIfShadowed

-- | Like `declare`, but returns a list of symbols that cannot be resolved
-- unambiguously (occurs when multiple hashes have the same name)
declareCheckAmbiguous
  :: (Monad m, Var v)
  => Hooks m v Reference -> [(v, Term v)] -> Codebase m v Reference (Type v) (Term v)
  -> Noted m [(v, [Term v])]
declareCheckAmbiguous hooks bindings code = do
  termBuiltins <- allTermsByVarName Term.ref code -- probably worth caching this, updating it incrementally
  let names = multimap (termBuiltins ++ Parsers.termBuiltins)
      groups = Components.components bindings
      bindings' = groups >>= \c -> case c of
        [(v,b)] -> [(v,b)]
        _ -> [ (v, Term.letRec c b) | (v,b) <- c ]
      metadata v = Metadata.Metadata Metadata.Term (Metadata.Names [v]) Nothing
      tb0 = Parsers.termBuiltins
      mangle name (Reference.Derived h) =
        Var.rename (Var.qualifiedName name `mappend` "#" `mappend` Text.take 8 (Hash.base64 h)) name
      mangle name (Reference.Builtin h) =
        Var.rename (Var.qualifiedName name `mappend` "#" `mappend` "builtin") name
      declare (v,b) bindings names = do
        let free = Term.freeVars b
            lookups = map (\v -> (v, Map.findWithDefault [] v names)) (Set.toList free)
            md = metadata v
            ok = all uniquelyResolves lookups
            uniquelyResolves (_, [_]) = True
            uniquelyResolves _ = False
            resolved = [(v, b) | (v, [b]) <- lookups ]
        case ok of
          True -> do
            h <- createTerm code (Parsers.bindBuiltins (tb0 ++ resolved) Parsers.typeBuiltins b) md
            updateMetadata code h md
            Note.lift $ finishedDeclaring hooks (v,b) h
            go (Map.insert v [Term.ref h] names) bindings
          False -> do
            Note.lift $ ambiguousReferences hooks lookups (v, b)
            pure lookups
      go _ [] = pure []
      go names ((v, b) : bindings) = case Map.lookup v names of
        Nothing -> do
          Note.lift $ nameShadowing hooks [] (v, b)
          declare (v,b) bindings names
        Just collisions -> do
          Note.lift $ nameShadowing hooks collisions (v, b)
          case handleShadowing hooks of
            FailIfShadowed -> pure [(v, collisions)]
            AllowShadowed -> declare (v,b) bindings names
            RenameOldIfShadowed -> do
              forM_ [ r | Term.Ref' r <- collisions ] $ \h -> do
                let v' = mangle v h
                    md = metadata v'
                updateMetadata code h md
                Note.lift $ renamedOldDefinition hooks v v'
              declare (v,b) bindings (Map.delete v names)
  go names bindings'

-- | Like `declare`, but takes a `String`
declareCheckAmbiguous'
  :: (Monad m, Var v)
  => Hooks m v Reference -> String -> Codebase m v Reference (Type v) (Term v)
  -> Noted m [(v, [Term v])]
declareCheckAmbiguous' hooks bindings code = do
  bs <- case Parser.run TermParser.moduleBindings bindings TypeParser.s0 of
    Parser.Fail err _ -> Noted (pure $ Left (Note err))
    Parser.Succeed bs _ _ -> pure bs
  declareCheckAmbiguous hooks bs code

multimap :: Ord k => [(k,v)] -> Map k [v]
multimap = foldl' insert Map.empty where
  insert m (k,v) = Map.alter (\vs -> Just $ v : fromMaybe [] vs) k m

allTermsByVarName :: (Monad m, Var v) => (h -> Term v) -> Codebase m v h (Type v) (Term v) -> Noted m [(v, Term v)]
allTermsByVarName ref code = do
  -- grab all definitions in the code
  results <- search code Term.blank [] 1000000 (Metadata.Query "") Nothing
  pure [ (v, ref h) | (h, md) <- references results
                    , v <- Metadata.allNames (Metadata.names md) ]

allTerms :: (Monad m, Var v) => Codebase m v h (Type v) (Term v) -> Noted m [(h, Term v)]
allTerms code = do
  hs <- map fst . references <$> search code Term.blank [] 100000 (Metadata.Query "") Nothing
  Map.toList <$> terms code hs

interpreter :: Var v
            => [Builtin v] -> Codebase IO v Reference (Type v) (Term v)
            -> Term v -> Noted IO (Term v)
interpreter builtins codebase =
  let env = Map.fromList [(ref, op) | Builtin ref (Just op) _ _ <- builtins ]
      resolveHash h = snd . head . Map.toList <$> terms codebase [Reference.Derived h]
  in Interpreter.make env resolveHash

