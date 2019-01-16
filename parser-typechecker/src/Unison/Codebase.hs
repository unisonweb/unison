{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.Codebase where

import           Control.Monad                  ( foldM
                                                , forM
                                                )
import           Control.Monad.State
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( toList
                                                , traverse_
                                                )
import           Data.Function                  ( on )
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String                    ( fromString )
import qualified Data.Text                     as Text
import           Data.Traversable               ( for )
import           Text.EditDistance              ( defaultEditCosts
                                                , levenshteinDistance
                                                )
import qualified Unison.ABT                    as ABT
import qualified Unison.Builtin                as Builtin
import           Unison.Codebase.Branch         ( Branch, Branch0 )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.DataDeclaration        as DD
import           Unison.Names                   ( Name )
import           Unison.Parser                  ( Ann )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent(..) )
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import qualified Unison.TermPrinter            as TermPrinter
import qualified Unison.Type                   as Type
import qualified Unison.TypePrinter            as TypePrinter
import           Unison.Typechecker.TypeLookup  ( Decl
                                                , TypeLookup(TypeLookup)
                                                )
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile             as UF
import           Unison.Util.AnnotatedText      ( AnnotatedText )
import           Unison.Util.ColorText          ( Color, ColorText )
import           Unison.Util.Pretty             ( Pretty )
import qualified Unison.Util.Pretty            as PP
import qualified Unison.Util.Relation          as R
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )

type DataDeclaration v a = DD.DataDeclaration' v a
type EffectDeclaration v a = DD.EffectDeclaration' v a
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

data Codebase m v a =
  Codebase { getTerm            :: Reference.Id -> m (Maybe (Term v a))
           , getTypeOfTerm      :: Reference -> m (Maybe (Type v a))
           , putTerm            :: Reference.Id -> Term v a -> Type v a -> m ()

           , getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))
           , putTypeDeclarationImpl :: Reference.Id -> Decl v a -> m ()
           , branches           :: m [Name]
           , getBranch          :: Name -> m (Maybe Branch)
           -- thought: this merges the given branch with the existing branch
           -- or creates a new branch if there's no branch with that name
           , mergeBranch        :: Name -> Branch -> m Branch
           , branchUpdates      :: m (m (), m (Set Name))

           , dependents :: Reference.Id -> m (Set Reference.Id)
           }

getTypeOfConstructor ::
  (Monad m, Ord v) => Codebase m v a -> Reference -> Int -> m (Maybe (Type v a))
getTypeOfConstructor codebase (Reference.DerivedId r) cid = do
  maybeDecl <- getTypeDeclaration codebase r
  pure $ case maybeDecl of
    Nothing -> Nothing
    Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
getTypeOfConstructor _ r cid =
  error $ "Don't know how to getTypeOfConstructor " ++ show r ++ " " ++ show cid


-- Scan the term for all its dependencies and pull out the `ReadRefs` that
-- gives info for all its dependencies, using the provided codebase.
typecheckingEnvironment
  :: (Monad m, Ord v) => Codebase m v a -> Term v a -> m (TypeLookup v a)
typecheckingEnvironment code t = do
  let deps = Term.dependencies t
  termTypes0 <- forM (toList deps) $ \r -> (r, ) <$> getTypeOfTerm code r
  let termTypes = Map.fromList [ (r, t) | (r, Just t) <- termTypes0 ]
  let rids      = [ (r0, r) | r0@(Reference.DerivedId r) <- toList deps ]
  decls0 <- forM rids $ \(r0, r) -> (r0, ) <$> getTypeDeclaration code r
  let allDecls         = Map.fromList [ (r, d) | (r, Just d) <- decls0 ]
      (datas, effects) = foldl' go (mempty, mempty) (Map.toList allDecls)
      go (datas, effects) (r, d) = case d of
        Left  e -> (datas, Map.insert r e effects)
        Right d -> (Map.insert r d datas, effects)
  pure $ TL.TypeLookup termTypes datas effects

listReferences
  :: (Var v, Monad m) => Codebase m v a -> Branch0 -> [Reference] -> m String
listReferences code branch refs = do
  let ppe = Branch.prettyPrintEnv1 branch
  terms0 <- forM refs $ \r -> do
    otyp <- getTypeOfTerm code r
    pure $ (PPE.termName ppe (Referent.Ref r), otyp)
  let terms = [ (name, t) | (name, Just t) <- terms0 ]
  let typeRefs0 = Branch.allNamedTypes branch
      typeRefs  = filter (`Set.member` typeRefs0) refs
  _decls <- fmap catMaybes . forM typeRefs $ \r -> case r of
    Reference.DerivedId id -> do
      d <- getTypeDeclaration code id
      pure $ fmap (PPE.typeName ppe r, ) d
    _ -> pure Nothing
  let termsPP = TypePrinter.prettySignatures ppe (sortOn fst terms)
  -- todo: type decls also
  pure (PP.render 80 termsPP)

fuzzyFindTerms' :: Branch -> [String] -> [(Name, Referent)]
fuzzyFindTerms' branch query =
  let
    termNames =
      Text.unpack <$> toList (Branch.allTermNames $ Branch.head branch)
    matchingTerms :: [String]
    matchingTerms = if null query
      then termNames
      else query >>= \q -> sortedApproximateMatches q termNames
    refsForName :: String -> [Referent]
    refsForName name =
      Set.toList $ Branch.termsNamed (Text.pack name) (Branch.head branch)
  in
    matchingTerms
      >>= \name -> (Text.pack name, ) <$> refsForName name

fuzzyFindTermTypes
  :: forall m v a
  .  (Var v, Monad m)
  => Codebase m v a
  -> Branch
  -> [String]
  -> m [(Name, Referent, Maybe (Type v a))]
fuzzyFindTermTypes codebase branch query =
  let found = fuzzyFindTerms' branch query
      tripleForRef name ref = (name, ref, ) <$> case ref of
        Referent.Ref r -> getTypeOfTerm codebase r
        Referent.Req r cid -> getTypeOfConstructor codebase r cid
        Referent.Con r cid -> getTypeOfConstructor codebase r cid
  in  traverse (uncurry tripleForRef) found

fuzzyFindTypes' :: Branch -> [String] -> [(Name, Reference)]
fuzzyFindTypes' (Branch.head -> branch) query =
  let typeNames =
        Text.unpack <$> toList (Branch.allTypeNames branch)
      matchingTypes = if null query
        then typeNames
        else query >>= \q -> sortedApproximateMatches q typeNames
  in  matchingTypes >>= \name ->
        (Text.pack name, )
          <$> (Set.toList $ Branch.typesNamed (Text.pack name) branch)

prettyTypeSource :: (Monad m, Var v) => Codebase m v a -> Name -> Reference -> Branch -> m (Maybe (Pretty ColorText))
prettyTypeSource = error "todo"


listReferencesMatching
  :: (Var v, Monad m) => Codebase m v a -> Branch -> [String] -> m String
listReferencesMatching code (Branch.head -> b) query = do
  let
    termNames     = Text.unpack <$> toList (Branch.allTermNames b)
    typeNames     = Text.unpack <$> toList (Branch.allTypeNames b)
    matchingTerms = if null query
      then termNames
      else query >>= \q -> sortedApproximateMatches q termNames
    matchingTypes = if null query
      then typeNames
      else query >>= \q -> sortedApproximateMatches q typeNames
    matchingTypeRefs = matchingTypes
      >>= \name -> Set.toList (Branch.typesNamed (Text.pack name) b)
    matchingTermRefs = matchingTerms
      >>= \name -> Set.toList (Branch.termsNamed (Text.pack name) b)
  listReferences code
                 b
                 (matchingTypeRefs ++ [ r | Ref r <- matchingTermRefs ])

data Err = InvalidBranchFile FilePath String deriving Show

putTypeDeclaration
  :: (Monad m, Ord v) => Codebase m v a -> Reference.Id -> Decl v a -> m ()
putTypeDeclaration c rid decl = do
  putTypeDeclarationImpl c rid decl
  traverse_ go $ case decl of
    Left  ed -> DD.effectConstructorTerms rid ed
    Right dd -> DD.dataConstructorTerms rid dd
  where go (r, tm, typ) = putTerm c r tm typ

-- | Put all the builtins into the codebase
initialize :: (Var.Var v, Monad m) => Codebase m v Ann -> m ()
initialize c = do
  traverse_ goData   Builtin.builtinDataDecls
  traverse_ goEffect Builtin.builtinEffectDecls
 where
  go f (_, (ref, decl)) = case ref of
    Reference.DerivedId id -> putTypeDeclaration c id (f decl)
    _                      -> pure ()
  goEffect = go Left
  goData   = go Right

prettyBinding
  :: (Var.Var v, Monad m)
  => Codebase m v a
  -> Name
  -> Referent
  -> Branch0
  -> m (Maybe (Pretty String))
prettyBinding _ _ (Referent.Ref (Reference.Builtin _)) _ = pure Nothing
prettyBinding cb name r0@(Referent.Ref r1@(Reference.DerivedId r)) b =
  go =<< getTerm cb r
 where
  go Nothing = pure Nothing
  go (Just tm) =
    let
-- We boost the `(r0,name)` association since if this is a recursive
-- fn whose body also mentions `r`, want name to be the same as the binding.
        ppEnv = Branch.prettyPrintEnv [b]
          `mappend` PPE.scale 10 (PPE.fromTermNames [(r0, name)])
    in  case tm of
          Term.Ann' _ _ ->
            pure $ Just (TermPrinter.prettyBinding ppEnv (Var.named name) tm)
          _ -> do
            Just typ <- getTypeOfTerm cb r1
            pure . Just $ TermPrinter.prettyBinding
              ppEnv
              (Var.named name)
              (Term.ann (ABT.annotation tm) tm typ)
prettyBinding _ _ r _ = error $ "unpossible " ++ show r

prettyBindings :: (Var.Var v, Monad m)
  => Codebase m v a -> [(Name,Referent)] -> Branch0 -> m (Pretty String)
prettyBindings cb tms b = do
  ds <- catMaybes <$> (forM tms $ \(name,r) -> prettyBinding cb name r b)
  pure $ PP.linesSpaced ds

-- Search for and display bindings matching the given query
prettyBindingsQ
  :: (Var.Var v, Monad m)
  => Codebase m v a
  -> String
  -> Branch0
  -> m (Pretty String)
prettyBindingsQ cb query b =
  let possible = Branch.allTermNames b
      matches =
        sortedApproximateMatches query (Text.unpack <$> toList possible)
      str = fromString
      bs =
        [ (name, r)
        | name <- Text.pack <$> matches
        , r    <- take 1 (toList $ Branch.termsNamed name b)
        ]
      go pp = if length matches > 5
        then PP.linesSpaced
          [ pp
          , "... "
          <> str (show (length matches - 5))
          <> " more (use `> list "
          <> str query
          <> "` to see all matches)"
          ]
        else pp
  in  go <$> prettyBindings cb (take 5 bs) b

prettyListingQ
  :: (Var.Var v, Monad m)
  => Codebase m v a
  -> String
  -> Branch
  -> m (AnnotatedText Color)
prettyListingQ _cb _query _b =
  error
    $  "todo - find all matches, display similar output to "
    <> "PrintError.prettyTypecheckedFile"

typeLookupForDependencies
  :: Monad m => Codebase m v a -> Set Reference -> m (TL.TypeLookup v a)
typeLookupForDependencies codebase refs = foldM go mempty refs
 where
  go tl ref@(Reference.DerivedId id) = fmap (tl <>) $ do
    getTypeOfTerm codebase ref >>= \case
      Just typ -> pure $ TypeLookup (Map.singleton ref typ) mempty mempty
      Nothing  -> getTypeDeclaration codebase id >>= \case
        Just (Left ed) ->
          pure $ TypeLookup mempty mempty (Map.singleton ref ed)
        Just (Right dd) ->
          pure $ TypeLookup mempty (Map.singleton ref dd) mempty
        Nothing -> pure mempty
  go tl _builtin = pure tl -- codebase isn't consulted for builtins

transitiveDependencies
  :: (Monad m, Var v)
  => Codebase m v a
  -> Set Reference
  -> Reference
  -> m (Set Reference)
transitiveDependencies code seen0 r = if Set.member r seen0
  then pure seen0
  else
    let seen = Set.insert r seen0
    in
      case r of
        Reference.DerivedId id -> do
          t <- getTerm code id
          case t of
            Just t ->
              foldM (transitiveDependencies code) seen (Term.dependencies t)
            Nothing -> do
              t <- getTypeDeclaration code id
              case t of
                Nothing        -> pure seen
                Just (Left ed) -> foldM (transitiveDependencies code)
                                        seen
                                        (DD.dependencies (DD.toDataDecl ed))
                Just (Right dd) -> foldM (transitiveDependencies code)
                                         seen
                                         (DD.dependencies dd)
        _ -> pure seen

-- Creates a self-contained `UnisonFile` which bakes in
-- all transitive dependencies
makeSelfContained
  :: (Monad m, Var v)
  => Codebase m v a
  -> Branch0
  -> UF.UnisonFile v a
  -> m (UF.UnisonFile v a)
makeSelfContained code b (UF.UnisonFile datas0 effects0 tm) = do
  deps <- foldM (transitiveDependencies code) Set.empty (Term.dependencies tm)
  let pp = Branch.prettyPrintEnv1 b
      termName r = PPE.termName pp (Referent.Ref r)
      typeName r = PPE.typeName pp r
  decls <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) -> fmap (r, ) <$> getTypeDeclaration code rid
    _                           -> pure Nothing
  termsByRef <- fmap catMaybes . forM (toList deps) $ \case
    r@(Reference.DerivedId rid) ->
      fmap (r, Var.named (termName r), ) <$> getTerm code rid
    _ -> pure Nothing
  let
    unref t = ABT.visitPure go t
     where
      go t@(Term.Ref' (r@(Reference.DerivedId _))) =
        Just (Term.var (ABT.annotation t) (Var.named $ termName r))
      go _ = Nothing
    datas = Map.fromList
      [ (v, (r, dd)) | (r, Right dd) <- decls, v <- [Var.named (typeName r)] ]
    effects = Map.fromList
      [ (v, (r, ed)) | (r, Left ed) <- decls, v <- [Var.named (typeName r)] ]
    bindings = [ ((ABT.annotation t, v), unref t) | (_, v, t) <- termsByRef ]
    unrefBindings bs = [ (av, unref t) | (av, t) <- bs ]
    tm' = case tm of
      Term.LetRecNamedAnnotatedTop' top ann bs e ->
        Term.letRec top ann (bindings ++ unrefBindings bs) (unref e)
      tm -> Term.letRec True (ABT.annotation tm) bindings (unref tm)
  pure $ UF.UnisonFile (datas0 <> datas) (effects0 <> effects) tm'

sortedApproximateMatches :: String -> [String] -> [String]
sortedApproximateMatches q possible = trim (sortOn fst matches)
 where
  nq = length q
  score s | s == q                         = 0 :: Int
          | -- exact match is top choice
            map toLower q == map toLower s = 1
          |        -- ignore case
            q `isSuffixOf` s               = 2
          |        -- matching suffix is pretty good
            q `isInfixOf` s                = 3
          |        -- a match somewhere
            q `isPrefixOf` s               = 4
          |        -- ...
            map toLower q `isInfixOf` map toLower s = 5
          | q `isSubsequenceOf` s          = 6
          | otherwise = 7 + editDistance (map toLower q) (map toLower s)
  editDistance q s = levenshteinDistance defaultEditCosts q s
  matches = map (\s -> (score s, s)) possible
  trim ((_, h) : _) | h == q = [h]
  trim ms = map snd $ takeWhile (\(n, _) -> n - 7 < nq `div` 4) ms

branchExists :: Functor m => Codebase m v a -> Name -> m Bool
branchExists codebase name = elem name <$> branches codebase

builtinBranch :: Branch
builtinBranch = Branch.append (Branch.fromNames Builtin.names) mempty

-- Predicate of Relation a b here is "a depends on b".
-- Dependents are in the domain and dependencies in the range.
type DependencyGraph = R.Relation Reference Reference

dependencyGraph
  :: (Ord v, Monad m) => Codebase m v a -> Branch0 -> m DependencyGraph
dependencyGraph c b = do
  termDeps <-
    for [ r | Reference.DerivedId r <- Referent.toReference <$> toList terms ]
      $ \r -> do
          mayTerm <- getTerm c r
          case mayTerm of
            Nothing -> fail $ "Missing term reference " <> show r
            Just t  -> pure (Reference.DerivedId r, Term.dependencies t)
  typeDeps <- for [ r | Reference.DerivedId r <- toList types ] $ \r -> do
    mayType <- getTypeDeclaration c r
    case mayType of
      Nothing -> fail $ "Missing type reference " <> show r
      Just t  -> pure
        (Reference.DerivedId r, DD.dependencies . either DD.toDataDecl id $ t)
  pure $ on R.union (R.fromMultimap . Map.fromList) termDeps typeDeps
 where
  terms = Branch.allTerms b
  types = Branch.allTypes b

isTerm :: Functor m => Codebase m v a -> Reference.Id -> m Bool
isTerm = ((isJust <$>) .) . getTerm

isType :: Functor m => Codebase m v a -> Reference.Id -> m Bool
isType = ((isJust <$>) .) . getTerm

-- referenceOps :: Codebase m v a -> Branch.ReferenceOps m
-- referenceOps c = Branch.ReferenceOps isTerm' isType' dependencies dependents
--  where
--   isTerm' (Reference.DerivedId r) = isTerm c r
--   isTerm' _                       = pure False
--   isType' (Reference.DerivedId r) = isType c r
--   isType' _                       = pure False
--   dependencies = (<$> get) . R.lookupDom
--   dependents   = (<$> get) . R.lookupRan


