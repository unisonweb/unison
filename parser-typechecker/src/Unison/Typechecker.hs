{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module is the primary interface to the Unison typechecker
-- module Unison.Typechecker (admissibleTypeAt, check, check', checkAdmissible', equals, locals, subtype, isSubtype, synthesize, synthesize', typeAt, wellTyped) where
module Unison.Typechecker where

import Control.Lens
import Control.Monad.Fail (fail)
import Control.Monad.State
  ( State,
    StateT,
    execState,
    get,
    modify,
  )
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Sequence.NonEmpty as NESeq (toSeq)
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import qualified Unison.Name as Name
import Unison.Prelude
import Unison.Referent (Referent)
import Unison.Result
  ( Result,
    ResultT,
    runResultT,
    pattern Result,
  )
import qualified Unison.Result as Result
import qualified Unison.Syntax.Name as Name (toText, unsafeFromText)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Typechecker.TypeVar as TypeVar
import Unison.Util.List (uniqueBy)
import Unison.Var (Var)
import qualified Unison.Var as Var

type Name = Text

data Notes v loc = Notes
  { bugs :: Seq (Context.CompilerBug v loc),
    errors :: Seq (Context.ErrorNote v loc),
    infos :: Seq (Context.InfoNote v loc)
  }
  deriving (Show)

instance Semigroup (Notes v loc) where
  Notes bs es is <> Notes bs' es' is' = Notes (bs <> bs') (es <> es') (is <> is')

instance Monoid (Notes v loc) where
  mempty = Notes mempty mempty mempty

convertResult :: Context.Result v loc a -> Result (Notes v loc) a
convertResult = \case
  Context.Success is a -> Result (Notes mempty mempty is) (Just a)
  Context.TypeError es is -> Result (Notes mempty (NESeq.toSeq es) is) Nothing
  Context.CompilerBug bug es is -> Result (Notes [bug] es is) Nothing

data NamedReference v loc = NamedReference
  { fqn :: Name,
    fqnType :: Type v loc,
    replacement :: Either v Referent
  }
  deriving (Show)

data Env v loc = Env
  { _ambientAbilities :: [Type v loc],
    _typeLookup :: TL.TypeLookup v loc,
    -- TDNR environment - maps short names like `+` to fully-qualified
    -- lists of named references whose full name matches the short name
    -- Example: `+` maps to [Nat.+, Float.+, Int.+]
    --
    -- This mapping is populated before typechecking with as few entries
    -- as are needed to help resolve variables needing TDNR in the file.
    _termsByShortname :: Map Name [NamedReference v loc]
  }

makeLenses ''Env

-- | Infer the type of a 'Unison.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize ::
  (Monad f, Var v, Ord loc, Monoid loc) =>
  Env v loc ->
  Term v loc ->
  ResultT (Notes v loc) f (Type v loc)
synthesize env t =
  let result =
        convertResult $
          Context.synthesizeClosed
            (TypeVar.liftType <$> view ambientAbilities env)
            (view typeLookup env)
            (TypeVar.liftTerm t)
   in Result.hoist (pure . runIdentity) $ fmap TypeVar.lowerType result

isSubtype :: Var v => Type v loc -> Type v loc -> Bool
isSubtype t1 t2 =
  handleCompilerBug (Context.isSubtype (tvar $ void t1) (tvar $ void t2))
  where
    tvar = TypeVar.liftType

handleCompilerBug :: Var v => Either (Context.CompilerBug v ()) a -> a
handleCompilerBug = \case
  Left bug -> error $ "compiler bug encountered: " ++ show bug
  Right b -> b

-- | Similar to 'isSubtype' but treats @t2@ as a scheme where the
-- outermost variables are existential rather than universal.
--
-- For example:
-- @
-- let
--   lhs = Unison.Type.ref () (Unison.Builtin.Decls.unitRef)
--   rhs = Unison.Type.forall () (Unison.Var.named "x") (Unison.Type.var () (Unison.Var.named "x"))
-- in fitsScheme @Symbol lhs rhs
-- @
-- is @True@ although the lhs is not a subtype of the rhs.
--
-- 'fitsScheme' is used to check that runnable types are a subtype of
-- @
-- exists x. '{IO, Exception} x
-- @
fitsScheme :: Var v => Type v loc -> Type v loc -> Bool
fitsScheme t1 t2 = handleCompilerBug (Context.fitsScheme (tvar $ void t1) (tvar $ void t2))
  where
    tvar = TypeVar.liftType

isEqual :: Var v => Type v loc -> Type v loc -> Bool
isEqual t1 t2 = isSubtype t1 t2 && isSubtype t2 t1

type TDNR f v loc a =
  StateT (Term v loc) (ResultT (Notes v loc) f) a

data Resolution v loc = Resolution
  { resolvedName :: Text,
    inferredType :: Context.Type v loc,
    resolvedLoc :: loc,
    suggestions :: [Context.Suggestion v loc]
  }

-- | Infer the type of a 'Unison.Term', using type-directed name resolution
-- to attempt to resolve unknown symbols.
synthesizeAndResolve ::
  (Monad f, Var v, Monoid loc, Ord loc) => Env v loc -> TDNR f v loc (Type v loc)
synthesizeAndResolve env = do
  tm <- get
  (tp, notes) <- listen . lift $ synthesize env tm
  typeDirectedNameResolution notes tp env

compilerBug :: Context.CompilerBug v loc -> Result (Notes v loc) ()
compilerBug bug = do
  tell $ Notes [bug] mempty mempty
  Control.Monad.Fail.fail ""

typeError :: Context.ErrorNote v loc -> Result (Notes v loc) ()
typeError note = do
  tell $ Notes mempty [note] mempty
  Control.Monad.Fail.fail ""

btw :: Monad f => Context.InfoNote v loc -> ResultT (Notes v loc) f ()
btw note = tell $ Notes mempty mempty [note]

liftResult :: Monad f => Result (Notes v loc) a -> TDNR f v loc a
liftResult = lift . MaybeT . WriterT . pure . runIdentity . runResultT

-- Resolve "solved blanks". If a solved blank's type and name matches the type
-- and unqualified name of a symbol that isn't imported, provide a note
-- suggesting the import. If the blank is ambiguous and only one typechecks, use
-- that one.  Otherwise, provide an unknown symbol error to the user.
-- The cases we consider are:
-- 1. There exist names that match and their types match too. Tell the user
--    the fully qualified names of these terms, and their types.
-- 2. There's more than one name that matches,
--    but only one that typechecks. Substitute that one into the code.
-- 3. No match at all. Throw an unresolved symbol at the user.
typeDirectedNameResolution ::
  forall v loc f.
  (Monad f, Var v, Ord loc, Monoid loc) =>
  Notes v loc ->
  Type v loc ->
  Env v loc ->
  TDNR f v loc (Type v loc)
typeDirectedNameResolution oldNotes oldType env = do
  -- Add typed components (local definitions) to the TDNR environment.
  let tdnrEnv = execState (traverse_ addTypedComponent $ infos oldNotes) env
  -- Resolve blanks in the notes and generate some resolutions
  resolutions <-
    liftResult . traverse (resolveNote tdnrEnv) . toList $
      infos
        oldNotes
  case catMaybes resolutions of
    [] -> pure oldType
    rs ->
      let goAgain =
            any ((== 1) . length . dedupe . filter Context.isExact . suggestions) rs
       in if goAgain
            then do
              traverse_ substSuggestion rs
              synthesizeAndResolve tdnrEnv
            else do
              -- The type hasn't changed
              liftResult $ suggest rs
              pure oldType
  where
    addTypedComponent :: Context.InfoNote v loc -> State (Env v loc) ()
    addTypedComponent (Context.TopLevelComponent vtts) =
      for_ vtts $ \(v, typ, _) ->
        for_ (Name.suffixes . Name.unsafeFromText . Var.name $ Var.reset v) $ \suffix ->
          termsByShortname
            %= Map.insertWith
              (<>)
              (Name.toText suffix)
              [NamedReference (Var.name v) typ (Left v)]
    addTypedComponent _ = pure ()

    suggest :: [Resolution v loc] -> Result (Notes v loc) ()
    suggest =
      traverse_
        ( \(Resolution name inferredType loc suggestions) ->
            typeError $
              Context.ErrorNote
                (Context.UnknownTerm loc (Var.named name) (dedupe suggestions) inferredType)
                []
        )
    guard x a = if x then Just a else Nothing

    substSuggestion :: Resolution v loc -> TDNR f v loc ()
    substSuggestion
      ( Resolution
          name
          _
          loc
          ( filter Context.isExact ->
              [Context.Suggestion _ _ replacement Context.Exact]
            )
        ) =
        do
          modify (substBlank (Text.unpack name) loc solved)
          lift . btw $ Context.Decision (Var.named name) loc solved
        where
          solved = either (Term.var loc) (Term.fromReferent loc) replacement
    substSuggestion _ = pure ()

    -- Resolve a `Blank` to a term
    substBlank :: String -> loc -> Term v loc -> Term v loc -> Term v loc
    substBlank s a r = ABT.visitPure go
      where
        go t = guard (ABT.annotation t == a) $ ABT.visitPure resolve t
        resolve (Term.Blank' (B.Recorded (B.Resolve loc name)))
          | name == s =
              Just (const loc <$> r)
        resolve _ = Nothing

    --  Returns Nothing for irrelevant notes
    resolveNote ::
      Env v loc ->
      Context.InfoNote v loc ->
      Result (Notes v loc) (Maybe (Resolution v loc))
    resolveNote env (Context.SolvedBlank (B.Resolve loc n) _ it) =
      fmap (Just . Resolution (Text.pack n) it loc . dedupe . join)
        . traverse (resolve it)
        . join
        . maybeToList
        . Map.lookup (Text.pack n)
        $ view termsByShortname env
    resolveNote _ n = btw n >> pure Nothing
    dedupe :: [Context.Suggestion v loc] -> [Context.Suggestion v loc]
    dedupe = uniqueBy Context.suggestionReplacement
    resolve ::
      Context.Type v loc ->
      NamedReference v loc ->
      Result (Notes v loc) [Context.Suggestion v loc]
    resolve inferredType (NamedReference fqn foundType replace) =
      -- We found a name that matches. See if the type matches too.
      case Context.isSubtype (TypeVar.liftType foundType) (Context.relax inferredType) of
        Left bug -> const [] <$> compilerBug bug
        -- Suggest the import if the type matches.
        Right b ->
          pure
            [ Context.Suggestion
                fqn
                (TypeVar.liftType foundType)
                replace
                (if b then Context.Exact else Context.WrongType)
            ]

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term. Returns @typ@ if successful,
-- and a note about typechecking failure otherwise.
check ::
  (Monad f, Var v, Ord loc, Monoid loc) =>
  Env v loc ->
  Term v loc ->
  Type v loc ->
  ResultT (Notes v loc) f (Type v loc)
check env term typ = synthesize env (Term.ann (ABT.annotation term) term typ)

-- | `checkAdmissible' e t` tests that `(f : t -> r) e` is well-typed.
-- If `t` has quantifiers, these are moved outside, so if `t : forall a . a`,
-- this will check that `(f : forall a . a -> a) e` is well typed.
-- checkAdmissible' :: Var v => Term v -> Type v -> Either Note (Type v)
-- checkAdmissible' term typ =
--   synthesize' (Term.blank() `Term.ann_` tweak typ `Term.app_` term)
--   where
--     tweak (Type.ForallNamed' v body) = Type.forall() v (tweak body)
--     tweak t = Type.arrow() t t
-- | Returns `True` if the expression is well-typed, `False` otherwise
wellTyped :: (Monad f, Var v, Ord loc, Monoid loc) => Env v loc -> Term v loc -> f Bool
wellTyped env term = go <$> runResultT (synthesize env term)
  where
    go (may, _) = isJust may

-- | @subtype a b@ is @Right b@ iff @f x@ is well-typed given
-- @x : a@ and @f : b -> t@. That is, if a value of type `a`
-- can be passed to a function expecting a `b`, then `subtype a b`
-- returns `Right b`. This function returns @Left note@ with information
-- about the reason for subtyping failure otherwise.
--
-- Example: @subtype (forall a. a -> a) (Int -> Int)@ returns @Right (Int -> Int)@.
-- subtype :: Var v => Type v -> Type v -> Either Note (Type v)
-- subtype t1 t2 = error "todo"
-- let (t1', t2') = (ABT.vmap TypeVar.Universal t1, ABT.vmap TypeVar.Universal t2)
-- in case Context.runM (Context.subtype t1' t2')
--                      (Context.MEnv Context.env0 [] Map.empty True) of
--   Left e -> Left e
--   Right _ -> Right t2

-- | Returns true if @subtype t1 t2@ returns @Right@, false otherwise
-- isSubtype :: Var v => Type v -> Type v -> Bool
-- isSubtype t1 t2 = case subtype t1 t2 of
--   Left _ -> False
--   Right _ -> True

-- | Returns true if the two type are equal, up to alpha equivalence and
-- order of quantifier introduction. Note that alpha equivalence considers:
-- `forall b a . a -> b -> a` and
-- `forall a b . a -> b -> a` to be different types
-- equals :: Var v => Type v -> Type v -> Bool
-- equals t1 t2 = isSubtype t1 t2 && isSubtype t2 t1
