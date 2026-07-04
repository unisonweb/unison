module Unison.Result where

import Control.Error.Util (note)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Fail qualified as Fail
import Control.Monad.Morph qualified as Morph
import Control.Monad.Writer (MonadWriter (..), WriterT (..), runWriterT)
import Unison.Name (Name)
import Unison.Names.ResolutionResult qualified as Names
import Unison.Prelude
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenResolver qualified as GR

type Result notes = ResultT notes Identity

type ResultT notes f = MaybeT (WriterT notes f)

data Note v loc
  = Parsing (Parser.Err v)
  | NameResolutionFailures [Names.ResolutionFailure loc]
  | UnknownSymbol v loc
  | TypeError (Context.ErrorNote v loc)
  | TypeInfo (Context.InfoNote v loc)
  | CompilerBug (CompilerBug v loc)
  | -- | An implicit-resolution failure. Surfaced
    -- by 'Unison.FileParsers.synthesizeFile' after running the
    -- 'Unison.Typechecker.GivenElaborator' over the typechecker's
    -- 'Context.ConstraintGoal' info notes. The 'GR.ResolveError'
    -- payload is the structured failure from
    -- 'Unison.Typechecker.GivenResolver': 'NoGiven', 'Ambiguous',
    -- 'DepthExceeded', 'Cycle', or 'UnresolvedMetavarInGoal'.
    --
    -- The first 'loc' is the apply-site source location (copied from
    -- the originating 'ConstraintGoal'); the 'Type' is the goal as
    -- the resolver saw it (post-substitution from inference); the
    -- 'GR.ResolveError' carries the structured reason.
    UnresolvedImplicit loc (Type v loc) (GR.ResolveError v loc)
  deriving (Show)

data CompilerBug v loc
  = TopLevelComponentNotFound v (Term v loc)
  | ResolvedNameNotFound v loc Name
  | TypecheckerBug (Context.CompilerBug v loc)
  deriving (Show)

result :: Result notes a -> Maybe a
result (Result _ may) = may

pattern Result :: w -> Maybe a -> MaybeT (WriterT w Identity) a
pattern Result notes may = MaybeT (WriterT (Identity (may, notes)))

{-# COMPLETE Result #-}

pattern TypeWarning w = TypeInfo (Context.Warning w)

makeResult :: (Applicative m) => notes -> Maybe a -> ResultT notes m a
makeResult notes value =
  MaybeT (WriterT (pure (value, notes)))

isSuccess :: (Functor f) => ResultT note f a -> f Bool
isSuccess = (isJust . fst <$>) . runResultT

isFailure :: (Functor f) => ResultT note f a -> f Bool
isFailure = (isNothing . fst <$>) . runResultT

toMaybe :: (Functor f) => ResultT note f a -> f (Maybe a)
toMaybe = (fst <$>) . runResultT

runResultT :: ResultT notes f a -> f (Maybe a, notes)
runResultT = runWriterT . runMaybeT

-- Returns the `Result` in the `f` functor.
getResult :: (Functor f) => ResultT notes f a -> f (Result notes a)
getResult r = uncurry (flip Result) <$> runResultT r

toEither :: (Functor f) => ResultT notes f a -> ExceptT notes f a
toEither r = ExceptT (go <$> runResultT r)
  where
    go (may, notes) = note notes may

tell1 :: (Monad f) => note -> ResultT (Seq note) f ()
tell1 = tell . pure

-- | Like 'tell1' but accepts a sequence of notes.
tellNotes :: (Monad f) => Seq note -> ResultT (Seq note) f ()
tellNotes = tell

fromParsing :: (Monad f) => Either (Parser.Err v) a -> ResultT (Seq (Note v loc)) f a
fromParsing (Left e) = do
  tell1 $ Parsing e
  Fail.fail ""
fromParsing (Right a) = pure a

tellAndFail :: (Monad f) => note -> ResultT (Seq note) f a
tellAndFail note = tell1 note *> Fail.fail "Elegantly and responsibly"

compilerBug :: (Monad f) => CompilerBug v loc -> ResultT (Seq (Note v loc)) f a
compilerBug = tellAndFail . CompilerBug

hoist ::
  (Monad f, Monoid notes) =>
  (forall a. f a -> g a) ->
  ResultT notes f b ->
  ResultT notes g b
hoist morph = Morph.hoist (Morph.hoist morph)
