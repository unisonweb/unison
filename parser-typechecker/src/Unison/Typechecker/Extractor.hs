{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Unison.Typechecker.Extractor where

-- import           Control.Applicative
-- import           Control.Monad
-- import           Data.Foldable              (toList)
-- import qualified Unison.Blank               as B
-- import           Unison.Reference           (Reference)
-- import qualified Unison.Term                as Term
import qualified Unison.Typechecker.Context as C
-- import           Unison.Var                 (Var)

newtype NoteExtractor v loc a =
  NoteExtractor { run :: C.Note v loc -> Maybe a }

newtype ElementExtractor v loc a =
  ElementExtractor { runElement :: C.PathElement v loc -> Maybe a }

data Ranged a = Ranged { get :: a, start :: Int, end :: Int }
newtype SubseqExtractor v loc a =
  SubseqExtractor { runSubseq :: [C.PathElement v loc] -> [Ranged a] }

adjacent :: SubseqExtractor v loc a -> SubseqExtractor v loc b -> SubseqExtractor v loc (a,b)
adjacent xa xb = SubseqExtractor $ \elts ->
  let as = runSubseq xa elts
      bs = runSubseq xb elts
  in [ Ranged (a, b) startA endB | Ranged a startA endA <- as
                                 , Ranged b startB endB <- bs
                                 , endA + 1 == startB
                                 ]

-- | returns occurrences of xa not preceded by occurrences of xb
notPrecededBy :: SubseqExtractor v loc a -> SubseqExtractor v loc b -> SubseqExtractor v loc a
notPrecededBy xa xb = SubseqExtractor $ \elts ->
  let as = runSubseq xa elts
      bs = runSubseq xb elts
  in [ ra | ra@(Ranged _ startA _) <- as
          , all (\(Ranged _ startB _) -> startB >= startA) bs ]

unique :: SubseqExtractor v loc a -> NoteExtractor v loc a
unique = error "todo"

nearest :: SubseqExtractor v loc a -> NoteExtractor v loc a
nearest = error "todo"

-- instance Alternative (SubseqExtractor v loc) where

-- adjacent3 :: SubseqExtractor v loc a
--           -> SubseqExtractor v loc b
--           -> Subseq

note :: NoteExtractor v loc (C.Note v loc)
note = NoteExtractor $ Just . id

-- path :: NoteExtractor v loc [C.PathElement v loc]
-- path = NoteExtractor $ pure . toList . C.path

-- innermostTerm :: NoteExtractor v loc (C.Term v loc)
-- innermostTerm = NoteExtractor $ \n -> case C.innermostErrorTerm n of
--   Just e -> pure e
--   Nothing -> mzero
--
-- type PathPredicate v loc = C.PathElement v loc -> Bool
--
-- -- toPathPredicate :: PathExtractor v loc a -> PathPredicate v loc
-- -- toPathPredicate ex p = case runPath ex p of Nothing -> False; _ -> True
--
-- exactly1AppBefore :: PathExtractor v loc a -> NoteExtractor v loc a
-- exactly1AppBefore p = snd <$> limitOccurrencesBefore 1 1 inSynthesizeApp p
--
-- occurrencesBefore :: PathExtractor v loc a
--                   -> PathExtractor v loc b
--                   -> NoteExtractor v loc ([a],b)
-- occurrencesBefore p q = do
--   (prefix, b) <- elementsUntil q
--   pure ([ a | Just a <- runPath p <$> prefix ], b)
--
-- limitOccurrencesBefore :: Int
--                        -> Int
--                        -> PathExtractor v loc a
--                        -> PathExtractor v loc b
--                        -> NoteExtractor v loc ([a],b)
-- limitOccurrencesBefore min max p q = do
--   result@(as, _) <- occurrencesBefore p q
--   if length as < min || length as > max then mzero else pure result
--
-- -- User is applying args to not-a-function
-- applyingNonFunction :: NoteExtractor v loc (C.Term v loc, C.Type v loc)
-- applyingNonFunction = do
--   (prefix, e) <- elementsUntil inSynthesize
--   case elementsUntil' inSynthesizeApp prefix of
--     Just ([], (ft, _argTerm, _argNum)) ->
--       case e of
--         Term.Apps' f _args -> pure (f, ft)
--         _                  -> mzero
--     _ -> mzero
--
-- -- returns the prefix of a path list that occurs
-- elementsUntil' :: PathExtractor v loc a
--                -> [C.PathElement v loc]
--                -> Maybe ([C.PathElement v loc], a)
-- elementsUntil' p l = go [] l where
--   go _ [] = Nothing
--   go acc (h:t) = case runPath p h of
--     Just a  -> Just (reverse acc, a)
--     Nothing -> go (h:acc) t
--
-- elementsUntil :: PathExtractor v loc a
--               -> NoteExtractor v loc ([C.PathElement v loc], a)
-- elementsUntil p = NoteExtractor $ elementsUntil' p . toList . C.path
--
-- -- The `n`th argument to `f` is `foundType`, but I was expecting `expectedType`.
-- --
-- --    30 |   asdf asdf asdf
-- --
-- -- If you're curious
-- -- `f` has type `blah`, where
-- --    `a` was chosen as `A`
-- --    `b` was chosen as `B`
-- --    `c` was chosen as `C`
-- -- (many colors / groups)
--
-- inApp :: forall v loc. (Show loc, Var v)
--       => NoteExtractor v loc (C.Term v loc, -- f
--                               Int, -- n
--                               C.Type v loc, -- expectedType
--                               C.Type v loc, -- foundType
--                               C.Term v loc, -- arg
--                               Maybe (C.Type v loc, [(v, C.Type v loc)]))
-- inApp = do
--   _ctx <- typeMismatch
--   ((foundType, expectedType),
--    (arg, _expectedType'),
--    (_solvedFnType, _arg', argNum),
--    (f, _ft, _args)) <- adjacent4 inSubtype inCheck inSynthesizeApp inSynthesizeApps
--   let polymorphicTypeInfo :: Maybe (C.Type v loc, [(v, C.Type v loc)])
--       polymorphicTypeInfo = Nothing
--       -- polymorphicTypeInfo = case f of
--       --   Term.Var' v -> do
--       --     rawType <- C.lookupAnn ctx v
--       --     let go :: C.TypeVar v loc -> Maybe (v, C.Type v loc)
--       --         go v0 = let v = TypeVar.underlying v0 in
--       --                 (v,) <$> C.lookupAnn ctx v
--       --         typeVars :: [C.TypeVar v loc]
--       --         typeVars = (toList . ABT.freeVars $ rawType)
--       --         solvedVars = catMaybes (go <$> typeVars)
--       --     pure (rawType, solvedVars)
--       --
--       --   -- Term.Ref' r -> lookup the type
--       --   -- Term.Builtin' r -> lookup the type
--       --
--       --   _ -> Nothing
--   pure (f, argNum, expectedType, foundType, arg, polymorphicTypeInfo)
--
-- -- Path Elements --
-- inAndApp :: NoteExtractor v loc Int
-- inAndApp = do
--   ((_,_,n),_) <- adjacent inSynthesizeApp inAndApp
--   pure n
--
-- inAndApp' :: PathExtractor v loc ()
-- inAndApp' = PathExtractor $ \case C.InAndApp -> Just (); _ -> Nothing
--
-- -- inAndApp :: NoteExtractor v loc ()
-- -- inAndApp = exactly1AppBefore . PathExtractor $ \case
-- --   C.InAndApp -> Just ()
-- --   _ -> Nothing
--
-- inOrApp :: NoteExtractor v loc ()
-- inOrApp = exactly1AppBefore . PathExtractor $ \case
--   C.InOrApp -> Just ()
--   _ -> Nothing
--
-- inIfCond :: NoteExtractor v loc ()
-- inIfCond = exactly1AppBefore . PathExtractor $ \case
--   C.InIfCond -> Just ()
--   _ -> Nothing
--
-- inIfBody :: NoteExtractor v loc loc
-- inIfBody = exactly1AppBefore . PathExtractor $ \case
--   C.InIfBody loc -> Just loc
--   _ -> Nothing
--
-- inVectorApp :: NoteExtractor v loc loc
-- inVectorApp = exactly1AppBefore . PathExtractor $ \case
--   C.InVectorApp loc -> Just loc
--   _ -> Nothing
--
-- inMatchCaseGuard :: NoteExtractor v loc ()
-- inMatchCaseGuard = do
--   (prefix, _) <- elementsUntil . PathExtractor $ \case
--     C.InMatch _ -> Just ()
--     _ -> Nothing
--   -- so brittle, whee!
--   if length prefix == 5 then pure () else mzero
--
-- inMatchCaseBody :: NoteExtractor v loc loc
-- inMatchCaseBody = do
--   (prefix, loc) <- elementsUntil . PathExtractor $ \case
--     C.InMatch loc -> Just loc
--     _ -> Nothing
--   -- so brittle, but I guess it's okay!
--   if length prefix == 3 then pure loc else mzero
--
--
-- -- Causes --
-- cause :: NoteExtractor v loc (C.Cause v loc)
-- cause = NoteExtractor $ pure . C.cause
--
-- typeMismatch :: NoteExtractor v loc (C.Context v loc)
-- typeMismatch = cause >>= \case
--   C.TypeMismatch c -> pure c
--   _ -> mzero
--
-- illFormedType :: NoteExtractor v loc (C.Context v loc)
-- illFormedType = cause >>= \case
--   C.IllFormedType c -> pure c
--   _ -> mzero
--
-- unknownSymbol :: NoteExtractor v loc (loc, v)
-- unknownSymbol = cause >>= \case
--   C.UnknownSymbol loc v -> pure (loc, v)
--   _ -> mzero
--
-- unknownTerm :: NoteExtractor v loc (loc, v, [C.Suggestion v loc], C.Type v loc)
-- unknownTerm = cause >>= \case
--   C.UnknownTerm loc v suggestions expectedType -> pure (loc, v, suggestions, expectedType)
--   _ -> mzero
--
-- abilityCheckFailure :: NoteExtractor v loc ([C.Type v loc], [C.Type v loc], C.Context v loc)
-- abilityCheckFailure = cause >>= \case
--   C.AbilityCheckFailure ambient requested ctx -> pure (ambient, requested, ctx)
--   _ -> mzero
--
-- effectConstructorWrongArgCount :: NoteExtractor v loc (C.ExpectedArgCount, C.ActualArgCount, Reference, C.ConstructorId)
-- effectConstructorWrongArgCount = cause >>= \case
--   C.EffectConstructorWrongArgCount expected actual r cid -> pure (expected, actual, r, cid)
--   _ -> mzero
--
-- malformedEffectBind :: NoteExtractor v loc (C.Type v loc, C.Type v loc, [C.Type v loc])
-- malformedEffectBind = cause >>= \case
--   C.MalformedEffectBind ctor ctorResult es -> pure (ctor, ctorResult, es)
--   _ -> mzero
--
-- solvedBlank :: NoteExtractor v loc (B.Recorded loc, v, C.Type v loc)
-- solvedBlank = cause >>= \case
--   C.SolvedBlank b v t -> pure (b, v, t)
--   _ -> mzero
--
-- -- Scopes --
-- inSynthesizeApps :: PathExtractor v loc (C.Term v loc, C.Type v loc, [C.Term v loc])
-- inSynthesizeApps = PathExtractor $ \case
--   C.InSynthesizeApps f ft e -> Just (f, ft,e)
--   _ -> mzero
--
-- inSynthesizeApp :: PathExtractor v loc (C.Type v loc, C.Term v loc, Int)
-- inSynthesizeApp = PathExtractor $ \case
--   C.InSynthesizeApp t e n -> Just (t,e,n)
--   _ -> Nothing
--
-- inSynthesize :: PathExtractor v loc (C.Term v loc)
-- inSynthesize = PathExtractor $ \case
--   C.InSynthesize t -> Just t
--   _ -> Nothing
--
-- inCheck :: PathExtractor v loc (C.Term v loc, C.Type v loc)
-- inCheck = PathExtractor $ \case
--   C.InCheck e t -> Just (e,t)
--   _ -> Nothing
--
-- inSubtype :: PathExtractor v loc (C.Type v loc, C.Type v loc)
-- inSubtype = PathExtractor $ \case
--   C.InSubtype found expected -> Just (found, expected)
--   _ -> Nothing
--
-- -- | Handle v
-- adjacent :: PathExtractor v loc a -> PathExtractor v loc b -> NoteExtractor v loc (Int, (a, b))
-- adjacent (PathExtractor a) (PathExtractor b) =
--   NoteExtractor $ go Nothing . toList . C.path where
--   go _ [] = Nothing
--   go Nothing (h:t) = go (a h) t
--   go (Just a) (h:t) = case b h of Nothing -> go Nothing t; Just b -> Just (a,b)
--
-- adjacent3 :: (Show a, Show b, Show c, Show loc, Var v)
--           => PathExtractor v loc a
--           -> PathExtractor v loc b
--           -> PathExtractor v loc c
--           -> NoteExtractor v loc (a, b, c)
-- adjacent3 (PathExtractor a) (PathExtractor b) (PathExtractor c)=
--   NoteExtractor $ go Nothing Nothing . toList . C.path where
--   go _ _ [] = Nothing
--   go Nothing Nothing (h:t) = go (a h) Nothing t
--   go (Just a) Nothing (h:t) = case b h of
--     Nothing -> go Nothing Nothing (h:t)
--     Just b  -> go (Just a) (Just (h,b)) t
--   go (Just a) (Just (bh,b)) (h:t) = case c h of
--     Nothing -> go Nothing Nothing (bh:h:t)
--     Just c  -> Just (a,b,c)
--   go a b c =
--     error ("invalid state in Extractor.adjacent3:\n" ++
--            show a ++ "\n" ++
--            show b ++ "\n" ++
--            show c ++ "\n")
--
-- adjacent4 :: (Show a, Show b, Show c, Show d, Show loc, Var v)
--           => PathExtractor v loc a
--           -> PathExtractor v loc b
--           -> PathExtractor v loc c
--           -> PathExtractor v loc d
--           -> NoteExtractor v loc (a, b, c, d)
-- adjacent4 (PathExtractor a) (PathExtractor b) (PathExtractor c) (PathExtractor d)=
--   NoteExtractor $ go Nothing Nothing Nothing . toList . C.path where
--   go _ _ _ [] = Nothing
--   go Nothing Nothing Nothing (h:t) = go (a h) Nothing Nothing t
--   go (Just a) Nothing Nothing (h:t) = case b h of
--     Nothing -> go Nothing Nothing Nothing (h:t)
--     Just b  -> go (Just a) (Just (h,b)) Nothing t
--   go (Just a) (Just (bh,b)) Nothing (h:t) = case c h of
--     Nothing -> go Nothing Nothing Nothing (bh:h:t)
--     Just c  -> go (Just a) (Just (bh, b)) (Just (h,c)) t
--   go (Just a) (Just (bh,b)) (Just (ch,c)) (h:t) = case d h of
--     Nothing -> go Nothing Nothing Nothing (bh:ch:h:t)
--     Just d  -> Just (a,b,c,d)
--   go a b c d =
--     error ("invalid state in Extractor.adjacent4:\n" ++
--            show a ++ "\n" ++
--            show b ++ "\n" ++
--            show c ++ "\n" ++
--            show d ++ "\n")

-- instance Functor (PathExtractor v loc) where
--   fmap = liftM
--
-- instance Applicative (PathExtractor v loc) where
--   (<*>) = ap
--   pure = return
--
-- instance Monad (PathExtractor v loc) where
--   fail _s = empty
--   return a = PathExtractor (\_ -> Just a)
--   PathExtractor r >>= f = PathExtractor go
--     where
--       go path = case r path of
--         Nothing -> Nothing
--         Just a  -> runPath (f a) path
--
-- instance Alternative (PathExtractor v loc) where
--   empty = mzero
--   (<|>) = mplus
--
-- instance MonadPlus (PathExtractor v loc) where
--   mzero = PathExtractor (\_ -> Nothing)
--   mplus (PathExtractor f1) (PathExtractor f2) =
--     PathExtractor (\note -> f1 note `mplus` f2 note)
--
-- instance Functor (NoteExtractor v loc) where
--   fmap = liftM
--
-- instance Applicative (NoteExtractor v loc) where
--   (<*>) = ap
--   pure = return
--
-- instance Monad (NoteExtractor v loc) where
--   fail _s = empty
--   return a = NoteExtractor (\_ -> Just a)
--   NoteExtractor r >>= f = NoteExtractor go
--     where
--       go note = case r note of
--         Nothing -> Nothing
--         Just a  -> run (f a) note
--
-- instance Alternative (NoteExtractor v loc) where
--   empty = mzero
--   (<|>) = mplus
--
-- instance MonadPlus (NoteExtractor v loc) where
--   mzero = NoteExtractor (\_ -> Nothing)
--   mplus (NoteExtractor f1) (NoteExtractor f2) =
--     NoteExtractor (\note -> f1 note `mplus` f2 note)
