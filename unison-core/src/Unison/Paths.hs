{-# LANGUAGE DeriveGeneric #-}

module Unison.Paths where

import Data.List
import qualified Data.Sequence as Sequence
import Unison.ABT (V)
import qualified Unison.ABT as ABT
import Unison.Prelude
import qualified Unison.Term as E
import qualified Unison.Type as T
import Unison.Var (Var)

type Type v = T.Type v ()

type Term v = E.Term v ()

data Target v
  = Term (Term v)
  | Type (Type v)
  | Var v
  | Declaration v (Term v)
  deriving (Generic)

-- Metadata

vmap :: Ord v2 => (v -> v2) -> Target v -> Target v2
vmap f (Var v) = Var (f v)
vmap f (Declaration v b) = Declaration (f v) (E.vmap f b)
vmap f (Term t) = Term (E.vmap f t)
vmap f (Type t) = Type (ABT.vmap f t)

data PathElement
  = -- | Points at function in a function/type application
    Fn
  | -- | Points at the argument of a function/type application
    Arg
  | -- | Points at the body of a lambda, let, binding, forall, or annotation
    Body
  | -- | Points at the symbol bound by a `let`, `lambda` or `forall` binder
    Bound
  | -- | Points at a particular binding in a let
    Binding !Int
  | -- | Points at the index of a vector
    Index !Int
  | -- | Points into the annotation
    Annotation
  | -- | Points at the left of an `Arrow`
    Input
  | -- | Points at the right of an `Arrow`
    Output
  deriving (Eq, Ord, Show, Generic)

focus1 ::
  Var v =>
  PathElement ->
  ABT.Path (Target v) (Target (V v)) (Target v) (Target (V v)) [v]
focus1 e = ABT.Path go'
  where
    go' t = go e t
    w = E.vmap ABT.Bound
    wt = ABT.vmap ABT.Bound
    go Fn (Term (E.App' fn arg)) =
      Just
        (Term fn, \fn -> Term <$> (E.app () <$> asTerm fn <*> pure (w arg)), [])
    go Fn (Type (T.App' fn arg)) =
      Just
        (Type fn, \fn -> Type <$> (T.app () <$> asType fn <*> pure (wt arg)), [])
    go Arg (Term (E.App' fn arg)) =
      Just (Term arg, \arg -> Term <$> (E.app () (w fn) <$> asTerm arg), [])
    go Arg (Type (T.App' fn arg)) =
      Just (Type arg, \arg -> Type <$> (T.app () (wt fn) <$> asType arg), [])
    go Body (Term (E.LamNamed' v body)) =
      Just
        (Term body, \t -> Term . set <$> asTerm t, [v])
      where
        set body = ABT.tm (E.Lam (ABT.absr v body))
    go Body (Term (E.Let1NamedTop' top v b body)) =
      Just
        (Term body, \t -> Term . set <$> asTerm t, [v])
      where
        set body = ABT.tm (E.Let top (w b) (ABT.absr v body))
    go p (Term (ABT.Cycle' vs (ABT.Tm' (E.LetRec top bs body)))) = case p of
      Body -> Just (Term body, \body -> Term . set <$> asTerm body, vs)
        where
          set body = ABT.cycler vs (ABT.tm (E.LetRec top (map w bs) body))
      Binding i
        | i >= 0 && i < length bs ->
          Just
            ( Declaration (vs !! i) (bs !! i),
              \b -> Term . set <$> asDeclaration b,
              vs
            )
        where
          replace f i a vs = map f (take i vs) ++ [a] ++ map f (drop (i + 1) vs)
          set (v, b) =
            let tm0 = ABT.tm (E.LetRec top (replace w i b bs) (w body))
                v0 = ABT.Bound (vs !! i)
                tm = if v /= v0 then ABT.rename v0 v tm0 else tm
             in ABT.cycler (replace id i (ABT.unvar v) vs) tm
      _ -> Nothing
    go Body (Type (T.ForallNamed' v body)) =
      Just
        (Type body, \t -> Type . set <$> asType t, [v])
      where
        set body = ABT.tm (T.Forall (ABT.absr v body))
    go Body (Declaration v body) =
      Just (Term body, \body -> Declaration (ABT.Bound v) <$> asTerm body, [])
    go Bound (Declaration v body) =
      Just (Var v, \v -> Declaration <$> asVar v <*> pure (w body), [])
    go Bound (Term (E.LamNamed' v body)) =
      Just (Var v, \v -> Term <$> (E.lam () <$> asVar v <*> pure (w body)), [])
    go Bound (Term (E.Let1NamedTop' top v b body)) =
      Just
        ( Var v,
          \v -> (\v -> Term $ E.let1 top [(((), v), w b)] (w body)) <$> asVar v,
          []
        )
    go Bound (Type (T.ForallNamed' v body)) =
      Just
        (Var v, \v -> Type <$> (T.forall () <$> asVar v <*> pure (wt body)), [])
    go (Index i) (Term (E.Sequence' vs))
      | i < Sequence.length vs && i >= 0 =
        Just
          ( Term (vs `Sequence.index` i),
            \e -> (\e -> Term $ E.seq' () $ Sequence.update i e (fmap w vs)) <$> asTerm e,
            []
          )
    go (Binding i) (Term (E.Let1NamedTop' top v b body))
      | i <= 0 =
        Just
          (Declaration v b, set, [])
      where
        set (Declaration v b) = pure . Term $ E.let1 top [(((), v), b)] (w body)
        set _ = Nothing
    go Annotation (Term (E.Ann' e t)) =
      Just (Type t, \t -> Term . E.ann () (w e) <$> asType t, [])
    go Body (Term (E.Ann' body t)) =
      Just
        (Term body, \body -> Term . flip (E.ann ()) (wt t) <$> asTerm body, [])
    go Input (Type (T.Arrow' i o)) =
      Just
        (Type i, \i -> Type <$> (T.arrow () <$> asType i <*> pure (wt o)), [])
    go Output (Type (T.Arrow' i o)) =
      Just (Type o, \o -> Type . T.arrow () (wt i) <$> asType o, [])
    go _ _ = Nothing

type Path = [PathElement]

focus :: Var v => Path -> Target v -> Maybe (Target v, Target (V v) -> Maybe (Target v), [v])
focus p t = tweak <$> ABT.focus (foldr ABT.compose ABT.here (map focus1 p)) t
  where
    tweak (get, set, vs) = (get, \t -> vmap ABT.unvar <$> set t, vs)

at :: Var v => Path -> Target v -> Maybe (Target v)
at path t = (\(a, _, _) -> a) <$> focus path t

atTerm :: Var v => Path -> Term v -> Maybe (Term v)
atTerm path t = asTerm =<< at path (Term t)

atType :: Var v => Path -> Type v -> Maybe (Type v)
atType path t = asType =<< at path (Type t)

modify :: Var v => (Target v -> Target (V v)) -> Path -> Target v -> Maybe (Target v)
modify f path t = focus path t >>= \(at, set, _) -> set (f at)

modifyTerm :: Var v => (Term v -> Term (V v)) -> Path -> Term v -> Maybe (Term v)
modifyTerm f p t = do
  (at, set, _) <- focus p (Term t)
  t <- asTerm at
  asTerm =<< set (Term $ f t)

modifyTerm' :: Var v => (Term v -> Term (V v)) -> Path -> Term v -> Term v
modifyTerm' f p t = fromMaybe t $ modifyTerm f p t

modifyType :: Var v => (Type v -> Type (V v)) -> Path -> Type v -> Maybe (Type v)
modifyType f p t = do
  (at, set, _) <- focus p (Type t)
  t <- asType at
  asType =<< set (Type $ f t)

inScopeAt :: Var v => Path -> Target v -> [v]
inScopeAt p t = maybe [] (\(_, _, vs) -> vs) (focus p t)

inScopeAtTerm :: Var v => Path -> Term v -> [v]
inScopeAtTerm p t = inScopeAt p (Term t)

inScopeAtType :: Var v => Path -> Type v -> [v]
inScopeAtType p t = inScopeAt p (Type t)

insertTerm :: Var v => Path -> Term v -> Maybe (Term v)
insertTerm at _ | null at = Nothing
insertTerm at ctx = do
  let at' = init at
  (parent, set, _) <- focus at' (Term ctx)
  case parent of
    Term (E.Sequence' vs) -> do
      i <- listToMaybe [i | Index i <- [last at]]
      let v2 =
            E.seq'
              ()
              ( (E.vmap ABT.Bound <$> Sequence.take (i + 1) vs)
                  `mappend` pure (E.blank ())
                  `mappend` (E.vmap ABT.Bound <$> Sequence.drop (i + 1) vs)
              )
      asTerm =<< set (Term v2)
    _ -> Nothing -- todo - allow other types of insertions, like \x -> y to \x x2 -> y

-- | Return the list of all prefixes of the input path
pathPrefixes :: Path -> [Path]
pathPrefixes = inits

-- | Add an element onto the end of this 'Path'
pathExtend :: PathElement -> Path -> Path
pathExtend e p = p ++ [e]

parent :: Path -> Maybe Path
parent [] = Nothing
parent p = Just (init p)

parent' :: Path -> Path
parent' = fromMaybe [] . parent

asTerm :: Target v -> Maybe (Term v)
asTerm (Term t) = Just t
asTerm _ = Nothing

asType :: Target v -> Maybe (Type v)
asType (Type t) = Just t
asType _ = Nothing

asVar :: Target v -> Maybe v
asVar (Var v) = Just v
asVar _ = Nothing

asDeclaration :: Target v -> Maybe (v, Term v)
asDeclaration (Declaration v b) = Just (v, b)
asDeclaration _ = Nothing
