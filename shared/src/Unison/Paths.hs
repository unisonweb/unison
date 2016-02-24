{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

module Unison.Paths where

import Data.Aeson.TH
import Data.List
import Data.Maybe
import Data.Vector ((//))
import Unison.ABT (V)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Term as E
import qualified Unison.Type as T

data Target v
  = Term (Term v)
  | Type (Type v)
  | Var v
  | Declaration v (Term v)
  -- Metadata

_Term :: ABT.Path (Target v) (Target (V v)) (Term v) (Term (V v))
_Term = ABT.Path go where
  go (Term t) = Just (t, \t -> Just (Term t))
  go _ = Nothing

_Type :: ABT.Path (Target v) (Target (V v)) (Type v) (Type (V v))
_Type = ABT.Path go where
  go (Type t) = Just (t, \t -> Just (Type t))
  go _ = Nothing

_Var :: ABT.Path (Target v) (Target (V v)) v v
_Var = ABT.Path go where
  go (Var v) = Just (v, \v -> Just (Var (ABT.Bound v)))
  go _ = Nothing

_Declaration :: ABT.Path (Target v) (Target (V v)) (v, Term v) (v, Term (V v))
_Declaration = ABT.Path go where
  go (Declaration v t) = Just ((v,t), \(v,t) -> Just (Declaration (ABT.Bound v) t))
  go _ = Nothing

_1 :: ABT.Path (a,b) (a',b) a a'
_1 = ABT.Path go where go (a,b) = Just (a, \a -> Just (a,b))

_2 :: ABT.Path (a,b) (a,b') b b'
_2 = ABT.Path go where go (a,b) = Just (b, \b -> Just (a,b))

data PathElement
  = Fn -- ^ Points at function in a function/type application
  | Arg -- ^ Points at the argument of a function/type application
  | Body -- ^ Points at the body of a lambda, let, binding, or forall
  | Bound -- ^ Points at the symbol bound by a `let`, `lambda` or `forall` binder
  | Binding !Int -- ^ Points at a particular binding in a let
  | Index !Int -- ^ Points at the index of a vector
  | Annotation -- ^ Points into the annotation
  | Input -- ^ Points at the left of an `Arrow`
  | Output -- ^ Points at the right of an `Arrow`
  deriving (Eq,Ord,Show)

focus1' :: Var v => PathElement -> ABT.Path (Target v) (Target (V v)) (Target v) (Target (V v))
focus1' e = ABT.Path go' where
  go' t = go e t
  w = E.vmap ABT.Bound
  wt = ABT.vmap ABT.Bound
  go Fn (Term (E.App' fn arg)) =
    Just (Term fn, \fn -> Term <$> (E.app <$> asTerm fn <*> pure (w arg)))
  go Fn (Type (T.App' fn arg)) =
    Just (Type fn, \fn -> Type <$> (T.app <$> asType fn <*> pure (wt arg)))
  go Arg (Term (E.App' fn arg)) =
    Just (Term arg, \arg -> Term <$> (E.app (w fn) <$> asTerm arg))
  go Arg (Type (T.App' fn arg)) =
    Just (Type arg, \arg -> Type <$> (T.app (wt fn) <$> asType arg))
  go Body (Term (E.LamNamed' v body)) = Just (Term body, \t -> Term . set <$> asTerm t) where
    set body = ABT.tm (E.Lam (ABT.absr v body))
  go Body (Term (E.Let1Named' v b body)) = Just (Term body, \t -> Term . set <$> asTerm t) where
    set body = ABT.tm (E.Let (w b) (ABT.absr v body))
  go p (Term (ABT.Cycle' vs (ABT.Tm' (E.LetRec bs body)))) = case p of
    Body -> Just (Term body, \body -> Term . set <$> asTerm body) where
      set body = ABT.cycler vs (ABT.tm (E.LetRec (map w bs) body))
    Binding i | i >= 0 && i < length bs ->
      Just (Declaration (vs !! i) (bs !! i), \b -> Term . set <$> asDeclaration b)
      where
      replace f i a vs = map f (take i vs) ++ [a] ++ map f (drop (i+1) vs)
      set (v,b) =
        let tm0 = ABT.tm (E.LetRec (replace w i b bs) (w body))
            v0 = ABT.Bound (vs !! i)
            tm = if v /= v0 then ABT.rename v0 v tm0 else tm
        in ABT.cycler (replace id i (ABT.unvar v) vs) tm
    _ -> Nothing
  go Body (Type (T.ForallNamed' v body)) = Just (Type body, \t -> Type . set <$> asType t) where
    set body = ABT.tm (T.Forall (ABT.absr v body))
  go Body (Declaration v body) = Just (Term body, \body -> Declaration (ABT.Bound v) <$> asTerm body)
  go Bound (Declaration v body) = Just (Var v, \v -> Declaration <$> asVar v <*> pure (w body))
  go Bound (Term (E.LamNamed' v body)) =
    Just (Var v, \v -> Term <$> (E.lam <$> asVar v <*> pure (w body)))
  go Bound (Term (E.Let1Named' v b body)) =
    Just (Var v, \v -> (\v -> Term $ E.let1 [(v,w b)] (w body)) <$> asVar v)
  go Bound (Type (T.ForallNamed' v body)) =
    Just (Var v, \v -> Type <$> (T.forall <$> asVar v <*> pure (wt body)))
  go (Index i) (Term (E.Vector' vs)) | i < Vector.length vs && i >= 0 =
    Just (Term (vs `Vector.unsafeIndex` i),
          \e -> (\e -> Term $ E.vector' $ fmap w vs // [(i,e)]) <$> asTerm e)
  go (Binding i) (Term (E.Let1Named' v b body)) | i <= 0 = Just (Declaration v b, set) where
    set (Declaration v b) = pure . Term $ E.let1 [(v, b)] (w body)
    set _ = Nothing
  --go (Binding i) (Term (E.LetRecNamed' bs body)) =
  --  listToMaybe (drop i bs)
  --  >>= \(v,b) -> Just (Declaration v b, set, map fst bs) where
  --  set (Declaration v b) = pure . Term $ E.letRec (take i bs ++ [(v,b)] ++ drop (i+1) bs) body
  --  set _ = Nothing
  go Annotation (Term (E.Ann' e t)) = Just (Type t, \t -> Term . E.ann (w e) <$> asType t)
  go Input (Type (T.Arrow' i o)) = Just (Type i, \i -> Type <$> (T.arrow <$> asType i <*> pure (wt o)))
  go Output (Type (T.Arrow' i o)) = Just (Type o, \o -> Type . T.arrow (wt i) <$> asType o)
  go _ _ = Nothing

focus1 :: Var v => PathElement -> Target v -> Maybe (Target v, Target v -> Maybe (Target v), [v])
focus1 Fn (Term (E.App' fn arg)) =
  Just (Term fn, \fn -> Term <$> (E.app <$> asTerm fn <*> pure arg), [])
focus1 Fn (Type (T.App' fn arg)) =
  Just (Type fn, \fn -> Type <$> (T.app <$> asType fn <*> pure arg), [])
focus1 Arg (Term (E.App' fn arg)) =
  Just (Term arg, \arg -> Term <$> (E.app fn <$> asTerm arg), [])
focus1 Arg (Type (T.App' fn arg)) =
  Just (Type arg, \arg -> Type <$> (T.app fn <$> asType arg), [])
focus1 Body (Term (E.LamNamed' v body)) =
  Just (Term body, \body -> Term . E.lam v <$> asTerm body, [v])
focus1 Body (Term (E.Let1Named' v b body)) =
  Just (Term body, \body -> Term . E.let1 [(v,b)] <$> asTerm body, [v])
focus1 Body (Term (E.LetRecNamed' bs body)) =
  Just (Term body, \body -> Term . E.letRec bs <$> asTerm body, map fst bs)
focus1 Body (Type (T.ForallNamed' v body)) =
  Just (Type body, \body -> Type . T.forall v <$> asType body, [v])
focus1 Body (Declaration v body) = Just (Term body, \body -> Declaration v <$> asTerm body, [])
focus1 Bound (Declaration v body) = Just (Var v, \v -> Declaration <$> asVar v <*> pure body, [])
focus1 Bound (Term (E.LamNamed' v body)) =
  Just (Var v, \v -> Term <$> (E.lam <$> asVar v <*> pure body), [])
focus1 Bound (Term (E.Let1Named' v b body)) =
  Just (Var v, \v -> (\v -> Term $ E.let1 [(v,b)] body) <$> asVar v, [])
focus1 Bound (Type (T.ForallNamed' v body)) =
  Just (Var v, \v -> Type <$> (T.forall <$> asVar v <*> pure body), [])
focus1 (Index i) (Term (E.Vector' vs)) | i < Vector.length vs && i >= 0 =
  Just (Term (vs `Vector.unsafeIndex` i),
        \e -> (\e -> Term $ E.vector' $ vs // [(i,e)]) <$> asTerm e,
        [])
focus1 (Binding i) (Term (E.Let1Named' v b body)) | i <= 0 = Just (Declaration v b, set, []) where
  set (Declaration v b) = pure . Term $ E.let1 [(v,b)] body
  set _ = Nothing
focus1 (Binding i) (Term (E.LetRecNamed' bs body)) =
  listToMaybe (drop i bs)
  >>= \(v,b) -> Just (Declaration v b, set, map fst bs) where
  set (Declaration v b) = pure . Term $ E.letRec (take i bs ++ [(v,b)] ++ drop (i+1) bs) body
  set _ = Nothing
focus1 Annotation (Term (E.Ann' e t)) = Just (Type t, \t -> Term . E.ann e <$> asType t, [])
focus1 Input (Type (T.Arrow' i o)) = Just (Type i, \i -> Type <$> (T.arrow <$> asType i <*> pure o), [])
focus1 Output (Type (T.Arrow' i o)) = Just (Type o, \o -> Type . T.arrow i <$> asType o, [])
focus1 _ _ = Nothing

type Path = [PathElement]

focus :: Var v => Path -> Target v -> Maybe (Target v, Target v -> Maybe (Target v), [v])
focus [] t = Just (t, Just, [])
focus (hd:tl) t = do
  (hdSub, updateHd, vs) <- focus1 hd t
  (tlSub, updateTl, vs2) <- focus tl hdSub
  pure (tlSub, \tlSub -> updateHd =<< updateTl tlSub, vs ++ vs2)

inScopeAt :: Var v => Path -> Target v -> [v]
inScopeAt path t = maybe [] (\(_,_,vs) -> vs) (focus path t)

inScopeAtTerm :: Var v => Path -> Term v -> [v]
inScopeAtTerm path t = maybe [] (\(_,_,vs) -> vs) (focus path (Term t))

at :: Var v => Path -> Target v -> Maybe (Target v)
at path t = (\(a,_,_) -> a) <$> focus path t

atTerm :: Var v => Path -> Term v -> Maybe (Term v)
atTerm path t = asTerm =<< at path (Term t)

atType :: Var v => Path -> Type v -> Maybe (Type v)
atType path t = asType =<< at path (Type t)

modify :: Var v => (Target v -> Target v) -> Path -> Target v -> Maybe (Target v)
modify f path t = focus path t >>= \(at,set,_) -> set (f at)

modifyTerm :: Var v => (Term v -> Term v) -> Path -> Term v -> Maybe (Term v)
modifyTerm f p t = do
  (at,set,_) <- focus p (Term t)
  t <- asTerm at
  asTerm =<< set (Term $ f t)

modifyTerm' :: Var v => (Term v -> Term v) -> Path -> Term v -> Term v
modifyTerm' f p t = fromMaybe t $ modifyTerm f p t

modifyType :: Var v => (Type v -> Type v) -> Path -> Type v -> Maybe (Type v)
modifyType f p t = do
  (at,set,_) <- focus p (Type t)
  t <- asType at
  asType =<< set (Type $ f t)

insertTerm :: Var v => Path -> Term v -> Maybe (Term v)
insertTerm at _ | null at = Nothing
insertTerm at ctx = do
  let at' = init at
  (parent,set,_) <- focus at' (Term ctx)
  case parent of
    Term (E.Vector' vs) -> do
      i <- listToMaybe [i | Index i <- [last at]]
      let v2 = E.vector' (Vector.take (i+1) vs `mappend` Vector.singleton E.blank `mappend` Vector.drop (i+1) vs)
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
asDeclaration (Declaration v b) = Just (v,b)
asDeclaration _ = Nothing

deriveJSON defaultOptions ''PathElement

