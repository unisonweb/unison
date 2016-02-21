{-# Language TemplateHaskell #-}
{-# Language ViewPatterns #-}

module Unison.Paths where

import Data.Aeson.TH
import Data.List
import Data.Maybe
import Data.Vector ((//))
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Vector as Vector
import qualified Unison.Term as E
import qualified Unison.Type as T

data Target v
  = Term (Term v)
  | Type (Type v)
  | Var v
  | Declaration v (Term v)
  -- Metadata

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

deriveJSON defaultOptions ''PathElement

