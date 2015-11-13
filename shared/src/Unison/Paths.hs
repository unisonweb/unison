module Unison.Paths where

import Unison.Var (Var)
import Data.Maybe
import Data.Text (Text)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.ABT as ABT
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as E
import qualified Unison.Type as T

data Target v
  = Term (Term (Symbol v))
  | Type (Type (Symbol v))
  | Symbol (Symbol v)
  | Name Text
  | Var v
  -- Metadata

data PathElement
  = Fn -- ^ Points at function in a function/type application
  | Arg -- ^ Points at the argument of a function/type application
  | Body -- ^ Points at the body of a lambda, let, or forall
  | Introduced -- ^ Points at the symbol introduced by a `let`, `lambda` or `forall` binder
  | NameOf -- ^ Points at the name of the introduced symbol
  | VarOf -- ^ Points at the `v` contained in a symbol
  | Binding !Int -- ^ Points at a particular binding in a let
  | Index !Int -- ^ Points at the index of a vector
  | Annotation -- ^ Points into the annotation
  | Input -- ^ Points at the left of an `Arrow`
  | Output -- ^ Points at the right of an `Arrow`
  deriving (Eq,Ord,Show)

focus1 :: Var v => PathElement -> Target v -> Maybe (Target v, Target v -> Maybe (Target v))
focus1 Fn (Term (E.App' fn arg)) = Just (Term fn, \fn -> Term <$> (E.app <$> asTerm fn <*> pure arg))
focus1 Fn (Type (T.App' fn arg)) = Just (Type fn, \fn -> Type <$> (T.app <$> asType fn <*> pure arg))
focus1 Arg (Term (E.App' fn arg)) = Just (Term arg, \arg -> Term <$> (E.app fn <$> asTerm arg))
focus1 Arg (Type (T.App' fn arg)) = Just (Type arg, \arg -> Type <$> (T.app fn <$> asType arg))
focus1 Body (Term (E.Lam' v body)) = Just (Term body, \body -> Term . E.lam v <$> asTerm body)
focus1 Body (Term (E.Let1' v b body)) = Just (Term body, \body -> Term . E.let1 [(v,b)] <$> asTerm body)
focus1 Body (Term (E.LetRec' bs body)) = Just (Term body, \body -> Term . E.letRec bs <$> asTerm body)
focus1 Body (Type (T.Forall' v body)) = Just (Type body, \body -> Type . T.forall v <$> asType body)
focus1 Introduced (Term (E.Lam' v body)) = Just (Symbol v, \v -> Term <$> (E.lam <$> asSymbol v <*> pure body))
focus1 Introduced (Term (E.Let1' v b body)) = Just (Symbol v, \v -> (\v -> Term $ E.let1 [(v,b)] body) <$> asSymbol v)
focus1 Introduced (Type (T.Forall' v body)) = Just (Symbol v, \v -> Type <$> (T.forall <$> asSymbol v <*> pure body))
focus1 NameOf (Symbol (Symbol.Symbol id n a)) = Just (Name n, \n -> (\n -> Symbol (Symbol.Symbol id n a)) <$> asText n)
focus1 VarOf (Symbol (Symbol.Symbol id n a)) = Just (Var a, \a -> (\a -> Symbol (Symbol.Symbol id n a)) <$> asVar a)
focus1 (Binding i) (Term (E.Let1' v b body)) | i <= 0 = Just (Term b, \b -> (\b -> Term $ E.let1 [(v,b)] body) <$> asTerm b)
focus1 (Binding i) (Term (E.LetRec' bs body)) =
  listToMaybe (drop i bs)
  >>= \(v,b) -> Just (Term b, \b -> (\b -> Term $ E.letRec (take i bs ++ [(v,b)] ++ drop (i+1) bs) body) <$> asTerm b)
focus1 Annotation (Term (E.Ann' e t)) = Just (Type t, \t -> Term . E.ann e <$> asType t)
focus1 Input (Type (T.Arrow' i o)) = Just (Type i, \i -> Type <$> (T.arrow <$> asType i <*> pure o))
focus1 Output (Type (T.Arrow' i o)) = Just (Type o, \o -> Type . T.arrow i <$> asType o)
focus1 _ _ = Nothing

type Path = [PathElement]

focus :: Var v => Path -> Target v -> Maybe (Target v, Target v -> Maybe (Target v))
focus [] t = Just (t, Just)
focus (hd:tl) t = do
  (hdSub, updateHd) <- focus1 hd t
  (tlSub, updateTl) <- focus tl hdSub
  pure (tlSub, \tlSub -> updateHd =<< updateTl tlSub)

at :: Var v => Path -> Target v -> Maybe (Target v)
at path t = fst <$> focus path t

modify :: Var v => (Target v -> Target v) -> Path -> Target v -> Maybe (Target v)
modify f path t = focus path t >>= \(at,set) -> set (f at)

asTerm :: Target v -> Maybe (Term (Symbol v))
asTerm (Term t) = Just t
asTerm _ = Nothing

asType :: Target v -> Maybe (Type (Symbol v))
asType (Type t) = Just t
asType _ = Nothing

asSymbol :: Target v -> Maybe (Symbol v)
asSymbol (Symbol v) = Just v
asSymbol _ = Nothing

asText :: Target v -> Maybe Text
asText (Name v) = Just v
asText _ = Nothing

asVar :: Target v -> Maybe v
asVar (Var v) = Just v
asVar _ = Nothing
