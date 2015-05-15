{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Term where

import Control.Applicative
import Control.Monad
import Data.Aeson.TH
import Data.Functor.Classes
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector, (!?))
import GHC.Generics
import Text.Show
import Unison.Hash (Hash)
import Unison.Reference (Reference)
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Aeson as Aeson
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Reference as Reference
import qualified Unison.Type as T
import qualified Unison.Distance as Distance
import qualified Unison.JSON as J

-- | Literals in the Unison language
data Literal
  = Number Double
  | Text Text
  | Distance Distance.Distance
  deriving (Eq,Ord,Generic)

-- | Base functor for terms in the Unison language
data F a
  = Lit Literal
  | Blank -- An expression that has not been filled in, has type `forall a . a`
  | Ref Reference
  | App a a
  | Ann a T.Type
  | Vector (Vector a)
  | Lam a
  -- Invariant: let rec blocks have an outer ABT.Cycle which introduces as many
  -- variables as there are bindings
  | LetRec [a] a
  | Let a a
  deriving (Eq,Foldable,Functor,Generic1,Traversable)

-- | Terms are represented as ABTs over the base functor F.
type Term = ABT.Term F

-- nicer pattern syntax

pattern Var' v <- ABT.Var' v
pattern Lit' l <- (ABT.out -> ABT.Tm (Lit l))
pattern Number' n <- Lit' (Number n)
pattern Text' s <- Lit' (Text s)
pattern Blank' <- (ABT.out -> ABT.Tm Blank)
pattern Ref' r <- (ABT.out -> ABT.Tm (Ref r))
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))
pattern Ann' x t <- (ABT.out -> ABT.Tm (Ann x t))
pattern Vector' xs <- (ABT.out -> ABT.Tm (Vector xs))
pattern Lam' v body <- (ABT.out -> ABT.Tm (Lam (ABT.Term _ (ABT.Abs v body))))
pattern Let1' v b e <- (ABT.out -> ABT.Tm (Let b (ABT.Abs' v e)))
pattern Let' bs e relet rec <- (unLets -> Just (bs,e,relet,rec))
pattern LetRec' bs e <- (unLetRec -> Just (bs,e))

fresh :: Term -> ABT.V -> ABT.V
fresh = ABT.fresh

-- some smart constructors

var :: ABT.V -> Term
var = ABT.var

var' :: Text -> Term
var' = var . ABT.v'

ref :: Reference -> Term
ref r = ABT.tm (Ref r)

num :: Double -> Term
num = lit . Number

lit :: Literal -> Term
lit l = ABT.tm (Lit l)

blank :: Term
blank = ABT.tm Blank

app :: Term -> Term -> Term
app f arg = ABT.tm (App f arg)

apps :: Term -> [Term] -> Term
apps f = foldl' app f

ann :: Term -> T.Type -> Term
ann e t = ABT.tm (Ann e t)

vector :: [Term] -> Term
vector es = ABT.tm (Vector (Vector.fromList es))

vector' :: Vector Term -> Term
vector' es = ABT.tm (Vector es)

lam :: ABT.V -> Term -> Term
lam v body = ABT.tm (Lam (ABT.abs v body))

lam' :: [Text] -> Term -> Term
lam' vs body = foldr lam body (map ABT.v' vs)

-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec :: [(ABT.V,Term)] -> Term -> Term
letRec [] e = e
letRec bindings e = ABT.cycle (foldr ABT.abs z (map fst bindings))
  where
    z = ABT.tm (LetRec (map snd bindings) e)

letRec' :: [(Text, Term)] -> Term -> Term
letRec' bs e = letRec [(ABT.v' name, b) | (name,b) <- bs] e

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
let' :: [(ABT.V,Term)] -> Term -> Term
let' bindings e = foldr f e bindings
  where
    f (v,b) body = ABT.tm (Let b (ABT.abs v body))

-- | Satisfies
--   `unLets (letRec bs e) == Just (bs, e, letRec, True)` and
--   `unLets (let' bs e) == Just (bs, e, let', False)`
-- Useful for writing code agnostic to whether a let block is recursive or not.
unLets :: Term -> Maybe ([(ABT.V,Term)], Term, [(ABT.V,Term)] -> Term -> Term, Bool)
unLets e =
  (f letRec True <$> unLetRec e) <|> (f let' False <$> unLet e)
  where f mkLet rec (bs,e) = (bs,e,mkLet,rec)

-- | Satisfies `unLetRec (letRec bs e) == Just (bs, e)`
unLetRec :: Term -> Maybe ([(ABT.V, Term)], Term)
unLetRec (ABT.Cycle' vs (ABT.Tm' (LetRec bs e)))
  | length vs == length vs = Just (zip vs bs, e)
unLetRec _ = Nothing

-- | Satisfies `unLet (let' bs e) == Just (bs, e)`
unLet :: Term -> Maybe ([(ABT.V, Term)], Term)
unLet t = fixup (go t) where
  go (ABT.out -> ABT.Tm (Let b (ABT.Abs' v t))) =
    case go t of (env,t) -> ((v,b):env, t)
  go t = ([], t)
  fixup ([], _) = Nothing
  fixup bst = Just bst

dependencies' :: Term -> Set Reference
dependencies' t = Set.fromList . Writer.execWriter $ ABT.visit' f t
  where f t@(Ref r) = Writer.tell [r] *> pure t
        f t = pure t

dependencies :: Term -> Set Hash
dependencies e = Set.fromList [ h | Reference.Derived h <- Set.toList (dependencies' e) ]

countBlanks :: Term -> Int
countBlanks t = Monoid.getSum . Writer.execWriter $ ABT.visit' f t
  where f Blank = Writer.tell (Monoid.Sum (1 :: Int)) *> pure Blank
        f t = pure t

data PathElement
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda or let
  | Binding !Int -- ^ Points at a particular binding in a let
  | Index !Int -- ^ Points at the index of a vector
  deriving (Eq,Ord,Show)

type Path = [PathElement]

-- | Use a @PathElement@ to compute one step into an @F Term@ subexpression
focus1 :: PathElement -> ABT.Focus1 F Term
focus1 Fn (App f x) = Just (f, \f -> App f x)
focus1 Arg (App f x) = Just (x, \x -> App f x)
focus1 Body (Lam (ABT.Abs' v body)) = Just (body, Lam . ABT.abs v)
focus1 Body (Let b (ABT.Abs' v body)) = Just (body, Let b . ABT.abs v)
focus1 Body (LetRec bs body) = Just (body, LetRec bs)
focus1 (Binding i) (Let b body) | i <= 0 = Just (b, \b -> Let b body)
focus1 (Binding i) (LetRec bs body) =
  listToMaybe (drop i bs)
  >>= \b -> Just (b, \b -> LetRec (take i bs ++ [b] ++ drop (i+1) bs) body)
focus1 (Index i) (Vector vs) =
  vs !? i >>= \v -> Just (v, \v -> Vector (Vector.update vs (Vector.singleton (i,v))))
focus1 _ _ = Nothing

-- | Return the list of all prefixes of the input path
pathPrefixes :: Path -> [Path]
pathPrefixes = inits

-- | Add an element onto the end of this 'Path'
pathExtend :: PathElement -> Path -> Path
pathExtend e p = p ++ [e]

at :: Path -> Term -> Maybe Term
at p t = ABT.at (map focus1 p) t

-- | Given a variable and a path, find the longest prefix of the path
-- which points to a term where the variable is unbound. Example:
-- `\f -> \x -> f {x}` would return the path pointing to `{\x -> f x}`
introducedAt :: ABT.V -> Path -> Term -> Maybe Path
introducedAt v path t = f <$> ABT.introducedAt v (map focus1 path) t where
  f p = take (length p) path

modify :: (Term -> Term) -> Path -> Term -> Maybe Term
modify f p t = ABT.modify f (map focus1 p) t

focus :: Path -> Term -> Maybe (Term, Term -> Term)
focus p t = ABT.focus (map focus1 p) t

parent :: Path -> Maybe Path
parent [] = Nothing
parent p = Just (init p)

parent' :: Path -> Path
parent' = fromMaybe [] . parent

bindingAt :: Path -> Term -> Maybe (ABT.V, Term)
bindingAt [] _ = Nothing
bindingAt path t = do
  parentPath <- parent path
  Let1' v b _ <- at parentPath t
  pure (v, b)

-- | Convert all 'Ref' constructors to the corresponding term
link :: (Applicative f, Monad f) => (Hash -> f Term) -> Term -> f Term
link env e =
  let ds = map (\h -> (h, link env =<< env h)) (Set.toList (dependencies e))
      sub e (h, ft) = replace <$> ft
        where replace t = ABT.replace t ((==) rt) e
              rt = ref (Reference.Derived h)
  in foldM sub e ds

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Term -> Term
betaReduce (App' (Lam' n body) arg) = ABT.subst arg n body
betaReduce e = e

-- mostly boring serialization and hashing code below ...

deriveJSON defaultOptions ''Literal

instance Eq1 F where eq1 = (==)
instance Show1 F where showsPrec1 = showsPrec

deriveJSON defaultOptions ''F
instance J.ToJSON1 F where toJSON1 f = Aeson.toJSON f
instance J.FromJSON1 F where parseJSON1 j = Aeson.parseJSON j


deriveJSON defaultOptions ''PathElement

instance Show Literal where
  show (Text t) = show t
  show (Number n) = show n
  show (Distance d) = show d

instance Show a => Show (F a) where
  showsPrec p fa = go p fa where
    go _ (Lit l) = showsPrec 0 l
    go p (Ann t k) = showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go _ (Lam body) = showParen True (s"λ " <> showsPrec 0 body)
    go _ (Vector vs) = showListWith (showsPrec 0) (Vector.toList vs)
    go _ Blank = s"_"
    go _ (Ref r) = showsPrec 0 r
    (<>) = (.)
    s = showString
