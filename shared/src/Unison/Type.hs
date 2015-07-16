{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type where

import Data.Aeson (toJSON, parseJSON)
import Data.Aeson.TH
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Prelude.Extras (Eq1(..),Show1(..))
import Unison.Note (Noted)
import Unison.Reference (Reference)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Doc as D
import qualified Unison.JSON as J
import qualified Unison.Kind as K
import qualified Unison.Symbol as Symbol

-- | Type literals
data Literal
  = Number
  | Text
  | Vector
  | Distance
  | Ref Reference -- ^ A type literal uniquely defined by some nameless Hash
  deriving (Eq,Ord,Generic)

deriveJSON defaultOptions ''Literal

-- | Base functor for types in the Unison language
data F a
  = Lit Literal
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Constrain a () -- todo: constraint language
  | Forall a
  | Existential a
  | Universal a
  deriving (Eq,Foldable,Functor,Generic1,Traversable)

deriveJSON defaultOptions ''F
instance Eq1 F where (==#) = (==)
instance Show1 F where showsPrec1 = showsPrec

-- | Terms are represented as ABTs over the base functor F.
type Type = ABT.Term F

-- An environment for looking up type references
type Env f = Reference -> Noted f Type

freeVars :: Type -> Set ABT.V
freeVars = ABT.freeVars

data Monotype = Monotype { getPolytype :: Type } deriving (Eq)

instance Show Monotype where
  show = show . getPolytype

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: Type -> Maybe Monotype
monotype t = Monotype <$> ABT.visit isMono t where
  isMono (Forall' _ _) = Just Nothing
  isMono _ = Nothing

-- some smart patterns
pattern Lit' l <- ABT.Tm' (Lit l)
pattern Arrow' i o <- ABT.Tm' (Arrow i o)
pattern Arrows' spine <- (unArrows -> Just spine)
pattern ArrowsP' spine <- (unArrows' -> Just spine)
pattern Ann' t k <- ABT.Tm' (Ann t k)
pattern App' f x <- ABT.Tm' (App f x)
pattern Apps' f args <- (unApps -> Just (f, args))
pattern AppsP' f args <- (unApps' -> Just (f, args))
pattern Constrain' t u <- ABT.Tm' (Constrain t u)
pattern Forall' v body <- ABT.Tm' (Forall (ABT.Abs' v body))
pattern Existential' v <- ABT.Tm' (Existential (ABT.Var' v))
pattern Universal' v <- ABT.Tm' (Universal (ABT.Var' v))

unArrows :: Type -> Maybe [Type]
unArrows t =
  case go t of [] -> Nothing; l -> Just l
  where
    go (Arrow' i o) = i : go o
    go _ = []

unArrows' :: Type -> Maybe [(Type,Path)]
unArrows' t = addPaths <$> unArrows t
  where addPaths ts = ts `zip` arrowPaths (length ts)

unApps :: Type -> Maybe (Type, [Type])
unApps t = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc = go i (o:acc)
  go fn args = fn:args

unApps' :: Type -> Maybe ((Type,Path), [(Type,Path)])
unApps' t = addPaths <$> unApps t
  where
  addPaths (f,args) = case appPaths (length args) of
    (fp,ap) -> ((f,fp), args `zip` ap)

appPaths :: Int -> (Path, [Path])
appPaths numArgs = (fnp, argsp)
  where
  fnp = replicate numArgs Fn
  argsp = take numArgs . drop 1 $ iterate (Fn:) [Arg]

arrowPaths :: Int -> [Path]
arrowPaths spineLength =
  (take (spineLength-1) $ iterate (Output:) [Input]) ++
  [replicate spineLength Output]

matchExistential :: ABT.V -> Type -> Bool
matchExistential v (Existential' x) = x == v
matchExistential _ _ = False

matchUniversal :: ABT.V -> Type -> Bool
matchUniversal v (Universal' x) = x == v
matchUniversal _ _ = False

-- some smart constructors

lit :: Literal -> Type
lit l = ABT.tm (Lit l)

ref :: Reference -> Type
ref = lit . Ref

app :: Type -> Type -> Type
app f arg = ABT.tm (App f arg)

arrow :: Type -> Type -> Type
arrow i o = ABT.tm (Arrow i o)

ann :: Type -> K.Kind -> Type
ann e t = ABT.tm (Ann e t)

forall :: ABT.V -> Type -> Type
forall v body = ABT.tm (Forall (ABT.abs v body))

existential :: ABT.V -> Type
existential v = ABT.tm (Existential (ABT.var v))

universal :: ABT.V -> Type
universal v = ABT.tm (Universal (ABT.var v))

v' :: Text -> Type
v' s = universal (ABT.v' s)

forall' :: [Text] -> Type -> Type
forall' vs body = foldr forall body (map ABT.v' vs)

constrain :: Type -> () -> Type
constrain t u = ABT.tm (Constrain t u)

-- | Bind all free variables with an outer `forall`.
generalize :: Type -> Type
generalize t = foldr forall t $ Set.toList (ABT.freeVars t)

data PathElement
  = Fn -- ^ Points at type in a type application
  | Arg -- ^ Points at the argument in a type application
  | Input -- ^ Points at the left of an `Arrow`
  | Output -- ^ Points at the right of an `Arrow`
  | Body -- ^ Points at the body of a forall
  deriving (Eq,Ord)

type Path = [PathElement]

layout :: (Reference -> ABT.V) -> Type -> D.Doc Text Path
layout ref t = go (0 :: Int) t
  where
  (<>) = D.append
  lit l = case l of
    Number -> "Number"
    Text -> "Text"
    Vector -> "Vector"
    Distance -> "Distance"
    Ref r -> Symbol.name (ref r) -- no infix type operators at the moment
  paren b d =
    let r = D.root d
    in if b then D.embed' r "(" <> d <> D.embed' r ")" else d
  arr = D.breakable " " <> D.embed "→ "
  sp = D.breakable " "
  sym v = D.embed (Symbol.name v)
  go p t = case t of
    Lit' l -> D.embed (lit l)
    ArrowsP' spine ->
      paren (p > 0) . D.group . D.delimit arr $ [ D.sub' p (go 1 s) | (s,p) <- spine ]
    AppsP' (fn,fnP) args ->
      paren (p > 9) . D.group . D.docs $
        [ D.sub' fnP (go 9 fn)
        , D.breakable " "
        , D.nest "  " . D.group . D.delimit sp $ [ D.sub' p (go 10 s) | (s,p) <- args ] ]
    Constrain' t _ -> go p t
    Universal' v -> sym v
    Existential' v -> D.embed ("'" `mappend` Symbol.name v)
    Ann' t k -> go p t -- ignoring kind annotations for now
    Forall' v body -> case p of
      0 -> D.sub Body (go p body)
      _ -> paren True . D.group . D.docs $
             [D.embed "∀ ", sym v, D.embed ".", sp, D.nest "  " $ D.sub Body (go 0 body)]

instance J.ToJSON1 F where
  toJSON1 f = toJSON f

instance J.FromJSON1 F where
  parseJSON1 j = parseJSON j

instance Show Literal where
  show Number = "Number"
  show Text = "Text"
  show Vector = "Vector"
  show Distance = "Distance"
  show (Ref r) = show r

instance Show a => Show (F a) where
  showsPrec p fa = go p fa where
    go _ (Lit l) = showsPrec 0 l
    go p (Arrow i o) =
      showParen (p > 0) $ showsPrec (p+1) i <> s" -> " <> showsPrec p o
    go p (Ann t k) =
      showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go p (Constrain t _) = showsPrec p t
    go _ (Universal v) = showsPrec 0 v
    go _ (Existential v) = s"'" <> showsPrec 0 v
    go p (Forall body) = case p of
      0 -> showsPrec p body
      _ -> showParen True $ s"∀ " <> showsPrec 0 body
    (<>) = (.)
    s = showString
