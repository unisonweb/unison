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

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Prelude.Extras (Eq1(..),Show1(..))
import Unison.Doc (Doc)
import Unison.Hashable (Hashable, Hashable1)
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol(..))
import Unison.Var (Var)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Dimensions as Dimensions
import qualified Unison.Doc as D
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as Hashable
import qualified Unison.JSON as J
import qualified Unison.Kind as K
import qualified Unison.Reference as Reference
import qualified Unison.Symbol as Symbol
import qualified Unison.Var as Var
import qualified Unison.View as View

-- | Type literals
data Literal
  = Number
  | Text
  | Vector
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

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v = AnnotatedType v ()

-- | Like `Type v`, but with an annotation of type `a` at every level in the tree
type AnnotatedType v a = ABT.Term F v a

-- An environment for looking up type references
type Env f v = Reference -> Noted f (Type v)

freeVars :: Type v -> Set v
freeVars = ABT.freeVars

data Monotype v = Monotype { getPolytype :: Type v } deriving (Eq)

instance Var v => Show (Monotype v) where
  show = show . getPolytype

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: Ord v => Type v -> Maybe (Monotype v)
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
pattern ForallsP' vs body <- (unForalls' -> Just (vs, body))
pattern Existential' v <- ABT.Tm' (Existential (ABT.Var' v))
pattern Universal' v <- ABT.Tm' (Universal (ABT.Var' v))

unArrows :: Type v -> Maybe [Type v]
unArrows t =
  case go t of [] -> Nothing; l -> Just l
  where
    go (Arrow' i o) = i : go o
    go _ = []

unArrows' :: Type v -> Maybe [(Type v,Path)]
unArrows' t = addPaths <$> unArrows t
  where addPaths ts = ts `zip` arrowPaths (length ts)

unForalls' :: Type v -> Maybe ([(v, Path)], (Type v, Path))
unForalls' (Forall' v body) = case unForalls' body of
  Nothing -> Just ([(v, [])], (body, [Body])) -- todo, need a path for forall vars
  Just (vs, (body,bodyp)) -> Just ((v, []) : vs, (body, Body:bodyp))
unForalls' _ = Nothing

unApps :: Type v -> Maybe (Type v, [Type v])
unApps t = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc = go i (o:acc)
  go fn args = fn:args

unApps' :: Type v -> Maybe ((Type v,Path), [(Type v,Path)])
unApps' t = addPaths <$> unApps t
  where
  addPaths (f,args) = case appPaths (length args) of
    (fp,ap) -> ((f,fp), args `zip` ap)

appPaths :: Int -> (Path, [Path])
appPaths numArgs = (fnp, argsp)
  where
  fnp = replicate numArgs Fn
  argsp = reverse . take numArgs $ iterate (Fn:) [Arg]

arrowPaths :: Int -> [Path]
arrowPaths spineLength =
  (take (spineLength-1) $ iterate (Output:) [Input]) ++
  [replicate spineLength Output]

matchExistential :: Eq v => v -> Type v -> Bool
matchExistential v (Existential' x) = x == v
matchExistential _ _ = False

matchUniversal :: Eq v => v -> Type v -> Bool
matchUniversal v (Universal' x) = x == v
matchUniversal _ _ = False

-- some smart constructors

lit :: Ord v => Literal -> Type v
lit l = ABT.tm (Lit l)

vector :: Ord v => Type v
vector = lit Vector

vectorOf :: Ord v => Type v -> Type v
vectorOf t = vector `app` t

ref :: Ord v => Reference -> Type v
ref = lit . Ref

app :: Ord v => Type v -> Type v -> Type v
app f arg = ABT.tm (App f arg)

arrow :: Ord v => Type v -> Type v -> Type v
arrow i o = ABT.tm (Arrow i o)

ann :: Ord v => Type v -> K.Kind -> Type v
ann e t = ABT.tm (Ann e t)

forall :: Ord v => v -> Type v -> Type v
forall v body = ABT.tm (Forall (ABT.abs v body))

existential :: Ord v => v -> Type v
existential v = ABT.tm (Existential (ABT.var v))

universal :: Ord v => v -> Type v
universal v = ABT.tm (Universal (ABT.var v))

v' :: Var v => Text -> Type v
v' s = universal (ABT.v' s)

forall' :: Var v => [Text] -> Type v -> Type v
forall' vs body = foldr forall body (map ABT.v' vs)

constrain :: Ord v => Type v -> () -> Type v
constrain t u = ABT.tm (Constrain t u)

-- | Bind all free variables with an outer `forall`.
generalize :: Ord v => Type v -> Type v
generalize t = foldr forall t $ Set.toList (ABT.freeVars t)

data PathElement
  = Fn -- ^ Points at type in a type application
  | Arg -- ^ Points at the argument in a type application
  | Input -- ^ Points at the left of an `Arrow`
  | Output -- ^ Points at the right of an `Arrow`
  | Body -- ^ Points at the body of a forall
  deriving (Eq,Ord,Show)

type Path = [PathElement]

type ViewableType = Type (Symbol View.DFO)

defaultSymbol :: Reference -> Symbol View.DFO
defaultSymbol (Reference.Builtin t) = Symbol.prefix t
defaultSymbol (Reference.Derived h) = Symbol.prefix (Text.cons '#' $ short h)
  where
  short h = Text.take 8 . Hash.base64 $ h

toString :: ViewableType -> String
toString t = D.formatText (Dimensions.Width 80) (view defaultSymbol t)

view :: (Reference -> Symbol View.DFO) -> ViewableType -> Doc Text Path
view ref t = go no View.low t
  where
  no = const False
  sym v = D.embed (Var.name v)
  op :: ViewableType -> Symbol View.DFO
  op t = case t of
    Lit' (Ref r) -> ref r
    Lit' l -> Symbol.annotate View.prefix . (\r -> Symbol.prefix r :: Symbol ()) . Text.pack . show $ l
    Universal' v -> v
    Existential' v -> v
    _ -> Symbol.annotate View.prefix (Symbol.prefix "" :: Symbol ())
  go :: (ViewableType -> Bool) -> View.Precedence -> ViewableType -> Doc Text Path
  go inChain p t = case t of
    ArrowsP' spine ->
      let arr = D.breakable " " `D.append` D.embed "→ "
      in D.parenthesize (p > View.low) . D.group . D.delimit arr $
          [ D.sub' p (go no (View.increase View.low) s) | (s,p) <- spine ]
    AppsP' (fn,fnP) args ->
      let
        Symbol _ name view = op fn
        (taken, remaining) = splitAt (View.arity view) args
        fmt (child,path) = (\p -> D.sub' path (go (fn ==) p child), path)
        applied = fromMaybe unsaturated (View.instantiate view fnP name (map fmt taken))
        unsaturated = D.sub' fnP $ go no View.high fn
      in
        (if inChain fn then id else D.group) $ case remaining of
          [] -> applied
          args -> D.parenthesize (p > View.high) . D.group . D.docs $
            [ applied, D.breakable " "
            , D.nest "  " . D.group . D.delimit (D.breakable " ") $
              [ D.sub' p (go no (View.increase View.high) s) | (s,p) <- args ] ]
    ForallsP' vs (body,bodyp) ->
      if p == View.low then D.sub' bodyp (go no p body)
      else D.parenthesize True . D.group $
           D.embed "∀ " `D.append`
           D.delimit (D.embed " ") (map (sym . fst) vs) `D.append`
           D.docs [D.embed ".", D.breakable " ", D.nest "  " $ D.sub' bodyp (go no View.low body)]
    Constrain' t _ -> go inChain p t
    Ann' t _ -> go inChain p t -- ignoring kind annotations for now
    Universal' v -> sym v
    Existential' v -> D.embed ("'" `mappend` Var.name v)
    Lit' _ -> D.embed (Var.name $ op t)
    _ -> error $ "layout match failure"

instance Hashable Literal where
  tokens l = case l of
    Number -> [Hashable.Tag 0]
    Text -> [Hashable.Tag 1]
    Vector -> [Hashable.Tag 2]
    Ref (Reference.Builtin name) -> Hashable.Tag 3 : Hashable.tokens name
    Ref (Reference.Derived h) -> [Hashable.Tag 4, Hashable.Hashed (Hashable.fromBytes (Hash.toBytes h))]

instance Hashable1 F where
  hash1 _ hash e =
    let
      (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      hashToken :: (Hashable.Hash h, Hashable t) => t -> Hashable.Token h
      hashToken = Hashable.Hashed . Hashable.hash'
    in Hashable.hash $ tag 0 : case e of
      Lit l -> [tag 0, hashToken l]
      Arrow a b -> [tag 1, hashed (hash a), hashed (hash b) ]
      App a b -> [tag 2, hashed (hash a), hashed (hash b) ]
      Ann a k -> [tag 3, hashed (hash a), hashToken k ]
      Constrain a u -> [tag 4, hashed (hash a), hashToken u]
      Forall a -> [tag 5, hashed (hash a)]
      Existential v -> [tag 6, hashed (hash v)]
      Universal v -> [tag 7, hashed (hash v)]

deriveJSON defaultOptions ''PathElement

instance J.ToJSON1 F where
  toJSON1 f = toJSON f

instance J.FromJSON1 F where
  parseJSON1 j = parseJSON j

instance Show Literal where
  show Number = "Number"
  show Text = "Text"
  show Vector = "Vector"
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
