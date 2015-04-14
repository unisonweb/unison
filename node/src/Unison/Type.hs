{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Type where

import Control.Applicative
import Control.Lens
import Data.Aeson.TH
import qualified Data.Aeson.Encode as JE
import qualified Data.List as L
import qualified Data.Set as S
import Unison.Note as N
import qualified Unison.Hash as H
import qualified Unison.Kind as K
import qualified Unison.Var as V
import qualified Unison.Reference as R

-- constructor is private not exported
data Monotype = Monotype { getPolytype :: Type } deriving (Eq,Ord)
instance Show Monotype where
  show (Monotype t) = show t

-- An environment for looking up type references
type Env f = R.Reference -> Noted f Type

-- | Type literals
data Literal
  = Number
  | Text
  | Vector
  | Distance
  | Ref R.Reference -- ^ A type literal uniquely defined by some nameless Hash
  deriving (Eq,Ord,Show)

-- | Types in the Unison language
data Type
  = Unit Literal
  | Arrow Type Type
  | Universal V.Var
  | Existential V.Var
  | Ann Type K.Kind
  | App Type Type
  | Constrain Type () -- todo: constraint language
  | Forall V.Var Type
  deriving (Eq,Ord)

instance Show Type where
  show (Unit l) = show l
  show (Arrow (Arrow i i2) o) = "(" ++ show i ++ " -> " ++ show i2 ++ ") -> " ++ show o
  show (Arrow i o) = show i ++ " -> " ++ show o
  show (Universal n) = show n
  show (Existential n) = "'" ++ show n
  show (Ann t k) = show t ++ ":" ++ show k
  show (App f arg) = "(" ++ show f ++ " " ++ show arg ++ ")"
  show (Constrain t _) = show t
  show (Forall x (Forall y (Forall z t))) =
    "(∀ " ++ (L.intercalate " " . map show) [x,y,z] ++ ". " ++ show t ++ ")"
  show (Forall x (Forall y t)) = "(∀ " ++ (L.intercalate " " . map show) [x,y] ++ ". " ++ show t++")"
  show (Forall x t) = "(∀ " ++ show x ++ ". " ++ show t++")"

monotype :: Type -> Maybe Monotype
monotype t = Monotype <$> go t where
  go (Unit l) = pure (Unit l)
  go (Arrow i o) = Arrow <$> go i <*> go o
  go (Universal v) = pure (Universal v)
  go (Existential v) = pure (Existential v)
  go (Ann t' k) = Ann <$> go t' <*> pure k
  go (Constrain t' c) = Constrain <$> go t' <*> pure c
  go _ = Nothing

maxV :: Type -> V.Var
maxV t = go t where
  go (Unit _) = bot
  go (Universal _) = bot
  go (Existential _) = bot
  go (Arrow i o) = go i `max` go o
  go (Ann t _) = go t
  go (App f arg) = go f `max` go arg
  go (Constrain t _) = go t
  go (Forall n _) = n
  bot = V.decr V.bound1

-- | HOAS syntax for `Forall` constructor:
-- `forall1 $ \x -> Arrow x x`
forall1 :: (Type -> Type) -> Type
forall1 f = Forall n body where
  body = f (Universal n)
  n = V.succ (maxV body)

-- | Apply a function underneath a @Forall@
under :: Type -> Maybe ((Type -> Type) -> Type)
under (Forall n body) = Just $ \f -> forall1 $ \x -> f (subst body n x)
under _ = Nothing

-- | HOAS syntax for `Forall` constructor,
-- `exists1 $ \x -> Arrow x x`
exists1 :: (Type -> Type) -> Type
exists1 f = Forall n body where
  body = f (Existential n)
  n = V.succ (maxV body)

-- | HOAS syntax for `Forall` constructor:
-- | HOAS syntax for `Forall` constructor:
-- `forall2 $ \x y -> Arrow (Arrow x y) (Arrow x y)`
forall2 :: (Type -> Type -> Type) -> Type
forall2 f = forall1 $ \x -> forall1 $ \y -> f x y

-- | HOAS syntax for `Forall` constructor:
-- `forall3 $ \x y z -> Arrow (Arrow x y z) (Arrow x y z)`
forall3 :: (Type -> Type -> Type -> Type) -> Type
forall3 f = forall1 $ \x -> forall1 $ \y -> forall1 $ \z -> f x y z

-- | mnemonic `subst fn var=arg`
subst :: Type -> V.Var -> Type -> Type
subst fn var arg = case fn of
  Unit l -> Unit l
  Arrow i o -> Arrow (subst i var arg) (subst o var arg)
  Universal v | v == var -> arg
              | otherwise -> fn
  Existential v | v == var -> arg
                | otherwise -> fn
  Ann fn' t -> Ann (subst fn' var arg) t
  App x y -> App (subst x var arg) (subst y var arg)
  Constrain fn' t -> Constrain (subst fn' var arg) t
  Forall v fn' -> Forall v (subst fn' var arg)

-- | The set of unbound variables in this type
freeVars :: Type -> S.Set V.Var
freeVars t = case t of
  Unit _ -> S.empty
  Arrow i o -> S.union (freeVars i) (freeVars o)
  Universal v -> S.singleton v
  Existential v -> S.singleton v
  Ann fn _ -> freeVars fn
  App x y -> S.union (freeVars x) (freeVars y)
  Constrain fn _ -> freeVars fn
  Forall v fn -> S.delete v (freeVars fn)

-- | Remove any stray quantifiers
gc :: Type -> Type
gc t = go (relevant t) t
  where
    relevant (Forall _ t) = relevant t
    relevant t = freeVars t
    go relevant (Forall v body) | S.member v relevant = Forall v (go relevant body)
    go relevant (Forall v body) | otherwise = go relevant body
    go _ t = t

hash :: Type -> H.Digest
hash = H.lazyBytes . JE.encode

finalizeHash :: Type -> H.Hash
finalizeHash = H.finalize . hash

hashes :: [Type] -> H.Hash
hashes _ = error "todo: Type.hashes"

makePrisms ''Literal
makePrisms ''Type

deriveJSON defaultOptions ''Literal
deriveJSON defaultOptions ''Type
