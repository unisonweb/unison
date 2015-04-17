{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.A_Term where

import Control.Applicative
import Data.Foldable (Foldable)
import Data.Functor.Classes
import Data.Traversable
import Data.Aeson.TH
import Data.Bytes.Serial
import GHC.Generics
import qualified Data.Bytes.Put as Put
import qualified Data.Aeson as Aeson
import qualified Unison.Digest as Digest
import qualified Unison.JSON as J
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as Txt
import qualified Data.Vector as V
import Unison.Var as V
import qualified Unison.Kind as K
import qualified Unison.Distance as Distance
import qualified Unison.A_Hash as H
import qualified Unison.Reference as R
import qualified Unison.A_Type as T
import qualified Unison.ABT as ABT

-- | Literals in the Unison language
data Literal
  = Number Double
  | Text Txt.Text
  | Distance Distance.Distance
  deriving (Eq,Ord,Show,Generic)

deriveJSON defaultOptions ''Literal
instance Serial Literal

-- | Base functor for terms in the Unison language
data F a
  = Lit Literal
  | Blank -- An expression that has not been filled in, has type `forall a . a`
  | Ref R.Reference
  | App a a
  | Ann a T.Type
  | Vector (V.Vector a)
  | Lam a
  | LetRec [a] a --
  | Let [a] a    -- no fwd refs allowed
  deriving (Eq,Foldable,Functor,Generic1)

instance Eq1 F
instance Serial1 F
instance Serial1 V.Vector where
  serializeWith f vs = serializeWith f (V.toList vs)
  deserializeWith v = V.fromList <$> deserializeWith v

deriveJSON defaultOptions ''F

-- | Terms are represented as ABTs over the base functor F.
type Term = ABT.Term F

-- some smart constructors

lit :: Literal -> Term
lit l = ABT.tm (Lit l)

blank :: Term
blank = ABT.tm Blank

app :: Term -> Term -> Term
app f arg = ABT.tm (App f arg)

ann :: Term -> T.Type -> Term
ann e t = ABT.tm (Ann e t)

vector :: [Term] -> Term
vector es = ABT.tm (Vector (V.fromList es))

vector' :: V.Vector Term -> Term
vector' es = ABT.tm (Vector es)

lam :: ABT.V -> Term -> Term
lam v body = ABT.tm (Lam (ABT.abs v body))

-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec :: [(ABT.V,Term)] -> Term -> Term
letRec bindings e =
  ABT.tm (LetRec (map (intro . snd) bindings) (intro e))
  where
    -- each e is wrapped in N abs introductions for each binding in block
    intro e = foldr ABT.abs e (map fst bindings)

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
let' :: [(ABT.V,Term)] -> Term -> Term
let' bindings e =
  ABT.tm (Let (map intro (zip [0..] bindings)) (introAll bindings e))
  where
    -- each e is wrapped in introduction of all variables declared at a previous
    -- bindings in the block
    intro (ind, (_, e)) = introAll (take ind bindings) e
    introAll bindings e = foldr ABT.abs e (map fst bindings)

instance Digest.Digestable1 F where
  digest1 s hash e = case e of
    Lit l -> Digest.run $ Put.putWord8 0 *> serialize l
    Blank -> Digest.run $ Put.putWord8 1
    Ref r -> Digest.run $ Put.putWord8 2
  --App a a
  --Ann a T.Type
  --Vector (V.Vector a)
  --Lam a
  --LetRec [a] a --
  --Let [a] a    -- no fwd refs allowed

instance J.ToJSON1 F where
  toJSON1 f = Aeson.toJSON f

instance J.FromJSON1 F where
  parseJSON1 j = Aeson.parseJSON j

