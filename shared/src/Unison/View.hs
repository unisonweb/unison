{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.View where

import Data.Aeson.TH
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Unison.Doc (Doc)
import Unison.Path (Path)
import qualified Data.Text as Text
import qualified Unison.Doc as D
import qualified Unison.Path as Path

newtype Precedence = Precedence Int deriving (Eq,Ord,Generic)

low :: Precedence
low = Precedence 0

high :: Precedence
high = Precedence 10

increase :: Precedence -> Precedence
increase (Precedence p) = Precedence (p + 1)

-- | `Arg 0` references the name of the operator; `Arg 1`
-- references the first argument it is applied to, `Arg 2`
-- the second argument it is applied to, etc.
newtype Var = Arg Int deriving (Eq,Ord,Generic)

data Segment = Slot Var Precedence | Text Text deriving (Generic)

instance IsString Segment where
  fromString s = Text (Text.pack s)

path :: Segment -> Maybe Var
path (Slot v _) = Just v
path _ = Nothing

space :: Doc Segment (Maybe Var)
space = D.delimiter (Text " ")

toDoc :: Segment -> Doc Segment (Maybe Var)
toDoc t@(Text _) = D.delimiter t
toDoc s@(Slot var _) = D.embed' (Just var) s

arg0, arg1, arg2, arg3, arg4 :: Precedence -> Doc Segment (Maybe Var)
arg0 p = toDoc $ Slot (Arg 0) p
arg1 p = toDoc $ Slot (Arg 1) p
arg2 p = toDoc $ Slot (Arg 2) p
arg3 p = toDoc $ Slot (Arg 3) p
arg4 p = toDoc $ Slot (Arg 4) p

name :: Doc Segment (Maybe Var)
name = toDoc $ Slot (Arg 0) high

-- | The associativity of a binary operator, which controls how / whether
-- parentheses or other indicators of grouping are displayed in chains
-- of operators.
--
--   * `AssociateL` on an operator, `+`, indicates that `(a + b) + c`
--      may be displayed without parens, as `a + b + c`. `a + (b + c)`
--      should display with parentheses.
--   * `AssociateR` on an operator, `::`, indicates that `a :: (b :: c)`
--      may be displayed without parens, as `a :: b :: c`. `(a :: b) :: c`
--      should display with parentheses.
--   * `Associative` on an operator, `*`, indicates that chains of that
--      operator should always display without parens. So a syntax tree of
--      `a * (b * c)` and `(a * b) * c` would display as `a * b * c`.
--   * `None` indicates that all grouping of the syntax tree should be
--      indicated visually. So `a ++ (b ++ c)` and `(a ++ b) ++ c` would
--      both display with parens if `++` had `None` as its associativity.
--
data Associativity = AssociateL | AssociateR | Associative | None deriving (Eq,Ord)

class View op where
  -- | A prefix operator, of arity 0. This is the only arity 0 operator.
  prefix :: op

  -- | A postfix operator, of arity 1.
  postfix1 :: Precedence -> op

  -- | A binary operator, of arity 2.
  binary :: Associativity -> Precedence -> op

  -- | All operators have an arity, which is the number of subsequent
  -- applications they capture for insertion into their layout.
  -- `arity prefix == 0` and `arity (binary assoc prec) == 2`.
  arity :: op -> Int

  -- | The layout is an arbitrary `Doc`, which can refer to both
  -- the operator's name, and any arguments captured by its arity.
  -- An unsaturated operator (applied to fewer than `arity` arguments)
  -- gets displayed as `prefix`.
  layout :: op -> Doc Segment (Maybe Var)

  -- | The precedence of the operator
  precedence :: op -> Precedence

data DFO = DFO (Doc Segment (Maybe Var)) Precedence deriving Generic

mixfix :: Precedence -> [Doc Segment (Maybe Var)] -> DFO
mixfix prec segs = DFO (D.docs segs) prec

instance View () where
  arity _ = 0
  precedence _ = high
  layout _ = name
  prefix = ()
  postfix1 _ = ()
  binary _ _ = ()

instance View DFO where
  arity (DFO l _) = maximum $ 0 : [ i | Slot (Arg i) _ <- D.elements l ]
  precedence (DFO _ p) = p
  layout (DFO l _) = l
  prefix = DFO name high
  postfix1 prec = DFO (D.docs [arg1 prec, space, name]) prec
  binary assoc prec =
    DFO layout prec
    where
    deltaL p | assoc == AssociateL || assoc == Associative = p
    deltaL p = increase p
    deltaR p | assoc == AssociateR || assoc == Associative = p
    deltaR p = increase p
    layout = D.docs
      [ arg1 (deltaL prec), D.breakable " ", name, space
      , arg2 (deltaR prec) ]

instantiate :: (Path p, View op) => op -> p -> Text -> [(Precedence -> Doc Text p, p)] -> Maybe (Doc Text p)
instantiate op opP name args | arity op == length args =
  D.ebind f (fmap g (layout op))
  where
  f (Slot (Arg 0) _) = D.embed name
  f (Slot (Arg i) prec) = let (a,_) = args !! (i - 1) in a prec
  f (Text t) = D.delimiter t
  g Nothing = Path.root
  g (Just (Arg 0)) = opP
  g (Just (Arg i)) = snd $ args !! (i - 1)
instantiate _ _ _ _ = Nothing

-- boring serialization code

deriveJSON defaultOptions ''Precedence
deriveJSON defaultOptions ''Var
deriveJSON defaultOptions ''Segment
deriveJSON defaultOptions ''DFO
