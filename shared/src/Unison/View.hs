{-# LANGUAGE OverloadedStrings #-}

module Unison.View where

import Data.String (IsString(..))
import Data.Text (Text)
import Unison.Doc (Doc)
import qualified Data.Text as Text
import qualified Unison.Doc as D

newtype Arity = Arity Int
newtype Precedence = Precedence Int deriving (Eq,Ord)

high :: Precedence
high = Precedence 10

increase :: Precedence -> Precedence
increase (Precedence p) = Precedence (p + 1)

-- | `Arg 0` references the name of the operator; `Arg 1`
-- references the first argument it is applied to, `Arg 2`
-- the second argument it is applied to, etc.
newtype Var = Arg Int deriving (Eq,Ord)

data Segment = Slot Var Precedence | Text Text

instance IsString Segment where
  fromString s = Text (Text.pack s)

path :: Segment -> [Var]
path (Slot v _) = [v]
path _ = []

text :: Text -> Doc Segment [Var]
text t = D.embed (Text t)

toDoc :: Segment -> Doc Segment [Var]
toDoc t@(Text _) = D.embed t
toDoc s@(Slot var _) = D.embed' [var] s

arg0, arg1, arg2, arg3, arg4 :: Precedence -> Doc Segment [Var]
arg0 p = toDoc $ Slot (Arg 0) p
arg1 p = toDoc $ Slot (Arg 1) p
arg2 p = toDoc $ Slot (Arg 2) p
arg3 p = toDoc $ Slot (Arg 3) p
arg4 p = toDoc $ Slot (Arg 4) p

name :: Doc Segment [Var]
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
  -- todo, might want to generalize to stuff other than Text

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
  layout :: op -> Doc Segment [Var]

  -- | The embedded `Nothing` is where the layout should be placed.
  wrapping :: op -> Doc (Maybe Text) ()

  -- | The precedence of the operator
  precedence :: op -> Precedence

data Rich =
  Rich
    (Doc Segment [Var])
    (Doc (Maybe Text) ())
    Precedence

mixfix :: Precedence -> [Doc Segment [Var]] -> Rich
mixfix prec segs = Rich (D.docs segs) unwrapped prec

unwrapped :: Doc (Maybe Text) ()
unwrapped = D.group (D.embed Nothing)

parens :: Doc (Maybe Text) ()
parens = D.docs [D.embed (Just "("), D.embed Nothing, D.embed (Just ")")]

instance View Rich where
  arity (Rich l _ _) = maximum $ 0 : [ i | Slot (Arg i) _ <- D.elements l ]
  precedence (Rich _ _ p) = p
  wrapping (Rich _ w _) = w
  layout (Rich l _ _) = l
  prefix =
    Rich name unwrapped high
  postfix1 prec =
    Rich (D.docs [arg1 prec, text " ", name]) unwrapped prec
  binary assoc prec =
    Rich layout unwrapped prec
    where
    deltaL p | assoc == AssociateL || assoc == Associative = p
    deltaL p = increase p
    deltaR p | assoc == AssociateR || assoc == Associative = p
    deltaR p = increase p
    layout = D.docs
      [ arg1 $ deltaL prec, D.breakable " ", name, text " "
      , arg2 $ deltaR prec ]
