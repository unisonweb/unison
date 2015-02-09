module Unison.Terms where

import Array
import Elmz.Distance as Distance
import List
import Unison.Term as E
import Unison.Reference as R

-- helper functions for building terms

ap = E.App
builtin s = E.Ref (R.Builtin s)
derived s = E.Ref (R.Derived s)
str s = E.Lit (E.Str s)
int n = E.Lit (E.Number (toFloat n))
vec es = E.Vector (Array.fromList es)

nums : E.Term
nums = vec (List.map int [0..20])

names : E.Term
names = vec (List.map str ["Alice", "Bob", "Burt", "Dave", "Carol"])

rgbTerm : Int -> Int -> Int -> E.Term
rgbTerm r g b =
  builtin "Color.rgba" `ap` int r `ap` int g `ap` int b `ap` int 1

-- expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (E.Builtin "View.cell" `ap` E.Builtin "View.swatch" `ap` rgbTerm 230 126 34))
expr0 = derived "foo" `ap` names `ap` (derived "baz" `ap` int 230 `ap` int 126)
expr = derived "foo" `ap` nums `ap` (derived "baz" `ap` (builtin "View.cell" `ap` builtin "View.swatch" `ap` rgbTerm 230 126 34))
-- this bombs
-- expr = E.Ref "uno" `ap` E.Ref "dos" `ap` E.Ref "tres" `ap` E.Ref "quatro" `ap` E.Ref "cinco" `ap` E.Ref "seis" `ap` E.Ref "siete" `ap` E.Ref "ocho"
-- expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (rgbTerm 230 126 34))

cell f x = builtin "View.cell" `ap` f `ap` x
panel v e = builtin "View.panel" `ap` v `ap` e
function1 f = builtin "View.function1" `ap` f
source e = builtin "View.source" `ap` e
verticalPanel es = panel (builtin "View.vertical") (vec es)
string s = E.Lit (E.Str s)
text s = builtin "View.text" `ap` s
centered s = builtin "View.textbox" `ap` builtin "Text.center" `ap` full `ap` s
h1 s = cell (text E.Blank) (E.Lit (E.Str s))
body s = cell (text E.Blank) (E.Lit (E.Str s))
full = E.Lit (E.Distance (Distance.Fraction 1.0))
