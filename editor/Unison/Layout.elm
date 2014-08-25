module Unison.Layout where

import Either(..)
import Graphics.Element as E
import Graphics.Element (Element)

type Pt = { x : Int, y : Int }

type Layout env a = env -> (Element, Pt -> a)

element : Element -> Layout {} ()
element e _ = (e, \_ -> ())

above : Layout env a -> Layout env b -> Layout env (Either a b)
above top bottom env =
  let (etop, ra) = top env
      (ebot, rb) = bottom env
      resolve pt = if pt.y <= heightOf etop
                   then Left (ra pt)
                   else Right (rb pt)
  in (flow down [etop, ebot], resolve)

beside : Layout env a -> Layout env b -> Layout env (Either a b)
beside l r env =
  let (el, ra) = l env
      (er, rb) = r env
      resolve pt = if pt.x <= widthOf el
                   then Left (ra pt)
                   else Right (rb pt)
  in (flow right [el, er], resolve)

map : (a -> b) -> Layout env a -> Layout env b
map f l env = case l env of (e, r) -> (e, \pt -> f (r pt))

-- horizontal : [Layout env a] -> Layout env (Maybe (Int,a))

