module Unison.View (Env, env0, key, l0, layout, layout', literalKey, L, reactivePaths) where

import Array
import Color
import Debug
import Elmz.Distance as Distance
import Elmz.Layout exposing (Layout)
import Elmz.Layout as L
import Elmz.Moore exposing (Moore(..))
import Elmz.Moore as Moore
import Elmz.Trie as Trie
import Elmz.Trie exposing (Trie)
import List
import Graphics.Element as E
import Maybe
import Unison.Reference as R
import Unison.Hash exposing (Hash)
import Unison.Metadata exposing (Metadata)
import Unison.Symbol exposing (Fixity)
import Unison.Symbol as Symbol
import Unison.Metadata as Metadata
import Unison.Styles exposing (codeText)
import Unison.Styles as Styles
import Unison.Term exposing (..)
import Unison.Term as Term
import Unison.Type as Type
import Unison.Path exposing (..)
import Unison.Path as Path
import String
import Text
type alias E = Path.E

type alias L = { path : Path, selectable : Bool }

l0 : L
l0 = { path = [], selectable = False }

type alias Env =
  { availableWidth : Int
  , metadata       : R.Reference -> Metadata
  , overrides      : Path -> Maybe Term
  , raw            : Maybe Path
  }

env0 : Env
env0 =
  { availableWidth = 1024
  , metadata = Metadata.defaultMetadata
  , overrides = always Nothing
  , raw = Nothing }

type alias Cur = { path : Path, term : Term }

literalKey : Term -> Maybe String
literalKey e = case e of
  Lit (Number n) -> Just <| toString n
  Lit (Text s) -> Just <| toString s
  Lit (Distance d) -> Just <| toString d
  Blank -> Just "_"
  _ -> Nothing

key : { tl | metadata : R.Reference -> Metadata }
   -> Cur
   -> String
key env cur = case cur.term of
  Blank -> "_"
  Var v -> v.name
  Lit (Number n) -> toString n
  Lit (Text s) -> toString s
  Lit (Distance d) -> toString d
  Ref r -> Metadata.firstName "anonymous" (env.metadata r)
  App f arg -> key env { cur | path <- cur.path `snoc` Fn, term <- f } ++
               key env { cur | path <- cur.path `snoc` Arg, term <- arg }
  Ann e t -> key env { cur | term <- e }
  Vector terms ->
    let ki i term = key env { cur | path <- cur.path `snoc` Index i, term <- term }
    in "[" ++ String.join "," (Array.toList (Array.indexedMap ki terms)) ++ "]"
  Lam n body -> n.name ++ " -> " ++
                key env { path = cur.path `snoc` Body
                        , term = body }
todo : a
todo = todo

tag path = { path = path, selectable = True }
utag path = { path = path, selectable = False }

space = codeText " "
spaces n =
  if n <= 0 then E.empty else codeText (String.padLeft (n*2) ' ' "")
space2 = codeText "  "

indentWidth = E.widthOf space2

paren : Bool -> Cur -> Layout L -> Layout L
paren parenthesize cur e =
  if parenthesize
  then let t = tag cur.path
           opening = L.embed t (codeText "(")
           closing = L.embed t (codeText ")")
           botY = L.heightOf e - L.heightOf closing
           topOpen = L.container t (L.widthOf opening) (L.heightOf e) (L.Pt 0 0) opening
           bottomClose = L.container t (L.widthOf opening) (L.heightOf e) (L.Pt 0 botY) closing
       in L.horizontal t [topOpen, e, bottomClose]
  else e

layout : Term -- term to render
      -> Env
      -> Layout L
layout expr env =
  impl env True 0 env.availableWidth { path = [], term = expr }

layout' : Env -> Cur -> Layout L
layout' env cur = impl env True 0 env.availableWidth cur

impl : Env
    -> Bool
    -> Int
    -> Int
    -> Cur
    -> Layout { path : Path, selectable : Bool }
impl env allowBreak ambientPrec availableWidth cur =
  case env.overrides cur.path of
    Just l -> let o p = if p == cur.path then Nothing else env.overrides p
              in impl { env | overrides <- o }
                      allowBreak
                      ambientPrec
                      availableWidth
                      { cur | term <- l }
    Nothing -> case cur.term of
      Embed l -> l
      Var v -> codeText ("v" ++ toString v) |> L.embed (tag cur.path)
      Ref h -> codeText (Metadata.firstName (R.toKey h) (env.metadata h)) |> L.embed (tag cur.path)
      Blank -> Styles.blank |> L.embed (tag cur.path)
      Lit (Number n) -> Styles.numericLiteral (toString n) |> L.embed (tag cur.path)
      Lit (Text s) -> Styles.stringLiteral ("\"" ++ s ++ "\"") |> L.embed (tag cur.path)
      Ann e t -> let ann = Styles.codeText (" : " ++ Type.key env t)
                 in L.beside (tag cur.path)
                             (impl env allowBreak 9 (availableWidth - E.widthOf ann)
                               { cur | term <- e })
                             (L.embed (tag cur.path) ann)
      Lam n body ->
        let space' = L.embed (tag cur.path) space
            arg = codeText (n.name) |> L.embed (tag cur.path)
            nested = case body of
              Lam _ _ -> True
              _ -> False
            argLayout = [arg]
                     ++ (if nested then [] else [L.embed (tag cur.path) (codeText "→")])
                     |> L.intersperseHorizontal space'
            cur' = { cur | term <- body, path <- cur.path `snoc` Body }
            unbroken = [ argLayout, impl env False 0 0 cur' ]
                    |> L.intersperseHorizontal space'
                    |> paren (ambientPrec > 0) cur
        in if not allowBreak || L.widthOf unbroken < availableWidth
           then unbroken
           else L.above (tag cur.path)
                  argLayout
                  (L.horizontal (tag cur.path)
                    [ space'
                    , space'
                    , impl env True 0 (availableWidth - indentWidth) cur' ])
                |> paren (ambientPrec > 0) cur
      _ -> case (Maybe.withDefault True <| Maybe.map (\p -> not (Path.startsWith cur.path p)) env.raw,
                 builtins env allowBreak ambientPrec availableWidth cur) of
        (True, Just l) -> l
        _ -> let space' = L.embed (tag cur.path) space in
          case break env cur of
            Prefix f args ->
              let f' = impl env False 9 availableWidth f
                  lines = f' :: List.map (impl env False 10 0) args
                  unbroken = L.intersperseHorizontal space' lines
                          |> paren (ambientPrec > 9) cur
              in if not allowBreak || L.widthOf unbroken < availableWidth
                 then unbroken
                 else let args' = List.map (impl env True 10 (availableWidth - L.widthOf f' - L.widthOf space')) args
                               |> L.vertical (tag cur.path)
                      in L.intersperseHorizontal space' [f',args']
                      |> paren (ambientPrec > 9) cur
            Operators leftAssoc prec hd tl ->
              let f (op,r) l = L.intersperseHorizontal space' [ l, impl env False 10 0 op, impl env False rprec 0 r ]
                  unbroken = List.foldl f (impl env False lprec 0 hd) tl
                          |> paren (ambientPrec > 9) cur
                  lprec = if leftAssoc then prec else 1+prec
                  rprec = if leftAssoc then 1+prec else prec
                  bf (op,r) l =
                    let op' = impl env False 10 0 op
                        remWidth = availableWidth - L.widthOf op' - L.widthOf space'
                    in L.above (tag cur.path) l <|
                       L.intersperseHorizontal space' [op', impl env True rprec remWidth r ]
              in if not allowBreak || L.widthOf unbroken < availableWidth
                 then unbroken
                 else List.foldl bf (impl env True lprec (availableWidth - indentWidth) hd) tl
                      |> paren (ambientPrec > 9) cur
            Bracketed es ->
              let unbroken = Styles.cells
                    (tag cur.path)
                    (codeText "[]")
                    (List.map (impl env False 0 0) es)
              in if not allowBreak || L.widthOf unbroken < availableWidth || List.length es < 2
              then unbroken
              else Styles.verticalCells (tag cur.path) (codeText "[]")
                                        (List.map (impl env True 0 (availableWidth - 4)) es) -- account for cell border

type Break a
  = Prefix a (List a)          -- `Prefix f [x,y,z] == f x y z`
  | Operators Bool Int a (List (a,a)) -- `Operators False x [(+,y), (+,z)] == (x + y) + z`
                                 -- `Operators True x [(^,y), (^,z)] == x ^ (y ^ z)`
  | Bracketed (List a)        -- `Bracketed [x,y,z] == [x,y,z]`
  | Lambda (List a) a          -- `Lambda [x,y,z] e == x -> y -> z -> e`

break : Env -> Cur -> Break Cur
break env cur =
  let prefix f acc path = case f of
        App f arg -> prefix f ({ cur | path <- path `snoc` Arg, term <- arg } :: acc)
                              (path `snoc` Fn)
        _ -> Prefix { cur | path <- path, term <- f } acc
      opsL o prec e acc path = case e of
        App (App op l) r ->
          if op == o
          then
            let hd = (
              { cur | path <- path `append` [Fn,Fn], term <- op },
              { cur | path <- path `snoc` Arg, term <- r })
            in opsL o prec l (hd :: acc) (path `append` [Fn,Arg])
          else Operators False prec { cur | path <- path, term <- e } acc
        _ -> Operators False prec { cur | path <- path, term <- e } acc
      opsR o prec e path = case e of
        App (App op l) r ->
          if op == o
          then case opsR o prec r (path `snoc` Arg) of
            Operators _ prec hd tl ->
              let tl' = ({ cur | path <- path `append` [Fn,Fn], term <- op }, hd) :: tl
              in Operators True prec { cur | path <- path `append` [Fn,Arg], term <- l } tl'
          else Operators True prec { cur | path <- path, term <- e } []
        _ -> Operators True prec { cur | path <- path, term <- e } []
  in case cur.term of
    Vector xs -> Array.toList xs
              |> List.indexedMap (\i a -> { cur | path <- cur.path `snoc` Index i, term <- a })
              |> Bracketed
    App (App op l) r ->
      let sym = case op of
        Ref h -> Metadata.firstSymbol (R.toKey h) (env.metadata h)
        Var v -> Symbol.prefix ("v" ++ toString v)
        _ -> Symbol.anonymous
      in case sym.fixity of
        Symbol.Prefix -> prefix (App (App op l) r) [] cur.path -- not an operator chain, fall back
        Symbol.InfixL -> opsL op sym.precedence (App (App op l) r) [] cur.path -- left associated operator chain
        Symbol.InfixR -> opsR op sym.precedence (App (App op l) r) cur.path
    _ -> prefix cur.term [] cur.path

-- denotes a function a -> Layout
{-

panel (f p q r) x evaluates x, and any arguments to `f` (p, q, r)

hide : View a
spacer : Relative -> Absolute -> View ()
color : Color -> View Panel
palette : View Color
rgb : Int -> Int -> Int -> Color
source : View a
text : Style -> View String
textbox : Alignment -> Distance -> Style -> View String
reactive : View a -> View a
fn : (Panel -> Panel) -> View (a -> b)
cell (fn f)
horizontal : View [Panel]
wrap : View [Panel]
vertical : View [Panel]
fit-width : Distance -> View a -> View a
container : Distance -> Distance -> (Distance,Distance) -> View a ->

-- set amount of padding size of top,right,bottom,left
pad : Distance -> Distance -> Distance -> Distance -> View a -> View a
view : View Panel
panel : View a -> a -> Panel
cell : View a -> a -> a
Text.{left, right, center, justify} : Alignment

cell vertical [
  cell source "hello",
  cell source (1 + 23)
]
panel view (panel blah x)
-}

-- eventually, this should return a list of paths needing evaluation
-- Flow a = Int -> Layout a

builtins : Env -> Bool -> Int -> Int -> Cur -> Maybe (Layout L)
builtins env allowBreak availableWidth ambientPrec cur =
  let
    t = tag (cur.path `snoc` Arg)
    go v e = case v of
      App (Ref (R.Builtin "View.color")) c -> case c of
        App (App (App (App (Ref (R.Builtin "Color.rgba")) (Lit (Number r))) (Lit (Number g))) (Lit (Number b))) (Lit (Number a)) ->
          let c' = Color.rgba (floor r) (floor g) (floor b) a
          in impl env allowBreak ambientPrec availableWidth
             { cur | path <- cur.path `snoc` Arg, term <- e }
             |> L.fill c' >> Just
        _ -> Nothing
      App (Ref (R.Builtin "View.fit-width")) (Lit (Term.Distance d)) ->
        let rem = availableWidth `min` floor (Distance.pixels d (toFloat availableWidth))
        in Just (impl env allowBreak ambientPrec rem
                 { cur | path <- cur.path `snoc` Arg, term <- e })
      Ref (R.Builtin "View.hide") -> Just (L.empty t)
      Ref (R.Builtin "View.horizontal") -> case e of
        Vector es -> Nothing -- todo more complicated, as we need to do sequencing
        _ -> Nothing
      Ref (R.Builtin "View.swatch") -> case e of
        App (App (App (App (Ref (R.Builtin "Color.rgba")) (Lit (Number r))) (Lit (Number g))) (Lit (Number b))) (Lit (Number a)) ->
          let c = Color.rgba (floor r) (floor g) (floor b) a
          in Just (L.embed t (Styles.swatch c))
        _ -> Nothing
      Ref (R.Builtin "View.source") ->
        Just (impl env allowBreak ambientPrec availableWidth
              { cur | path <- cur.path `snoc` Arg, term <- e })
      App (App (Ref (R.Builtin "View.spacer")) (Lit (Term.Distance w))) (Lit (Term.Number h)) ->
        let w' = availableWidth `min` floor (Distance.pixels w (toFloat availableWidth))
            h' = ceiling h
        in Just (L.embed t (E.spacer w' h'))
      App (Ref (R.Builtin "View.text")) style -> case e of
        -- todo, actually interpret style
        Lit (Text s) -> Just (L.embed t (E.leftAligned (Text.style Text.defaultStyle (Text.fromString s))))
      App (App (App (Ref (R.Builtin "View.textbox")) (Ref (R.Builtin alignment))) (Lit (Term.Distance d))) style ->
        case e of
          Lit (Text s) ->
            -- todo, actually interpret style
            let f = case alignment of
                      "Text.left"    -> E.leftAligned
                      "Text.right"   -> E.rightAligned
                      "Text.center"  -> E.centered
                      "Text.justify" -> E.justified
                e = f (Text.style Text.defaultStyle (Text.fromString s))
                rem = availableWidth `max` floor (Distance.pixels d (toFloat availableWidth))
                e' = if E.widthOf e > rem then E.width rem e else e
            in Just (L.embed t e')
          _ -> Nothing
      Ref (R.Builtin "View.vertical") -> case e of
        Vector es ->
          let f i e = impl env allowBreak ambientPrec availableWidth
                      { cur | path <- cur.path `append` [Arg, Path.Index i], term <- e }
          in Just (L.vertical (tag (cur.path `snoc` Arg)) (List.indexedMap f (Array.toList es)))
        _ -> Nothing
      Ref (R.Builtin "View.embed") -> builtins env allowBreak availableWidth ambientPrec
                                   { cur | path <- cur.path `snoc` Arg, term <- e }
      Ref (R.Builtin "View.wrap") -> case e of
        Vector es -> Nothing -- todo more complicated, as we need to do sequencing
        _ -> Nothing
      _ -> Nothing
  in case cur.term of
    App (App (App (Ref (R.Builtin "View.view")) (App (Ref (R.Builtin "View.function1")) (Lam n body))) f) e ->
      -- all paths will point to `f` aside from `e`
      let eview = impl env allowBreak 0 availableWidth
                  { cur | path <- cur.path `snoc` Arg, term <- e }
                  |> Embed >> close
          fpath = cur.path `append` [Fn,Arg]
          trim l = if Path.startsWith fpath l.path then { l | path <- cur.path } else l
          g view = impl env allowBreak ambientPrec availableWidth
                   { cur | path <- fpath, term <- betaReduce (App (Lam n body) (unclose view)) }
                   |> L.map trim
      in Maybe.map g eview
    App (App (Ref (R.Builtin "View.cell")) v) e -> go v e
    App (App (Ref (R.Builtin "View.view")) v) e -> go v e
    _ -> Nothing

reactivePaths : Term -> Trie Path.E ()
reactivePaths e =
  let ok e = case e of
    App (App (Ref (R.Builtin "View.cell")) (Ref (R.Builtin "View.reactive"))) e -> True
    App (App (Ref (R.Builtin "View.view")) (Ref (R.Builtin "View.reactive"))) e -> True
    _ -> False
  in Term.matchingPaths ok e

declaredPaths : Term -> Trie Path.E String
declaredPaths e =
  let ok e = case e of
    App (App (Ref (R.Builtin "View.declare")) (Lit (Text s))) e -> Just s
    _ -> Nothing
  in Term.collectPaths ok e
