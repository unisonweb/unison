module Unison.View (Env, key, layout, layout', L) where

import Array
import Color
import Debug
import Elmz.Distance as Distance
import Elmz.Layout (Layout)
import Elmz.Layout as L
import Elmz.Trie as Trie
import Elmz.Trie (Trie)
import List
import List ((::))
import Graphics.Element as E
import Maybe
import Unison.Reference as R
import Unison.Hash (Hash)
import Unison.Metadata (Metadata, Fixity)
import Unison.Metadata as Metadata
import Unison.Styles (codeText)
import Unison.Styles as Styles
import Unison.Term (..)
import Unison.Term as Term
import Unison.Type as Type
import Unison.Path (..)
import Unison.Path as Path
import Unison.Var as V
import String
import Text
type alias E = Path.E
type alias Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

type alias L = { path : Path, selectable : Bool }

type alias Env =
  { rootMetadata   : Metadata
  , availableWidth : Int
  , metadata       : R.Reference -> Metadata
  , overrides      : Path -> Maybe Term
  , raw            : Trie E () -- whether a path should be displayed as raw source
  }

type alias Cur =
  { path : Path
  , term : Term
  , boundAt : Path -> V.I -> Path }

resolveLocal : String -> Metadata -> Path -> Metadata.Symbol
resolveLocal prefix md p = case Metadata.localSymbol md p of
  Nothing ->
    let sym = Metadata.anonymousSymbol
        depth = List.length (List.filter ((==) Path.Body) p) + 1
    in { sym | name <- prefix ++ toString depth }
  Just sym -> sym

weakenBoundAt : (Path -> V.I -> Path) -> Path -> V.I -> Path
weakenBoundAt boundAt path v =
  if v == V.bound1 then Path.trimThroughScope path
  else boundAt (Path.trimThroughScope path) (V.decr v)

key : { tl | rootMetadata : Metadata, metadata : R.Reference -> Metadata }
   -> Cur
   -> String
key env cur = case cur.term of
  Blank -> "_"
  Var v -> (resolveLocal "v" env.rootMetadata (cur.boundAt cur.path v)).name
  Lit (Number n) -> toString n
  Lit (Str s) -> "\"" ++ toString s ++ "\""
  Lit (Distance d) -> toString d
  Ref r -> Metadata.firstName "anonymous" (env.metadata r)
  App f arg -> key env { cur | path <- cur.path `snoc` Fn, term <- f } ++
               key env { cur | path <- cur.path `snoc` Arg, term <- arg }
  Ann e t -> key env { cur | term <- e }
  Vector terms ->
    let ki i term = key env { cur | path <- cur.path `snoc` Index i, term <- term }
    in "[" ++ String.join "," (Array.toList (Array.indexedMap ki terms)) ++ "]"
  Lam body -> key env { path = cur.path `snoc` Body
                      , term = body
                      , boundAt = weakenBoundAt cur.boundAt }

{-|

  Layout proceeds in phases:

  1. Traverse the term, find all panels / cells, and compute hashes for all
  2. Build mapping from path to hash - for each panel / cell path, what is its hash
  3. For each panel / cell, compute its dependencies, Dict Hash [Hash]

  At this point, we have a dependency graph for the root panel.

  4. For each panel / cell, if it is marked reactive, as in `cell reactive x`,
     if `x` is a closed term, add it to list of paths that need evaluation.
  5. Send all terms needing evaluation to the node. Node replies with a
     Dict Path Term which editor will splice in.
     (optimization - evaluate some terms locally when possible)

  At this point, we have a fully resolved term tree.

  6. We traverse the resolved tree, applying special layout forms, and building
     up an 'overrides' map `Dict Path (Layout L)`.
     a. This gives us
  7. Finally, we invoke the regular Term.layout function, passing it the overrides map.

  Can be smarter about how we do updates, avoid needless recomputation.

  cell : (a -> Layout) -> a -> a
  vflow : [Layout] -> Layout
  fn : (Layout -> Layout) -> (a -> b) -> Layout

  cell (fn (\x -> vflow [x, hline])) sqrt 23

  panel vflow [panel source 12, panel source "hi", panel reactive #af789de]
  need to
  don't do any evaluation during layout, up to user to evaluate beforehand
  panel vflow (map blah [0..100]) is problematic, doing arbitrary computation at layout time
  but we have to do this for cases like `panel reactive (2 + 2)`

specialLayout : Path -> Term -> Maybe (Layout L)
specialLayout at e = case e of
  -- panel : (a -> Layout) -> a -> Layout
  App (App (Builtin "Unison.Layout.Panel") f) r ->
    interpretLayout f r
  -- cell : (a -> Layout) -> a -> a
  _ -> Nothing


interpretLayout : Path -> Term -> Term -> Maybe (Layout L)
interpretLayout at f e = case f of
  Builtin "Unison.Layout.hflow" -> case e of
  -- panel vflow [panel source 12, panel source "woot"]

-}

todo : a
todo = todo

-- use overrides for

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
  impl env True 0 env.availableWidth { path = [], term = expr, boundAt _ v = [] }

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
    Just l -> impl env allowBreak ambientPrec availableWidth { cur | term <- l }
    Nothing -> case cur.term of
      Embed l -> l
      Var v -> codeText (resolveLocal "v" env.rootMetadata (cur.boundAt cur.path v)).name
            |> L.embed (tag cur.path)
      Ref h -> codeText (Metadata.firstName (R.toKey h) (env.metadata h)) |> L.embed (tag cur.path)
      Blank -> Styles.blank |> L.embed (tag cur.path)
      Lit (Number n) -> Styles.numericLiteral (toString n) |> L.embed (tag cur.path)
      Lit (Str s) -> Styles.stringLiteral ("\"" ++ s ++ "\"") |> L.embed (tag cur.path)
      Ann e t -> let ann = Styles.codeText (" : " ++ Type.key env t)
                 in L.beside (tag cur.path)
                             (impl env allowBreak 9 (availableWidth - E.widthOf ann)
                               { cur | term <- e })
                             (L.embed (tag cur.path) ann)
      Lam body ->
        let space' = L.embed (tag cur.path) space
            arg = codeText (resolveLocal "v" env.rootMetadata cur.path).name
               |> L.embed (tag cur.path)
            nested = case body of
              Lam _ -> True
              _ -> False
            argLayout = [arg]
                     ++ (if nested then [] else [L.embed (tag cur.path) (codeText "→")])
                     |> L.intersperseHorizontal space'
            weakened = weakenBoundAt cur.boundAt
            cur' = { cur | boundAt <- weakenBoundAt cur.boundAt
                         , term <- body
                         , path <- cur.path `snoc` Body }
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
      _ -> case (Trie.lookup cur.path env.raw, builtins env allowBreak ambientPrec availableWidth cur) of
        (Nothing, Just l) -> l
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
              let unbroken = Styles.cells (tag cur.path) (codeText "[]") (List.map (impl env False 0 0) es)
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
    Vector xs -> xs
              |> Array.indexedMap (\i a -> { cur | path <- cur.path `snoc` Index i, term <- a })
              |> Array.toList
              |> Bracketed
    App (App op l) r ->
      let sym = case op of
        Ref h -> Metadata.firstSymbol (R.toKey h) (env.metadata h)
        Var v -> resolveLocal "v" env.rootMetadata (cur.boundAt cur.path v)
        _ -> Metadata.anonymousSymbol
      in case sym.fixity of
        Metadata.Prefix -> prefix (App (App op l) r) [] cur.path -- not an operator chain, fall back
        Metadata.InfixL -> opsL op sym.precedence (App (App op l) r) [] cur.path -- left associated operator chain
        Metadata.InfixR -> opsR op sym.precedence (App (App op l) r) cur.path
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
textboxt : Alignment -> Distance -> Style -> View String
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

panel vertical [
  panel source "hello",
  panel source (1 + 23)
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
        Lit (Str s) -> Just (L.embed t (Text.leftAligned (Text.style Text.defaultStyle (Text.fromString s))))
      App (App (App (Ref (R.Builtin "View.textbox")) (Ref (R.Builtin alignment))) (Lit (Term.Distance d))) style ->
        case e of
          Lit (Str s) ->
            -- todo, actually interpret style
            let f = case alignment of
                      "Text.left"    -> Text.leftAligned
                      "Text.right"   -> Text.rightAligned
                      "Text.center"  -> Text.centered
                      "Text.justify" -> Text.justified
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
      Ref (R.Builtin "View.id") -> builtins env allowBreak availableWidth ambientPrec
                                   { cur | path <- cur.path `snoc` Arg, term <- e }
      Ref (R.Builtin "View.wrap") -> case e of
        Vector es -> Nothing -- todo more complicated, as we need to do sequencing
        _ -> Nothing
      _ -> Nothing
  in case cur.term of
    App (App (App (Ref (R.Builtin "View.cell")) (App (Ref (R.Builtin "View.function1")) (Lam body))) f) e ->
      -- all paths will point to `f` aside from `e`
      let eview = impl env allowBreak 0 availableWidth
                  { cur | path <- cur.path `snoc` Arg, term <- e }
                  |> Embed >> close
          fpath = cur.path `append` [Fn,Arg]
          trim l = if Path.startsWith fpath l.path then { l | path <- cur.path } else l
          g view = impl env allowBreak ambientPrec availableWidth
                   { cur | path <- fpath, term <- betaReduce (App (Lam body) (unclose view)) }
                   |> L.map trim
      in Maybe.map g eview
    App (App (Ref (R.Builtin "View.panel")) v) e -> go v e
    App (App (Ref (R.Builtin "View.cell")) v) e -> go v e
    _ -> Nothing

