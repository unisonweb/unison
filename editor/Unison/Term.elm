module Unison.Term where

import Array
import Array (Array)
import Dict
import Dict (Dict)
import Json
import Set
import Set (Set)
import Graphics.Element as Element
import Graphics.Input (Handle, hoverable)
import Text(..)
import Unison.Styles as Styles
import Unison.Styles (codeText)
import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Jsonify as J
import Unison.Jsonify (Jsonify)
import Unison.Metadata as Metadata
import Unison.Metadata (Metadata, Fixity)
import Unison.Parser as P
import Unison.Parser (Parser)
import Unison.Path (..)
import Unison.Var (I)
import Unison.Var as V
import Unison.Type as T

data Literal
  = Number Float
  | String String
  | Vector (Array Term)

data Term
  = Var I
  | Lit Literal
  | Con Hash
  | Ref Hash
  | App Term Term
  | Ann Term T.Type
  | Lam I Term

render : Term -- term to render
      -> { handle         : Handle (Maybe (Hash, Path))
         , key            : Hash
         , highlighted    : Set Path
         , availableWidth : Int
         , metadata       : Hash -> Metadata }
      -> Element
render expr env =
  let
    md = env.metadata env.key
    go : Bool -> Int -> Int -> { path : Path, term : Term } -> Element
    go allowBreak ambientPrec availableWidth cur =
      case cur.term of
        Var n -> hoverable env.handle (msg cur.path) (codeText (Metadata.resolveLocal md cur.path n).name)
        Ref h -> hoverable env.handle (msg cur.path) (codeText (Metadata.firstName h (env.metadata h)))
        Con h -> hoverable env.handle (msg cur.path) (codeText (Metadata.firstName h (env.metadata h)))
        Lit (Number n) -> hoverable env.handle (msg cur.path) (codeText (show n))
        Lit (String s) -> hoverable env.handle (msg cur.path) (codeText s)
        _ -> case break md cur.path cur.term of
          Prefix f args ->
            let f' = go False 9 availableWidth f
                lines = f' :: map (go False 10 0) args
                unbroken = paren (ambientPrec > 9) cur.path (flow right (intersperse space lines))
            in if not allowBreak || widthOf unbroken < availableWidth
               then unbroken
               else let args' = map (go True 10 (availableWidth - indentWidth)) args |> flow down
                    in f' `above` (space2 `beside` args')
                       |> paren (ambientPrec > 9) cur.path
          Operators leftAssoc prec hd tl ->
            let f (op,r) l = flow right [ l, space, go False 10 0 op, space, go False rprec 0 r ]
                unbroken = foldl f (go False lprec 0 hd) tl |> paren (ambientPrec > 9) cur.path
                lprec = if leftAssoc then prec else 1+prec
                rprec = if leftAssoc then 1+prec else prec
                bf (op,r) l =
                  let op' = go False 10 0 op
                      remWidth = availableWidth - widthOf op' - widthOf space
                  in l `above` flow right [op', space, go True rprec remWidth r ]
            in if not allowBreak || widthOf unbroken < availableWidth
               then unbroken
               else let h = hoverable env.handle (msg cur.path) (spaces 2)
                    in foldl bf (go True lprec (availableWidth - indentWidth) hd) tl
                       |> paren (ambientPrec > 9) cur.path
          Bracketed es ->
            let unbroken = Styles.cells (codeText "[]") (map (go False 0 0) es)
            in if not allowBreak || widthOf unbroken < env.availableWidth || length es < 2
            then unbroken
            else Styles.verticalCells unbroken
                                      (map (go True 0 (availableWidth - 6)) es) -- account for cell border
          Lambda args body ->
            let argLayout = flow right <|
                  intersperse (codeText " ") (map (go False 0 0) args) ++ [codeText " → "]
                unbroken = flow right [argLayout, go False 0 0 body]
                        |> paren (ambientPrec > 0) cur.path
            in if not allowBreak || widthOf unbroken < availableWidth
               then unbroken
               else flow down [argLayout, space2 `beside` go True 0 (availableWidth - indentWidth) body]
                    |> paren (ambientPrec > 0) cur.path

    msg path b = if b then Just (env.key, path) else Nothing

    paren : Bool -> Path -> Element -> Element
    paren parenthesize path e =
      if parenthesize
      then let (opening, closing) = (codeText "(", codeText ")")
               topOpen = container (widthOf opening) (heightOf e) topLeft (codeText "(")
                      |> hoverable env.handle (msg path)
               bottomClose = container (widthOf closing) (heightOf e) bottomLeft (codeText ")")
                          |> hoverable env.handle (msg path)
           in flow right [topOpen, e, bottomClose]
      else e

    space = codeText " "
    spaces n =
      if n <= 0 then empty else codeText (String.padLeft (n*2) ' ' "")
    space2 = codeText "  "
    indentWidth = widthOf space2

  in go True 0 env.availableWidth { path = Array.empty, term = expr }

data Break a
  = Prefix a [a]          -- `Prefix f [x,y,z] == f x y z`
  | Operators Bool Int a [(a,a)] -- `Operators False x [(+,y), (+,z)] == (x + y) + z`
                                 -- `Operators True x [(^,y), (^,z)] == x ^ (y ^ z)`
  | Bracketed [a]         -- `Bracketed [x,y,z] == [x,y,z]`
  | Lambda [a] a          -- `Lambda [x,y,z] e == x -> y -> z -> e`

break : Metadata -> Path -> Term -> Break { path : Path, term : Term }
break md path expr = todo

todo : a
todo = todo

parseLiteral : Parser Literal
parseLiteral = P.union' <| \t ->
  if | t == "Number" -> P.map Number P.number
     | t == "String" -> P.map String P.string
     | t == "Vector" -> P.map (Vector . Array.fromList) (P.array parseTerm)

jsonifyLiteral l = case l of
  Number n -> J.tag' "Number" J.number n
  String s -> J.tag' "String" J.string s
  Vector es -> J.tag' "Vector" (J.contramap Array.toList (J.array jsonifyTerm)) es

parseTerm : Parser Term
parseTerm = P.union' <| \t ->
  if | t == "Var" -> P.map Var V.parse
     | t == "Lit" -> P.map Lit parseLiteral
     | t == "Con" -> P.map Con H.parse
     | t == "Ref" -> P.map Ref H.parse
     | t == "App" -> P.lift2 App parseTerm parseTerm
     | t == "Ann" -> P.lift2 Ann parseTerm T.parseType
     | t == "Lam" -> P.lift2 Lam V.parse parseTerm

jsonifyTerm : Jsonify Term
jsonifyTerm e = case e of
  Var v -> J.tag' "Var" V.jsonify v
  Lit l -> J.tag' "Lit" jsonifyLiteral l
  Con h -> J.tag' "Con" H.jsonify h
  Ref h -> J.tag' "Ref" H.jsonify h
  App f x -> J.tag' "App" (J.array jsonifyTerm) [f, x]
  Ann e t -> J.tag' "Ann" (J.tuple2 jsonifyTerm T.jsonifyType) (e, t)
  Lam n body -> J.tag' "Lam" (J.tuple2 V.jsonify jsonifyTerm) (n, body)

