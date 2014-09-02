module Unison.Term where

import Array
import Array (Array)
import Dict
import Dict (Dict)
import Json
import Set
import Set (Set)
import String
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
  | Str String
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
         , highlighted    : [Path]
         , availableWidth : Int
         , metadata       : Hash -> Metadata }
      -> Element
render expr env =
  let
    md = env.metadata env.key
    msg path b = if b then Just (env.key, path) else Nothing

    go : Bool -> Int -> Int -> { path : Path, term : Term } -> Element
    go allowBreak ambientPrec availableWidth cur =
      case cur.term of
        Var n -> hoverable env.handle (msg cur.path) (codeText (Metadata.resolveLocal md cur.path n).name)
        Ref h -> hoverable env.handle (msg cur.path) (codeText (Metadata.firstName h (env.metadata h)))
        Con h -> hoverable env.handle (msg cur.path) (codeText (Metadata.firstName h (env.metadata h)))
        Lit (Number n) -> hoverable env.handle (msg cur.path) (codeText (String.show n))
        Lit (Str s) -> hoverable env.handle (msg cur.path) (codeText s)
        _ -> case break env.key env.metadata cur.path cur.term of
          Prefix f args ->
            let f' = go False 9 availableWidth f
                lines = f' :: map (go False 10 0) args
                unbroken = paren (ambientPrec > 9) cur.path (flow right (intersperse space lines |> Styles.row))
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

break : Hash -> (Hash -> Metadata) -> Path -> Term -> Break { path : Path, term : Term }
break hash md path expr =
  let prefix f acc path = case f of
        App f arg -> prefix f ({ path = path `push` Arg, term = arg } :: acc) (path `push` Fn)
        _ -> Prefix { path = path, term = f } acc
      opsL o prec e acc path = case e of
        App (App op l) r ->
          if op == o
          then
            let hd = (
              { path = path `append` [Fn,Fn], term = op },
              { path = path `push` Arg, term = r })
            in opsL o prec l (hd :: acc) (path `append` [Fn,Arg])
          else Operators False prec { path = path, term = e} acc
        _ -> Operators False prec { path = path, term = e } acc
      opsR o prec e path = case e of
        App (App op l) r ->
          if op == o
          then case opsR o prec r (path `push` Arg) of
            Operators _ prec hd tl ->
              let tl' = ({ path = path `append` [Fn,Fn], term = op }, hd) :: tl
              in Operators True prec { path = path `append` [Fn,Arg], term = l} tl'
          else Operators True prec { path = path, term = e} []
        _ -> Operators True prec { path = path, term = e } []
  in case expr of
    Lit (Vector xs) -> xs
                    |> Array.indexedMap (\i a -> { path = path `push` Index i, term = a })
                    |> Array.toList
                    |> Bracketed
    App (App op l) r ->
      let sym = case op of
        Ref h -> Metadata.firstSymbol h (md h)
        Con h -> Metadata.firstSymbol h (md h)
        Var v -> Metadata.resolveLocal (md hash) path v
      in case sym.fixity of
        Metadata.Prefix -> prefix (App (App op l) r) [] path -- not an operator chain, fall back
        Metadata.InfixL -> opsL op sym.precedence (App (App op l) r) [] path -- left associated operator chain
        Metadata.InfixR -> opsR op sym.precedence (App (App op l) r) path
    Lam v body -> case body of -- audit this
      Lam _ _ -> case break hash md (path `push` Body) body of
        Lambda args body2 -> Lambda ({ path = path `push` Body, term = body } :: args) body2
        _ -> Lambda [{path = path, term = expr }] { path = path `push` Body, term = body }
      _ -> Lambda [{path = path, term = expr }] { path = path `push` Body, term = body }
    _ -> prefix expr [] path

parseLiteral : Parser Literal
parseLiteral = P.union' <| \t ->
  if | t == "Number" -> P.map Number P.number
     | t == "String" -> P.map Str P.string
     | t == "Vector" -> P.map (Vector . Array.fromList) (P.array parseTerm)

jsonifyLiteral l = case l of
  Number n -> J.tag' "Number" J.number n
  Str s -> J.tag' "String" J.string s
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

