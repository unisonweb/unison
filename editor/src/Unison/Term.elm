module Unison.Term where

import Array
import Array (Array)
import Debug
import Dict
import Dict (Dict)
import Elmz.Distance as Distance
import Elmz.Maybe as EM
import Elmz.Layout (Layout)
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Decoder (Decoder)
import Elmz.Json.Decoder as Decoder
import List
import List ((::))
import Maybe
import Set
import Set (Set)
import String
import Text
import Unison.Reference as R
import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Metadata (Metadata, Fixity)
import Unison.Metadata as Metadata
import Unison.Path (..)
import Unison.Path as Path
import Unison.Type as T
import Unison.Var (I)
import Unison.Var as V
type alias E = Path.E

type Literal
  = Number Float
  | Str String
  | Distance Distance.Distance

type Term
  = Var I
  | Blank
  | Lit Literal
  | Ref R.Reference
  | App Term Term
  | Ann Term T.Type
  | Lam Term
  | Vector (Array Term)
  | Embed (Layout { path : Path, selectable : Bool })

type ClosedTerm = ClosedTerm Term

close : Term -> Maybe ClosedTerm
close e = if isClosed e then Just (ClosedTerm e) else Nothing

isClosed : Term -> Bool
isClosed e =
  let go depth e = case e of
        Var i -> i < depth
        App f x -> go depth f || go depth x
        Ann e _ -> go depth e
        Lam body -> go (V.succ depth) body
        Vector vs -> List.all (go depth) (Array.toList vs)
        _ -> True
  in go V.bound1 e

unclose : ClosedTerm -> Term
unclose (ClosedTerm e) = e

checkLiteral : Literal -> T.Type -> Bool
checkLiteral lit admissible = case (lit,admissible) of
  -- weird parser bug prevents use of T.Unit T.Distance as a pattern
  (Distance _, T.Unit d) -> d == T.Distance
  (Str _, T.Unit s) -> s == T.String
  (Number _, T.Unit n) -> n == T.Number
  (_, T.Forall n (T.Universal n')) -> if n == n' then True else False
  _ -> False

betaReduce : Term -> Term
betaReduce e =
  let go depth arg body = case body of
    App f x -> App (go depth arg f) (go depth arg x)
    Vector vs -> Vector (Array.fromList (List.map (go depth arg) (Array.toList vs)))
    Ann body t -> Ann (go depth arg body) t
    Lam body -> Lam (go (V.succ depth) arg body)
    Var v -> if v == depth then arg else Var v
    _ -> body
  in case e of
    App (Lam f) arg -> go V.bound1 f arg
    _ -> e

{-| Returns the subterm at the given path, if the path is valid. -}
at : Path -> Term -> Maybe Term
at p e = case (p,e) of
  ([], e) -> Just e
  (Fn :: t, App f _) -> at t f
  (Arg :: t, App _ arg) -> at t arg
  (Body :: t, Lam body) -> at t body
  (Index i :: t, Vector es) -> case Array.get i es of
    Just e -> at t e
    _ -> Nothing
  _ -> Nothing

{-| Sets the given path to `e'`, if the path is valid. -}
set : Path -> Term -> Term -> Maybe Term
set p e e' = let ap = EM.ap in case (p,e) of
  ([], e) -> Just e'
  (Fn :: t, App f arg) -> Just App `ap` set t f e' `ap` Just arg
  (Arg :: t, App f arg) -> Just (App f) `ap` set t arg e'
  (Body :: t, Lam body) -> Just Lam `ap` set t body e'
  (Index i :: t, Vector es) -> case Array.get i es of
    Just e -> Maybe.map (\e -> Vector (Array.set i e es)) (set t e e')
    _ -> Nothing
  _ -> Nothing

{-| Returns the `(locOuter,inner,locInner)` with maximum length of
`locInner` such that `at locInner inner == at loc e`,
`locOuter ++ locInner == loc`, and `inner` has no free variables.
That is, we modify the path to be relative to the nearest closed
term, rather than from the root.
-}
narrow : Term -> Path -> (Path,Term,Path)
narrow e loc = ([],e,loc) -- todo

{-| Modify the term at the given location, if valid. -}
modify : Path -> (Term -> Term) -> Term -> Maybe Term
modify path f e =
  at path e `Maybe.andThen` \focus -> set path e (f focus)

{-| Sets the given path to `e'`, if the path is valid, otherwise returns `ctx`. -}
trySet : Path -> Term -> Term -> Term
trySet p e ctx = Maybe.withDefault ctx (set p e ctx)

delete : Path -> Term -> Maybe Term
delete p e =
  let ap = EM.ap
      orElse m1 m2 = Maybe.oneOf [m1,m2]
      go p e = case (p,e) of
        ([], e) -> Nothing
        (Fn :: t, App f arg) -> Just App `ap` go t f `ap` Just arg `orElse` Just arg
        (Arg :: t, App f arg) -> Just (App f) `ap` go t arg `orElse` Just f
        (Body :: t, Lam body) -> Just Lam `ap` go t body
        (Index i :: t, Vector es) -> case Array.get i es of
          Just e -> Maybe.map (\e -> Vector (Array.set i e es)) (go t e) `orElse`
                    Just (Vector (Array.slice 0 i es `Array.append`
                                  Array.slice (i+1) (Array.length es) es))
          _ -> Nothing
        _ -> Nothing
  in if valid e p then go p e else Nothing

{-| Returns `True` if the path points to a valid subterm -}
valid : Term -> Path -> Bool
valid e p = case at p e of
  Nothing -> False
  Just _ -> True

{-| Move path to point to leftmost child, or return `p` unmodified
    if no such child exists. -}
down : Term -> Path -> Path
down e p =
  let apps e = case e of
        App f x -> apps f + 1
        _ -> 1
      go e = case e of
        App f x -> p `append` List.repeat (apps f) Fn
        Vector es -> if Array.length es == 0 then p else p `snoc` Index 0
        Lam _ -> p `snoc` Body
        _ -> p
  in Maybe.withDefault p (Maybe.map go (at p e))

{-| Move path to point to parent node in "logical" layout. -}
up : Path -> Path
up p =
  let go p = case p of
    [] -> []
    _ :: Body :: tl -> List.reverse (Body :: tl)
    _ :: Arg :: tl -> List.reverse (Arg :: tl)
    _ :: Index i :: tl -> List.reverse (Index i :: tl)
    Fn :: tl -> go tl
    Arg :: tl -> go tl
    _ :: tl -> List.reverse tl
  in go (List.reverse p)

{-| Move the path to its immediate sibling to the right,
    or return `p` unmodified if no such sibling exists.  -}
siblingR : Term -> Path -> Path
siblingR e p =
  let p2 = increment (valid e) p
  in if decrement (valid e) p2 == p then p2
     else p

{-| Move the path to its immediate sibling to the right,
    or return `p` unmodified if no such sibling exists.  -}
siblingL : Term -> Path -> Path
siblingL e p =
  let p2 = decrement (valid e) p
  in if increment (valid e) p2 == p then p2
     else p

trimTo : (Term -> Bool) -> Term -> Path -> Maybe Path
trimTo goal e path =
  if | Maybe.withDefault False (Maybe.map goal (at path e)) -> Just path
     | path == [] -> Nothing
     | otherwise -> trimTo goal e (up path)

decodeDistance : Decoder (Distance.Distance)
decodeDistance = Decoder.union' <| \t ->
  if | t == "Pixel" -> Decoder.unit Distance.Pixel
     | t == "Scale" -> Decoder.product2 Distance.Scale Decoder.float decodeDistance
     | t == "Ceiling" -> Decoder.map Distance.Ceiling decodeDistance
     | t == "Floor" -> Decoder.map Distance.Floor decodeDistance
     | t == "Min" -> Decoder.product2 Distance.Min decodeDistance decodeDistance
     | t == "Max" -> Decoder.product2 Distance.Max decodeDistance decodeDistance

encodeDistance : Encoder Distance.Distance
encodeDistance e = case e of
  Distance.Pixel -> Encoder.tag' "Pixel" Encoder.product0 ()
  Distance.Scale k dist -> Encoder.tag' "Scale" (Encoder.tuple2 Encoder.float encodeDistance) (k,dist)
  Distance.Ceiling dist -> Encoder.tag' "Ceiling" encodeDistance dist
  Distance.Floor dist -> Encoder.tag' "Floor" encodeDistance dist
  Distance.Max dist1 dist2 -> Encoder.tag' "Max" (Encoder.tuple2 encodeDistance encodeDistance) (dist1, dist2)
  Distance.Min dist1 dist2 -> Encoder.tag' "Min" (Encoder.tuple2 encodeDistance encodeDistance) (dist1, dist2)

decodeLiteral : Decoder Literal
decodeLiteral = Decoder.union' <| \t ->
  if | t == "Number" -> Decoder.map Number Decoder.float
     | t == "String" -> Decoder.map Str Decoder.string
     | t == "Distance" -> Decoder.map Distance decodeDistance

encodeLiteral l = case l of
  Number n -> Encoder.tag' "Number" Encoder.float n
  Str s -> Encoder.tag' "String" Encoder.string s
  Distance d -> Encoder.tag' "Distance" encodeDistance d

decodeTerm : Decoder Term
decodeTerm = Decoder.union' <| \t ->
  if | t == "Var" -> Decoder.map Var V.decode
     | t == "Lit" -> Decoder.map Lit decodeLiteral
     | t == "Vector" -> Decoder.map Vector (Decoder.array decodeTerm)
     | t == "Ref" -> Decoder.map Ref R.decode
     | t == "App" -> Decoder.product2 App decodeTerm decodeTerm
     | t == "Ann" -> Decoder.product2 Ann decodeTerm T.decodeType
     | t == "Lam" -> Decoder.map Lam decodeTerm
     | t == "Blank" -> Decoder.unit Blank

encodeTerm : Encoder Term
encodeTerm e = case e of
  Blank -> Encoder.tag' "Blank" Encoder.product0 ()
  Var v -> Encoder.tag' "Var" V.encode v
  Lit l -> Encoder.tag' "Lit" encodeLiteral l
  Ref h -> Encoder.tag' "Ref" R.encode h
  App f x -> Encoder.tag' "App" (Encoder.list encodeTerm) [f, x]
  Ann e t -> Encoder.tag' "Ann" (Encoder.tuple2 encodeTerm T.encodeType) (e, t)
  Lam body -> Encoder.tag' "Lam" encodeTerm body
  Embed e -> Encoder.tag' "Embed" Encoder.product0 ()
  Vector es -> Encoder.tag' "Vector" (Encoder.list encodeTerm) (Array.toList es)
