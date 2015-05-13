module Unison.Term where

import Array
import Array exposing (Array)
import Debug
import Dict
import Dict exposing (Dict)
import Elmz.Distance as Distance
import Elmz.Maybe as EM
import Elmz.Layout exposing (Layout)
import Elmz.Json.Encoder exposing (Encoder)
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Decoder exposing (Decoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Trie exposing (Trie)
import Elmz.Trie as Trie
import List
import Maybe
import Set
import Set exposing (Set)
import String
import Unison.Reference as R
import Unison.Hash exposing (Hash)
import Unison.Hash as H
import Unison.Metadata exposing (Metadata)
import Unison.Symbol exposing (Symbol,Fixity)
import Unison.Symbol as Symbol
import Unison.Metadata as Metadata
import Unison.Path exposing (..)
import Unison.Path as Path
import Unison.Type as T
type alias E = Path.E

type Literal
  = Number Float
  | Text String
  | Distance Distance.Distance

type Term
  = Var Symbol
  | Blank
  | Lit Literal
  | Ref R.Reference
  | App Term Term
  | Ann Term T.Type
  | Lam Symbol Term
  | Vector (Array Term)
  | Embed (Layout { path : Path, selectable : Bool })

type ClosedTerm = ClosedTerm Term

close : Term -> Maybe ClosedTerm
close e = if isClosed e then Just (ClosedTerm e) else Nothing

isClosed : Term -> Bool
isClosed e =
  let go env e = case e of
        Var v -> Set.member (Symbol.toKey v) env
        App f x -> go env f && go env x
        Ann e _ -> go env e
        Lam n body -> go (Set.insert (Symbol.toKey n) env) body
        Vector vs -> List.all (go env) (Array.toList vs)
        _ -> True
  in go Set.empty e

freeVars : Term -> List Symbol
freeVars e =
  let go env e = case e of
        Var v -> if Set.member (Symbol.toKey v) env then [] else [v]
        App f x -> go env f ++ go env x
        Ann e _ -> go env e
        Lam n body -> go (Set.insert (Symbol.toKey n) env) body
        Vector vs -> List.concatMap (go env) (Array.toList vs)
        _ -> []
      op v (env,vs) =
        let k = Symbol.toKey v
        in if Set.member k env then (env,vs)
           else (Set.insert k env, v::vs)
      dedup = List.foldl op (Set.empty, [])
  in snd (dedup (go Set.empty e))

unclose : ClosedTerm -> Term
unclose (ClosedTerm e) = e

checkLiteral : Term -> T.Type -> Bool
checkLiteral lit admissible = case (lit,admissible) of
  (Blank, _) -> True
  -- weird parser bug prevents use of T.Unit T.Distance as a pattern
  (Lit (Distance _), T.Lit d) -> d == T.Distance
  (Lit (Text _), T.Lit s) -> s == T.Text
  (Lit (Number _), T.Lit n) -> n == T.Number
  (_, T.Forall n (T.Universal n')) -> if n == n' then True else False
  _ -> False

{-| Returns the subterm at the given path, if the path is valid. -}
at : Path -> Term -> Maybe Term
at p e = case (p,e) of
  ([], e) -> Just e
  (Fn :: t, App f _) -> at t f
  (Arg :: t, App _ arg) -> at t arg
  (Body :: t, Lam _ body) -> at t body
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
  (Body :: t, Lam n body) -> Just (Lam n) `ap` set t body e'
  (Index i :: t, Vector es) -> case Array.get i es of
    Just e -> Maybe.map (\e -> Vector (Array.set i e es)) (set t e e')
    _ -> Nothing
  _ -> Nothing

{-| Rename `old` to `new` in `t`, avoiding subtrees that shadow `old`. -}
rename : Symbol -> Symbol -> Term -> Term
rename old new t = case t of
  Var v -> if v == old then Var new else t
  App f x -> App (rename old new f) (rename old new x)
  Ann e t -> Ann (rename old new e) t
  Lam n body -> if n == old then t else Lam n (rename old new body)
  Vector vs -> Vector (Array.map (rename old new) vs)
  _ -> t

{-| Substitute `t` for `v` in `body`, avoiding capture by alpha-renaming subtrees in `body`
    that use free variables of `t`. -}
subst : Term -> Symbol -> Term -> Term
subst t v body =
  let
    maxIdIn t = List.foldl max 0 (List.map .freshId (freeVars t))
    tmaxId = maxIdIn t
    closed = isClosed t
    go t v body = case body of
      Var v' -> if v' == v then t else body
      App f x -> App (go t v f) (go t v x)
      Ann e typ -> Ann (go t v e) typ
      Lam n e ->
        if closed then Lam n (go t v e)
        else
          -- rename `n` in `e` so `t` does not capture `e`
          let n' = { n | freshId <- 1 + (tmaxId `max` (maxIdIn body)) }
          in Lam n' (go t v (rename n n' e))
      Vector vs -> Vector (Array.map (go t v) vs)
      _ -> body
  in
    go t v body

betaReduce : Term -> Term
betaReduce e = case e of
  App (Lam n body) e -> subst e n body
  _ -> e

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
        (Body :: t, Lam n body) -> Just (Lam n) `ap` go t body
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
        Lam _ _ -> p `snoc` Body
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

{-| Return all paths into the term for which the focus matches the predicate. -}
matchingPaths : (Term -> Bool) -> Term -> Trie Path.E ()
matchingPaths ok =
  let ok' e = if ok e then Just () else Nothing
  in collectPaths ok'

{-| Extract information from all paths for which `ok` returns `Just`. -}
collectPaths : (Term -> Maybe a) -> Term -> Trie Path.E a
collectPaths ok e =
  let add = case ok e of
    Just a -> Trie.set a
    Nothing -> identity
  in add <| case e of
    App f arg -> Trie.cons Path.Fn (collectPaths ok f) `Trie.mergeDisjoint`
                 Trie.cons Path.Arg (collectPaths ok arg)
    Vector es -> Array.toList es
              |> List.indexedMap (\i e -> Trie.cons (Path.Index i) (collectPaths ok e))
              |> List.foldr Trie.mergeDisjoint Trie.empty
    Ann e _ -> collectPaths ok e
    Lam _ body -> Trie.cons Path.Body (collectPaths ok body)
    _ -> Trie.empty

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
     | t == "Text" -> Decoder.map Text Decoder.string
     | t == "Distance" -> Decoder.map Distance decodeDistance

encodeLiteral l = case l of
  Number n -> Encoder.tag' "Number" Encoder.float n
  Text s -> Encoder.tag' "Text" Encoder.string s
  Distance d -> Encoder.tag' "Distance" encodeDistance d

decodeTerm : Decoder Term
decodeTerm =
  Decoder.arrayUnion <| \t ->
  if | t == "Var" -> Decoder.map Var Symbol.decodeSymbol
     | t == "Tm" -> Decoder.union' <| \t ->
       if | t == "Lit" -> Decoder.map Lit decodeLiteral
          | t == "Vector" -> Decoder.map Vector (Decoder.array decodeTerm)
          | t == "Ref" -> Decoder.map Ref R.decode
          | t == "App" -> Decoder.product2 App decodeTerm decodeTerm
          | t == "Ann" -> Decoder.product2 Ann decodeTerm T.decodeType
          | t == "Lam" -> Decoder.arrayNewtyped "Abs" (Decoder.product2 Lam Symbol.decodeSymbol decodeTerm)
          | t == "Blank" -> Decoder.unit Blank
          | otherwise -> Decoder.fail ("decodeTerm.F unknown tag: " ++ t)
     | otherwise -> Decoder.fail ("decodeTerm.ABT unknown tag: " ++ t)

encodeTerm : Encoder Term
encodeTerm e = case e of
  Var v -> Encoder.tagProduct "Var" Symbol.encodeSymbol v
  Lam v body ->
    Encoder.tagProduct
      "Tm"
      (Encoder.tag' "Lam" (Encoder.tagProduct "Abs" (Encoder.tuple2 Symbol.encodeSymbol encodeTerm)))
      (v, body)
  _ -> Encoder.tagProduct "Tm" (\e -> case e of
    Blank -> Encoder.tag' "Blank" Encoder.product0 ()
    Lit l -> Encoder.tag' "Lit" encodeLiteral l
    Ref h -> Encoder.tag' "Ref" R.encode h
    App f x -> Encoder.tag' "App" (Encoder.list encodeTerm) [f, x]
    Ann e t -> Encoder.tag' "Ann" (Encoder.tuple2 encodeTerm T.encodeType) (e, t)
    Embed e -> Encoder.tag' "Embed" Encoder.product0 ()
    Vector es -> Encoder.tag' "Vector" (Encoder.list encodeTerm) (Array.toList es)
  ) e
