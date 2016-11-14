{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Unison.Views where

import Data.Map (Map)
import Data.Text (Text)
import Unison.Doc (Doc)
import Unison.Metadata (Metadata)
import Unison.Paths (Path)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol(..))
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Unison.Doc as D
import qualified Unison.Hash as Hash
import qualified Unison.Metadata as Metadata
import qualified Unison.Paths as P
import qualified Unison.Reference as Reference
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as E
import qualified Unison.Type as T
import qualified Unison.Var as Var
import qualified Unison.View as View

type ViewableTerm = Term (Symbol View.DFO)
type ViewableType = Type (Symbol View.DFO)

lookupSymbol :: Map Reference (Metadata (Symbol View.DFO) Reference) -> Reference -> Symbol View.DFO
lookupSymbol mds ref = maybe (defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref mds)
  where
  firstName :: Metadata.Names v -> v
  firstName (Metadata.Names (n:_)) = n
  firstName _ = error "empty names"

termMd :: Map Reference (Metadata (Symbol View.DFO) Reference) -> ViewableTerm -> Doc Text Path
termMd mds = term (lookupSymbol mds)

typeMd :: Map Reference (Metadata (Symbol View.DFO) Reference) -> ViewableType -> Doc Text Path
typeMd mds = type' (lookupSymbol mds)

bindingMd
  :: Map Reference (Metadata (Symbol View.DFO) Reference) -> Symbol View.DFO -> ViewableTerm
  -> Doc Text Path
bindingMd mds = binding (lookupSymbol mds)

binding :: (Reference -> Symbol View.DFO) -> Symbol View.DFO -> ViewableTerm -> Doc Text Path
binding ref name body = case body of
  E.Ann' body t -> D.sub P.Annotation $
    D.docs [
      D.group (D.docs [term ref (E.var name), D.delimiter " :", D.breakable " ", D.nest "  " (type' ref t)]),
      D.linebreak,
      D.group (go body)
    ]
  body -> go body
  where
  go (LamsP' vs (body,bodyp)) =
    D.docs [ term ref (E.var name `E.apps` map (E.var . fst) vs)
           , D.delimiter " =", D.breakable " "
           , D.nest "  " (D.sub' bodyp $ term ref body), D.linebreak ]
  go body =
    D.docs [ term ref (E.var name), D.delimiter " =", D.breakable " "
           , D.nest "  " (D.sub P.Body $ term ref body), D.linebreak]

term :: (Reference -> Symbol View.DFO) -> ViewableTerm -> Doc Text Path
term ref t = D.group (go no View.low t) where
  no = const False
  sym v = D.embed (Var.name v)
  op t = case t of
    E.Lit' l -> Symbol.annotate View.prefix . (\r -> Symbol.prefix r :: Symbol ()) . Text.pack . show $ l
    E.Var' v -> v
    E.Ref' r -> ref r
    E.Blank' -> Symbol.annotate View.prefix (Symbol.prefix "_" :: Symbol ())
    _ -> Symbol.annotate View.prefix (Symbol.prefix "<unresolved-operator>" :: Symbol ())
  formatBinding :: Path -> Symbol View.DFO -> ViewableTerm -> Doc Text Path
  formatBinding path name body = case body of
    LamsP' vs (body,bodyp) ->
      let lhs = fmap fixup $ go no View.low (E.apps (E.var name) (map (E.var . fst) vs))
          fixup _ = [] -- todo - could use paths to individual variables
          rhs = D.sub P.Body . D.sub' bodyp $ go no View.low body
      in D.group . D.sub' path $ D.docs [lhs, D.delimiter " =", D.breakable " ", D.nest "  " rhs]
    _ -> D.sub' path . D.group . D.docs $
           [ D.sub P.Bound (sym name), D.delimiter " =", D.breakable " "
           , D.nest "  " . D.sub P.Body $ go no View.low body ]
  go :: (ViewableTerm -> Bool) -> View.Precedence -> ViewableTerm -> Doc Text Path
  go inChain p t = case t of
    E.Lets' bs e ->
      let
        -- bindings have paths [P.Binding 0], [P.Body, P.Binding 0], [P.Body,P.Body,P.Binding 0], etc
        pe = replicate (length bs) P.Body
        bps = [ replicate n P.Body ++ [P.Binding 0] | n <- [0 .. length bs - 1] ]
        formattedBs = [ formatBinding bp name b | ((name,b), bp) <- bs `zip` bps ]
      in D.parenthesize (p /= View.low) . D.group $
           D.docs [D.delimiter "let", D.breakable " "]
           `D.append`
             D.nest "  " (D.delimit (D.breakable "; ") (formattedBs ++ [D.sub' pe (go no View.low e)]))
    E.LetRecNamed' bs e ->
      let
        bps = map P.Binding [0 .. length bs - 1]
        formattedBs = [ formatBinding [bp] name b | ((name,b), bp) <- bs `zip` bps ]
      in D.parenthesize (p /= View.low) . D.group $
         D.docs [D.delimiter "let rec", D.breakable " "]
         `D.append`
           D.nest "  " (D.delimit (D.breakable "; ") (formattedBs ++ [D.sub P.Body $ go no View.low e]))
    E.Vector' vs | Vector.null vs -> D.embed "[ ]"
                 | otherwise      ->
      let
        fmt i v = D.nest "  " . D.sub (P.Index i) $ go no View.low v
        subs = [ fmt i v | (v,i) <- Vector.toList vs `zip` [0..] ]
      in D.group . D.docs $
           [ D.delimiter "[ "
           , D.delimit (D.breakable ", ") subs
           , D.delimiter " ]" ]
    AppsP' (fn,fnP) args ->
      let
        Symbol.Symbol _ name view = op fn
        arity = if length args >= View.arity view then View.arity view else 0
        (taken, remaining) = splitAt arity args
        fmt (child,path) = (\p -> go (fn ==) p child, path)
        paren = p > View.precedence view && View.arity view /= 0
        applied = maybe unsaturated (D.parenthesize paren) $
                  View.instantiate view fnP name (map fmt taken)
        unsaturated = D.sub' fnP $ go no View.high fn
      in
        (if inChain fn then id else D.group) $ case remaining of
          [] -> applied
          args -> D.group . D.parenthesize (p >= View.high) . D.docs $
            [ applied, D.breakable " "
            , D.nest "  " . D.group . D.delimit (D.breakable " ") $
              [ D.sub' p (go no (View.increase View.high) s) | (s,p) <- args ] ]
    LamsP' vs (body,bodyp) -> D.group . D.parenthesize (p /= View.low) $
      D.delimit (D.delimiter " ") (map (\(v,p) -> D.sub' p (sym v)) vs) `D.append`
      D.docs [ D.delimiter " ->", D.breakable " "
             , D.nest "  " $ D.sub' bodyp (go no View.low body) ]
    E.Ann' e t -> D.group . D.parenthesize (p /= View.low) $
                D.docs [ go no p e, D.delimiter " :", D.breakable " "
                       , D.nest "  " $ D.sub P.Annotation (type' ref t) ]
    E.Var' v -> sym v
    E.Lit' _ -> D.embed (Var.name $ op t)
    E.Blank' -> D.embed "_"
    E.Ref' r ->
      let op = ref r
      in (if View.arity (Symbol.annotation op) /= 0 then D.parenthesize True else id) $ sym op
    _ -> error $ "layout match failure " ++ show t

type' :: (Reference -> Symbol View.DFO) -> ViewableType -> Doc Text Path
type' ref t = go no View.low t
  where
  no = const False
  sym v = D.embed (Var.name v)
  op :: ViewableType -> Symbol View.DFO
  op t = case t of
    T.Lit' (T.Ref r) -> ref r
    T.Lit' l -> Symbol.annotate View.prefix . (\r -> Symbol.prefix r :: Symbol ()) . Text.pack . show $ l
    T.Var' v -> v
    -- todo: allow lambdas, etc, in head position
    _ -> Symbol.annotate View.prefix (Symbol.prefix "" :: Symbol ())
  go :: (ViewableType -> Bool) -> View.Precedence -> ViewableType -> Doc Text Path
  go inChain p t = case t of
    ArrowsPt' spine ->
      let arr = D.breakable " " `D.append` D.embed "-> "
      in D.parenthesize (p > View.low) . D.group . D.delimit arr $
          [ D.sub' p (go no (View.increase View.low) s) | (s,p) <- spine ]
    AppsPt' (fn,fnP) args ->
      let
        Symbol _ name view = op fn
        (taken, remaining) = splitAt (View.arity view) args
        fmt (child,path) = (\p -> D.sub' path (go (fn ==) p child), path)
        applied = maybe unsaturated (D.parenthesize (p > View.precedence view && View.arity view /= 0)) $
                  View.instantiate view fnP name (map fmt taken)
        unsaturated = D.sub' fnP $ go no View.high fn
      in
        (if inChain fn then id else D.group) $ case remaining of
          [] -> applied
          args -> D.group . D.parenthesize (p >= View.high) . D.docs $
            [ applied, D.breakable " "
            , D.nest "  " . D.group . D.delimit (D.breakable " ") $
              [ D.sub' p (go no (View.increase View.high) s) | (s,p) <- args ] ]
    ForallsPt' vs (body,bodyp) ->
      if p == View.low then D.sub' bodyp (go no p body)
      else D.parenthesize True . D.group $
           D.embed "∀ " `D.append`
           D.delimit (D.embed " ") (map (sym . fst) vs) `D.append`
           D.docs [D.embed ".", D.breakable " ", D.nest "  " $ D.sub' bodyp (go no View.low body)]
    T.Constrain' t _ -> go inChain p t
    T.Ann' t _ -> go inChain p t -- ignoring kind annotations for now
    T.Var' v -> sym v
    T.Lit' _ -> D.embed (Var.name $ op t)
    _ -> error $ "layout match failure"

-- helper functions and patterns

pattern LamsP' vs body <- (unLams' -> Just (vs, body))
pattern AppsP' f args <- (unApps' -> Just (f, args))
pattern AppsPt' f args <- (unAppst' -> Just (f, args))
pattern ArrowsPt' spine <- (unArrows' -> Just spine)
pattern ForallsPt' vs body <- (unForalls' -> Just (vs, body))

defaultSymbol :: Reference -> Symbol View.DFO
defaultSymbol (Reference.Builtin t) = Symbol.prefix t
defaultSymbol (Reference.Derived h) = Symbol.prefix (Text.cons '#' $ short h)
  where
  short h = Text.take 8 . Hash.base64 $ h

unLams' :: Term v -> Maybe ([(v, Path)], (Term v, Path))
unLams' (E.LamNamed' v body) = case unLams' body of
  Nothing -> Just ([(v, [P.Bound])], (body, [P.Body]))
  Just (vs, (body,bodyp)) -> Just ((v, [P.Bound]) : fmap (\(v,tl) -> (v,P.Body:tl)) vs, (body, P.Body:bodyp))
unLams' _ = Nothing

unApps' :: Term v -> Maybe ((Term v,Path), [(Term v,Path)])
unApps' t = addPaths <$> E.unApps t
  where
  addPaths (f,args) = case appPaths (length args) of
    (fp,ap) -> ((f,fp), args `zip` ap)

unArrows :: Type v -> Maybe [Type v]
unArrows t =
  case go t of [] -> Nothing; l -> Just l
  where
    go (T.Arrow' i o@(T.Arrow' _ _)) = i : go o
    go (T.Arrow' i o) = i : [o]
    go _ = []

unArrows' :: Type v -> Maybe [(Type v,Path)]
unArrows' t = addPaths <$> unArrows t
  where addPaths ts = ts `zip` arrowPaths (length ts)

arrowPaths :: Int -> [Path]
arrowPaths spineLength =
  (take (spineLength-1) $ iterate (P.Output:) [P.Input]) ++
  [replicate spineLength P.Output]

unForalls' :: Type v -> Maybe ([(v, Path)], (Type v, Path))
unForalls' (T.ForallNamed' v body) = case unForalls' body of
  Nothing -> Just ([(v, [P.Bound])], (body, [P.Body]))
  Just (vs, (body,bodyp)) -> Just ((v, [P.Bound]) : fmap (\(v,tl) -> (v,P.Body:tl)) vs, (body, P.Body:bodyp))
unForalls' _ = Nothing

unAppst :: Type v -> Maybe (Type v, [Type v])
unAppst t = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (T.App' i o) acc = go i (o:acc)
  go _ [] = []
  go fn args = fn:args

unAppst' :: Type v -> Maybe ((Type v,Path), [(Type v,Path)])
unAppst' t = addPaths <$> unAppst t
  where
  addPaths (f,args) = case appPaths (length args) of
    (fp,ap) -> ((f,fp), args `zip` ap)

appPaths :: Int -> (Path, [Path])
appPaths numArgs = (fnp, argsp)
  where
  fnp = replicate numArgs P.Fn
  argsp = reverse . take numArgs $ iterate (P.Fn:) [P.Arg]
