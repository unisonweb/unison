{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Term where

import Control.Applicative
import Control.Monad
import Data.Aeson.TH
import Data.Aeson (ToJSON, FromJSON)
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector, (!?))
import GHC.Generics
import Prelude.Extras (Eq1(..), Show1(..))
import Text.Show
import Unison.Doc (Doc)
import Unison.Hash (Hash)
import Unison.Hashable (Hashable, Hashable1)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Aeson as Aeson
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Dimensions as Dimensions
import qualified Unison.Doc as D
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as Hashable
import qualified Unison.JSON as J
import qualified Unison.Reference as Reference
import qualified Unison.Symbol as Symbol
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import qualified Unison.View as View

-- | Literals in the Unison language
data Literal
  = Number Double
  | Text Text
  deriving (Eq,Ord,Generic)

instance Hashable Literal where
  tokens (Number d) = [Hashable.Tag 0, Hashable.Double d]
  tokens (Text txt) = [Hashable.Tag 1, Hashable.Text txt]

-- | Base functor for terms in the Unison language
data F v a
  = Lit Literal
  | Blank -- An expression that has not been filled in, has type `forall a . a`
  | Ref Reference
  | App a a
  | Ann a (Type v)
  | Vector (Vector a)
  | Lam a
  -- Invariant: let rec blocks have an outer ABT.Cycle which introduces as many
  -- variables as there are bindings
  | LetRec [a] a
  | Let a a
  deriving (Eq,Foldable,Functor,Generic1,Traversable)

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type AnnotatedTerm v a = ABT.Term (F v) v a

-- | Terms are represented as ABTs over the base functor F, with variables in `v`
type Term v = AnnotatedTerm v ()

-- nicer pattern syntax

pattern Var' v <- ABT.Var' v
pattern Lit' l <- (ABT.out -> ABT.Tm (Lit l))
pattern Number' n <- Lit' (Number n)
pattern Text' s <- Lit' (Text s)
pattern Blank' <- (ABT.out -> ABT.Tm Blank)
pattern Ref' r <- (ABT.out -> ABT.Tm (Ref r))
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))
pattern Apps' f args <- (unApps -> Just (f, args))
pattern AppsP' f args <- (unApps' -> Just (f, args))
pattern Ann' x t <- (ABT.out -> ABT.Tm (Ann x t))
pattern Vector' xs <- (ABT.out -> ABT.Tm (Vector xs))
pattern Lam' v body <- (ABT.out -> ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body))))
pattern LamsP' vs body <- (unLams' -> Just (vs, body))
pattern Let1' v b e <- (ABT.out -> ABT.Tm (Let b (ABT.Abs' v e)))
pattern Lets' bs e <- Let' bs e _ False
pattern Let' bs e relet rec <- (unLets -> Just (bs,e,relet,rec))
pattern LetRec' bs e <- (unLetRec -> Just (bs,e))

fresh :: Var v => Term v -> v -> v
fresh = ABT.fresh

-- some smart constructors

var :: v -> Term v
var = ABT.var

var' :: Var v => Text -> Term v
var' = var . ABT.v'

ref :: Ord v => Reference -> Term v
ref r = ABT.tm (Ref r)

num :: Ord v => Double -> Term v
num = lit . Number

lit :: Ord v => Literal -> Term v
lit l = ABT.tm (Lit l)

blank :: Ord v => Term v
blank = ABT.tm Blank

app :: Ord v => Term v -> Term v -> Term v
app f arg = ABT.tm (App f arg)

apps :: Ord v => Term v -> [Term v] -> Term v
apps f = foldl' app f

ann :: Ord v => Term v -> Type v -> Term v
ann e t = ABT.tm (Ann e t)

vector :: Ord v => [Term v] -> Term v
vector es = ABT.tm (Vector (Vector.fromList es))

vector' :: Ord v => Vector (Term v) -> Term v
vector' es = ABT.tm (Vector es)

lam :: Ord v => v -> Term v -> Term v
lam v body = ABT.tm (Lam (ABT.abs v body))

lam' :: Var v => [Text] -> Term v -> Term v
lam' vs body = foldr lam body (map ABT.v' vs)

-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec :: Ord v => [(v,Term v)] -> Term v -> Term v
letRec [] e = e
letRec bindings e = ABT.cycle (foldr ABT.abs z (map fst bindings))
  where
    z = ABT.tm (LetRec (map snd bindings) e)

letRec' :: Var v => [(Text, Term v)] -> Term v -> Term v
letRec' bs e = letRec [(ABT.v' name, b) | (name,b) <- bs] e

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
let1 :: Ord v => [(v,Term v)] -> Term v -> Term v
let1 bindings e = foldr f e bindings
  where
    f (v,b) body = ABT.tm (Let b (ABT.abs v body))

let1' :: Var v => [(Text,Term v)] -> Term v -> Term v
let1' bs e = let1 [(ABT.v' name, b) | (name,b) <- bs ] e

-- | Satisfies
--   `unLets (letRec bs e) == Just (bs, e, letRec, True)` and
--   `unLets (let' bs e) == Just (bs, e, let', False)`
-- Useful for writing code agnostic to whether a let block is recursive or not.
unLets :: Ord v => Term v -> Maybe ([(v,Term v)], Term v, [(v,Term v)] -> Term v -> Term v, Bool)
unLets e =
  (f letRec True <$> unLetRec e) <|> (f let1 False <$> unLet e)
  where f mkLet rec (bs,e) = (bs,e,mkLet,rec)

-- | Satisfies `unLetRec (letRec bs e) == Just (bs, e)`
unLetRec :: Term v -> Maybe ([(v, Term v)], Term v)
unLetRec (ABT.Cycle' vs (ABT.Tm' (LetRec bs e)))
  | length vs == length vs = Just (zip vs bs, e)
unLetRec _ = Nothing

-- | Satisfies `unLet (let' bs e) == Just (bs, e)`
unLet :: Term v -> Maybe ([(v, Term v)], Term v)
unLet t = fixup (go t) where
  go (ABT.out -> ABT.Tm (Let b (ABT.Abs' v t))) =
    case go t of (env,t) -> ((v,b):env, t)
  go t = ([], t)
  fixup ([], _) = Nothing
  fixup bst = Just bst

unApps :: Term v -> Maybe (Term v, [Term v])
unApps t = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc = go i (o:acc)
  go _ [] = []
  go fn args = fn:args

unApps' :: Term v -> Maybe ((Term v,Path), [(Term v,Path)])
unApps' t = addPaths <$> unApps t
  where
  addPaths (f,args) = case appPaths (length args) of
    (fp,ap) -> ((f,fp), args `zip` ap)

appPaths :: Int -> (Path, [Path])
appPaths numArgs = (fnp, argsp)
  where
  fnp = replicate numArgs Fn
  argsp = reverse . take numArgs $ iterate (Fn:) [Arg]

unLams' :: Term v -> Maybe ([(v, Path)], (Term v, Path))
unLams' (Lam' v body) = case unLams' body of
  Nothing -> Just ([(v, [])], (body, [Body])) -- todo, need a path for forall vars
  Just (vs, (body,bodyp)) -> Just ((v, []) : vs, (body, Body:bodyp))
unLams' _ = Nothing

dependencies' :: Ord v => Term v -> Set Reference
dependencies' t = Set.fromList . Writer.execWriter $ ABT.visit' f t
  where f t@(Ref r) = Writer.tell [r] *> pure t
        f t = pure t

dependencies :: Ord v => Term v -> Set Hash
dependencies e = Set.fromList [ h | Reference.Derived h <- Set.toList (dependencies' e) ]

countBlanks :: Ord v => Term v -> Int
countBlanks t = Monoid.getSum . Writer.execWriter $ ABT.visit' f t
  where f Blank = Writer.tell (Monoid.Sum (1 :: Int)) *> pure Blank
        f t = pure t

data PathElement
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda or let
  | Binding !Int -- ^ Points at a particular binding in a let
  | Index !Int -- ^ Points at the index of a vector
  | Annotation Type.Path -- ^ Points into the type of an `Ann`
  deriving (Eq,Ord,Show)

type Path = [PathElement]

-- | Use a @PathElement@ to compute one step into an @F Term v@ subexpression
focus1 :: Ord v => PathElement -> ABT.Focus1 (F v) (Term v)
focus1 Fn (App f x) = Just (f, \f -> App f x)
focus1 Arg (App f x) = Just (x, \x -> App f x)
focus1 Body (Lam (ABT.Abs' v body)) = Just (body, Lam . ABT.abs v)
focus1 Body (Let b (ABT.Abs' v body)) = Just (body, Let b . ABT.abs v)
focus1 Body (LetRec bs body) = Just (body, LetRec bs)
focus1 (Binding i) (Let b body) | i <= 0 = Just (b, \b -> Let b body)
focus1 (Binding i) (LetRec bs body) =
  listToMaybe (drop i bs)
  >>= \b -> Just (b, \b -> LetRec (take i bs ++ [b] ++ drop (i+1) bs) body)
focus1 (Index i) (Vector vs) =
  vs !? i >>= \v -> Just (v, \v -> Vector (Vector.update vs (Vector.singleton (i,v))))
focus1 (Annotation pt) (Ann e t) = Just (e, \e -> Ann e t) -- todo: revisit
focus1 _ _ = Nothing

-- | Return the list of all prefixes of the input path
pathPrefixes :: Path -> [Path]
pathPrefixes = inits

-- | Add an element onto the end of this 'Path'
pathExtend :: PathElement -> Path -> Path
pathExtend e p = p ++ [e]

at :: Ord v => Path -> Term v -> Maybe (Term v)
at p t = ABT.at (map focus1 p) t

-- | Given a variable and a path, find the longest prefix of the path
-- which points to a term where the variable is unbound. Example:
-- `\f -> \x -> f {x}` would return the path pointing to `{\x -> f x}`
introducedAt :: Ord v => v -> Path -> Term v -> Maybe Path
introducedAt v path t = f <$> ABT.introducedAt v (map focus1 path) t where
  f p = take (length p) path

modify :: Ord v => (Term v -> Term v) -> Path -> Term v -> Maybe (Term v)
modify f p t = ABT.modify f (map focus1 p) t

focus :: Ord v => Path -> Term v -> Maybe (Term v, Term v -> Term v)
focus p t = ABT.focus (map focus1 p) t

parent :: Path -> Maybe Path
parent [] = Nothing
parent p = Just (init p)

parent' :: Path -> Path
parent' = fromMaybe [] . parent

bindingAt :: Ord v => Path -> Term v -> Maybe (v, Term v)
bindingAt [] _ = Nothing
bindingAt path t = do
  parentPath <- parent path
  Let1' v b _ <- at parentPath t
  pure (v, b)

-- | Convert all 'Ref' constructors to the corresponding term
link :: (Applicative f, Monad f, Var v) => (Hash -> f (Term v)) -> Term v -> f (Term v)
link env e =
  let ds = map (\h -> (h, link env =<< env h)) (Set.toList (dependencies e))
      sub e (h, ft) = replace <$> ft
        where replace t = ABT.replace t ((==) rt) e
              rt = ref (Reference.Derived h)
  in foldM sub e ds

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Var v => Term v -> Term v
betaReduce (App' (Lam' n body) arg) = ABT.subst arg n body
betaReduce e = e

type ViewableTerm = Term (Symbol View.DFO)

toString :: ViewableTerm -> String
toString t = D.formatText (Dimensions.Width 80) (view Type.defaultSymbol t)

view :: (Reference -> Symbol View.DFO) -> ViewableTerm -> Doc Text Path
view ref t = go no View.low t where
  no = const False
  sym v = D.embed (Var.name v)
  op t = case t of
    Lit' l -> Symbol.annotate View.prefix . (\r -> Symbol.prefix r :: Symbol ()) . Text.pack . show $ l
    Var' v -> v
    _ -> Symbol.annotate View.prefix (Symbol.prefix "" :: Symbol ())
  formatBinding :: Path -> Symbol View.DFO -> ViewableTerm -> Doc Text Path
  formatBinding path name body = case body of
    LamsP' vs (body,bodyp) ->
      let lhs = fmap fixup $ go no View.low (apps (var name) (map (var . fst) vs))
          fixup _ = [] -- todo, could use paths to individual variables
          rhs = D.sub' bodyp $ go no View.low body
      in D.group . D.sub' path $ D.docs [lhs, D.embed " =", D.breakable " ", D.nest "  " rhs]
    _ -> D.sub' path $ D.docs [sym name, D.embed " =", D.breakable " ", D.nest "  " $ go no View.low body ]
  go :: (ViewableTerm -> Bool) -> View.Precedence -> ViewableTerm -> Doc Text Path
  go inChain p t = case t of
    Lets' bs e ->
      let
        pe = replicate (length bs) Body
        bps = tail (tails pe)
        formattedBs = [ formatBinding bp name b | ((name,b), bp) <- bs `zip` bps ]
      in D.group $ D.docs [D.embed "let", D.breakable " "] `D.append`
                   D.nest "  " (D.delimit (D.breakable "; ") formattedBs) `D.append`
                   D.docs [ D.breakable " ", D.embed "in", D.breakable " "
                          , D.sub' pe . D.nest "  " $ go no View.low e ]
    LetRec' bs e ->
      let
        bps = map Binding [0 .. length bs - 1]
        formattedBs = [ formatBinding [bp] name b | ((name,b), bp) <- bs `zip` bps ]
      in D.group $ D.docs [D.embed "let rec", D.breakable " "] `D.append`
                   D.nest "  " (D.delimit (D.breakable "; ") formattedBs) `D.append`
                   D.docs [ D.breakable " ", D.embed "in", D.breakable " "
                          , D.sub Body . D.nest "  " $ go no View.low e ]
    AppsP' (fn,fnP) args ->
      let
        Symbol.Symbol _ name view = op fn
        (taken, remaining) = splitAt (View.arity view) args
        fmt (child,path) = (\p -> D.sub' path (go (fn ==) p child), path)
        applied = fromMaybe unsaturated (View.instantiate view fnP name (map fmt taken))
        unsaturated = D.sub' fnP $ go no View.high fn
      in
        (if inChain fn then id else D.group) $ case remaining of
          [] -> applied
          args -> D.parenthesize (p > View.high) . D.group . D.docs $
            [ applied, D.breakable " "
            , D.nest "  " . D.group . D.delimit (D.breakable " ") $
              [ D.sub' p (go no (View.increase View.high) s) | (s,p) <- args ] ]
    LamsP' vs (body,bodyp) ->
      if p == View.low then D.sub' bodyp (go no p body)
      else D.parenthesize True . D.group $
           D.delimit (D.embed " ") (map (sym . fst) vs) `D.append`
           D.docs [D.embed "->", D.breakable " ", D.nest "  " $ D.sub' bodyp (go no View.low body)]
    Vector' vs ->
      let
        fmt i v = D.nest "  " . D.sub (Index i) $ go no View.low v
        subs = [ fmt i v | (v,i) <- Vector.toList vs `zip` [0..] ]
      in D.group . D.docs $
           [ D.embed "[ "
           , D.delimit (D.breakable ", ") subs
           , D.embed " ]" ]
    Ann' e t -> D.group . D.parenthesize (p /= View.low) $
                D.docs [ go no p e, D.embed " :", D.breakable " "
                       , D.nest "  " $ (\p -> [Annotation p]) <$> Type.view ref t ]
    Var' v -> sym v
    Lit' _ -> D.embed (Var.name $ op t)
    _ -> error $ "layout match failure"

instance Var v => Hashable1 (F v) where
  hash1 hashCycle hash e =
    let
      (tag, hashed, varint) = (Hashable.Tag, Hashable.Hashed, Hashable.VarInt)
      hashToken :: (Hashable.Hash h, Hashable t) => t -> Hashable.Token h
      hashToken = Hashable.Hashed . Hashable.hash'
    in case e of
      -- So long as `Reference.Derived` ctors are created using the same hashing
      -- function as is used here, this case ensures that references are 'transparent'
      -- wrt hash and hashing is unaffected by whether expressions are linked.
      -- So for example `x = 1 + 1` and `y = x` hash the same.
      Ref (Reference.Derived h) -> Hashable.fromBytes (Hash.toBytes h)
      -- Note: start each layer with leading `1` byte, to avoid collisions with
      -- types, which start each layer with leading `0`. See `Hashable1 Type.F`
      _ -> Hashable.hash $ tag 1 : case e of
        Lit l -> [tag 0, hashToken l]
        Blank -> [tag 1]
        Ref (Reference.Builtin name) -> [tag 2, hashToken name]
        Ref (Reference.Derived _) -> error "handled above, but GHC can't figure this out"
        App a a2 -> [tag 3, hashed (hash a), hashed (hash a2)]
        Ann a t -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
        Vector as -> tag 5 : varint (Vector.length as) : map (hashed . hash) (Vector.toList as)
        Lam a -> [tag 6, hashed (hash a) ]
        -- note: we use `hashCycle` to ensure result is independent of let binding order
        LetRec as a -> case hashCycle as of
          (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
        -- here, order is significant, so don't use hashCycle
        Let b a -> [tag 8, hashed (hash b), hashed (hash a)]

-- mostly boring serialization code below ...

deriveJSON defaultOptions ''Literal

instance Var v => Eq1 (F v) where (==#) = (==)
instance Var v => Show1 (F v) where showsPrec1 = showsPrec

deriveToJSON defaultOptions ''F
instance (Ord v, FromJSON v, FromJSON r) => FromJSON (F v r) where
  parseJSON = $(mkParseJSON defaultOptions ''F)

instance ToJSON v => J.ToJSON1 (F v) where toJSON1 f = Aeson.toJSON f
instance (Ord v, FromJSON v) => J.FromJSON1 (F v) where parseJSON1 j = Aeson.parseJSON j

deriveJSON defaultOptions ''PathElement

instance Show Literal where
  show (Text t) = show t
  show (Number n) = case floor n of
    m | fromIntegral m == n -> show (m :: Int)
    _ -> show n

instance (Var v, Show a) => Show (F v a) where
  showsPrec p fa = go p fa where
    go _ (Lit l) = showsPrec 0 l
    go p (Ann t k) = showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go _ (Lam body) = showParen True (s"λ " <> showsPrec 0 body)
    go _ (Vector vs) = showListWith (showsPrec 0) (Vector.toList vs)
    go _ Blank = s"_"
    go _ (Ref r) = showsPrec 0 r
    (<>) = (.)
    s = showString
