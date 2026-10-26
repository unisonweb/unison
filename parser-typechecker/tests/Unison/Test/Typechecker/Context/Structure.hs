{-# LANGUAGE OverloadedStrings #-}

-- | Checks the indexed context ("Unison.Typechecker.Context.Structure")
-- against a plain list of elements carrying the same operations, over random
-- sequences of appends, replacements, retractions and retention marks. After
-- every step the two must hold the same elements and answer every query the
-- typechecker asks the same way.
module Unison.Test.Typechecker.Context.Structure (test) where

import Control.Monad (replicateM_)
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import EasyTest
import Unison.Blank qualified as B
import Unison.Symbol (Symbol (..))
import Unison.Type qualified as Type
import Unison.Typechecker.Context.Structure (BadContext (..), Context, Element (..), context0)
import Unison.Typechecker.Context.Structure qualified as Ctx
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var qualified as Var

type V = Symbol

type TV = TypeVar.TypeVar (B.Blank ()) V

type Ty = Type.Type TV ()

type El = Element V ()

-- The reference: the elements in order, nothing else.
type Ref = [El]

test :: Test ()
test =
  scope "context.structure" $
    tests
      [ scope "random operations agree with a list" . replicateM_ 300 $ do
          n <- int' 10 80
          steps n context0 [] [],
        scope "a replacement is placed before the replaced element" $ do
          let a = pool !! 0
              b = pool !! 1
              c = pool !! 2
              d = pool !! 3
              e = pool !! 4
          ctx <- accepted (Ctx.extends [existential a, existential b, Marker c] context0)
          ctx' <- accepted (Ctx.replace b [existential d, Solved B.Blank b (mono (exVar d))] ctx)
          check [] "elements" (Ctx.elements ctx') [existential a, existential d, Solved B.Blank b (mono (exVar d)), Marker c]
          check [] "ordered a d" (Ctx.ordered ctx' a d) True
          check [] "ordered d b" (Ctx.ordered ctx' d b) True
          check [] "ordered d c" (Ctx.ordered ctx' d c) True
          check [] "ordered c d" (Ctx.ordered ctx' c d) False
          -- and nested: before d, which was itself inserted before b
          ctx'' <- accepted (Ctx.replace d [existential e, Solved B.Blank d (mono (exVar e))] ctx')
          check [] "nested elements" (map Ctx.variable (Ctx.elements ctx'')) [a, e, d, b, c]
          check [] "ordered a e" (Ctx.ordered ctx'' a e) True
          check [] "ordered e d" (Ctx.ordered ctx'' e d) True
          check [] "ordered e b" (Ctx.ordered ctx'' e b) True
          ok
      ]
  where
    accepted :: Either (Ctx.Rejected V ()) (Context V ()) -> Test (Context V ())
    accepted = either (\(Ctx.Rejected _ e bad) -> crash ("rejected " ++ show e ++ ": " ++ show bad)) pure

-- Runs `k` random operations against both implementations, comparing after each.
steps :: Int -> Context V () -> Ref -> [String] -> Test ()
steps 0 _ _ _ = ok
steps k ctx ref trace = do
  op <- randomOp ref
  let trace' = show op : trace
      actual = step op ctx
      expected = refStep op ref
  check trace' "outcome" (fmap (\(c, seg) -> (Ctx.elements c, seg)) actual) expected
  check trace' "blanks" (fmap (\(c, seg) -> (map blankOf (Ctx.elements c), map blankOf seg)) actual) (fmap (\(r, seg) -> (map blankOf r, map blankOf seg)) expected)
  case (actual, expected) of
    (Right (ctx', _), Right (ref', _)) -> do
      queries trace' ctx' ref'
      steps (k - 1) ctx' ref' trace'
    _ -> steps (k - 1) ctx ref trace'

-- Every question the typechecker asks of a context.
queries :: [String] -> Context V () -> Ref -> Test ()
queries trace ctx ref = do
  check trace "inconsistent" (Ctx.inconsistent ctx) (any isInconsistent ref)
  for_ pool \v -> do
    check trace ("lookupElement " ++ show v) (Ctx.lookupElement v ctx) (boundIn ref v)
    check trace ("existentialIn " ++ show v) (Ctx.existentialIn Ctx.AtEnd v ctx) (refExistential ref v)
    check trace ("universalIn " ++ show v) (Ctx.universalIn Ctx.AtEnd v ctx) (refUniversal ref v)
    check trace ("lookupSolved " ++ show v) (Type.getPolytype <$> Ctx.lookupSolved ctx v) (refSolved ref v)
    check trace ("lookupAnn " ++ show v) (Ctx.lookupAnn ctx v) (refAnn ref v)
    for_ pool \w ->
      check trace ("ordered " ++ show v ++ " " ++ show w) (Ctx.ordered ctx v w) (refOrdered ref v w)
    -- the scope just before v's element
    case Ctx.scopeOf v ctx of
      Nothing -> check trace ("scopeOf " ++ show v) (boundIn ref v) Nothing
      Just sc -> for_ pool \w -> do
        let before = takeWhile ((/= Just v) . Ctx.binds) ref
        check trace ("existentialIn (before " ++ show v ++ ") " ++ show w) (Ctx.existentialIn sc w ctx) (refExistential before w)
        check trace ("universalIn (before " ++ show v ++ ") " ++ show w) (Ctx.universalIn sc w ctx) (refUniversal before w)
  check trace "annotations" (List.sort (Ctx.annotations ctx)) (List.sort [(v, l, t) | Ann v l t <- ref])
  t <- randomType ref 3
  check trace ("apply " ++ show t) (Ctx.apply ctx t) (refApply ref t)

check :: (Eq a, Show a) => [String] -> String -> a -> a -> Test ()
check trace what actual expected
  | actual == expected = pure ()
  | otherwise =
      crash $
        what
          ++ ": the structure says "
          ++ show actual
          ++ " but the reference says "
          ++ show expected
          ++ "\nafter the operations (latest first):\n"
          ++ unlines trace

-- Operations ------------------------------------------------------------------

data Op
  = Extend El
  | Replace V [El]
  | Retract V
  | Mark (Set V)
  deriving (Show)

step :: Op -> Context V () -> Either BadContext (Context V (), [El])
step (Extend e) ctx = (,[]) <$> Ctx.extend e ctx
step (Replace v es) ctx = either (\(Ctx.Rejected _ _ bad) -> Left bad) (Right . (,[])) (Ctx.replace v es ctx)
step (Retract v) ctx = Right (maybe (ctx, []) id (Ctx.retract v ctx))
step (Mark vs) ctx = Right (Ctx.markRetained vs ctx, [])

refStep :: Op -> Ref -> Either BadContext (Ref, [El])
refStep (Extend e) ref = (,[]) <$> refExtend e ref
refStep (Replace v es) ref = (,[]) <$> refReplace v es ref
refStep (Retract v) ref = Right (maybe (ref, []) id (refRetract v ref))
refStep (Mark vs) ref = Right (map (refMark vs) ref, [])

-- The reference implementation ------------------------------------------------

boundIn :: Ref -> V -> Maybe El
boundIn ref v = List.find ((== Just v) . Ctx.binds) ref

refExistential :: Ref -> V -> Bool
refExistential ref v = maybe False isExistential (boundIn ref v)

refUniversal :: Ref -> V -> Bool
refUniversal ref v = maybe False isUniversal (boundIn ref v)

refSolved :: Ref -> V -> Maybe Ty
refSolved ref v = case boundIn ref v of
  Just (Solved _ _ t) -> Just (Type.getPolytype t)
  _ -> Nothing

refAnn :: Ref -> V -> Maybe Ty
refAnn ref v = case boundIn ref v of
  Just (Ann _ _ t) -> Just t
  _ -> Nothing

refOrdered :: Ref -> V -> V -> Bool
refOrdered ref v w = case (index v, index w) of
  (Just i, Just j) -> i < j && refExistential ref v
  _ -> False
  where
    index x = List.findIndex ((== Just x) . Ctx.binds) ref

-- Checks an element placed after `scope`, in a context whose other elements
-- are `whole`.
refCheck :: Ref -> Ref -> El -> Maybe BadContext
refCheck whole scope e
  | Just v <- Ctx.binds e, any ((== Just v) . Ctx.binds) whole = Just Shadowing
  | Refined v _ <- e, not (refUniversal scope v) = Just UnboundUn
  | otherwise = listToMaybe (mapMaybe unbound (Set.toList (freeVarsOf e)))
  where
    unbound (TypeVar.Existential _ v)
      | refExistential scope v = Nothing
      | otherwise = Just UnboundEx
    unbound (TypeVar.Universal v)
      | refUniversal scope v = Nothing
      | otherwise = Just UnboundUn

refExtend :: El -> Ref -> Either BadContext Ref
refExtend e ref = maybe (Right (ref ++ [e])) Left (refCheck ref ref e)

refReplace :: V -> [El] -> Ref -> Either BadContext Ref
refReplace v es ref = case break ((== Just v) . Ctx.binds) ref of
  (_, []) -> Right ref
  (pre, _ : post)
    | not (any ((== Just v) . Ctx.binds) es) -> Left NotRebound
    | otherwise -> go pre es
    where
      go acc [] = Right (acc ++ post)
      go acc (e : rest) = case refCheck (acc ++ post) acc e of
        Just bad -> Left bad
        Nothing -> go (acc ++ [e]) rest

refRetract :: V -> Ref -> Maybe (Ref, [El])
refRetract v ref = case break ((== Just v) . Ctx.binds) ref of
  (_, []) -> Nothing
  (pre, _ : post) -> Just (pre, post)

refMark :: Set V -> El -> El
refMark keep e = case e of
  Var (TypeVar.Existential B.Blank v) | Set.member v keep -> Var (TypeVar.Existential B.Retain v)
  Solved B.Blank v t | Set.member v keep -> Solved B.Retain v t
  _ -> e

-- The rightmost solution and refinement of each variable, applied by the
-- same substitution the structure uses.
refApply :: Ref -> Ty -> Ty
refApply = Ctx.applySegment

isExistential :: El -> Bool
isExistential (Var (TypeVar.Existential _ _)) = True
isExistential (Solved {}) = True
isExistential _ = False

isUniversal :: El -> Bool
isUniversal (Var (TypeVar.Universal _)) = True
isUniversal _ = False

isInconsistent :: El -> Bool
isInconsistent (Inconsistent _) = True
isInconsistent _ = False

blankOf :: El -> Maybe (B.Blank ())
blankOf (Var (TypeVar.Existential b _)) = Just b
blankOf (Solved b _ _) = Just b
blankOf _ = Nothing

freeVarsOf :: El -> Set TV
freeVarsOf (Solved _ _ t) = Type.freeVars (Type.getPolytype t)
freeVarsOf (Ann _ _ t) = Type.freeVars t
freeVarsOf (Refined _ t) = Type.freeVars t
freeVarsOf _ = Set.empty

-- Generators ------------------------------------------------------------------

pool :: [V]
pool = [Symbol i (Var.User (Text.pack ("v" ++ show i))) | i <- [0 .. 11]]

existential :: V -> El
existential v = Var (TypeVar.Existential B.Blank v)

universal :: V -> El
universal v = Var (TypeVar.Universal v)

exVar :: V -> Ty
exVar v = Type.var () (TypeVar.Existential B.Blank v)

unVar :: V -> Ty
unVar v = Type.var () (TypeVar.Universal v)

mono :: Ty -> Type.Monotype TV ()
mono = Type.Monotype

-- A variable, usually one that is not bound, so that most operations are
-- accepted and the context grows.
freshish :: Ref -> Test V
freshish ref = do
  k <- int' 0 9
  let unbound = [v | v <- pool, boundIn ref v == Nothing]
  if k < 8 && not (null unbound) then pick unbound else pick pool

randomBlank :: Test (B.Blank ())
randomBlank = pick [B.Blank, B.Blank, B.Recorded (B.Placeholder () "p"), B.Retain]

-- A type over the variables bound in `ref`, now and then over an unbound one.
randomType :: Ref -> Int -> Test Ty
randomType ref depth = do
  k <- int' 0 (if depth == 0 then 2 else 5)
  case k of
    0 -> exVar <$> boundVar isExistential
    1 -> unVar <$> boundVar isUniversal
    2 -> do
      v <- pick pool
      pick [exVar v, unVar v]
    3 -> Type.arrow () <$> randomType ref (depth - 1) <*> randomType ref (depth - 1)
    4 -> do
      q <- pick pool
      body <- randomType ref (depth - 1)
      pure (Type.forAll () (TypeVar.Universal q) (Type.arrow () (unVar q) body))
    _ -> Type.app () <$> randomType ref (depth - 1) <*> randomType ref (depth - 1)
  where
    boundVar p = case [Ctx.variable e | e <- ref, p e] of
      [] -> pick pool
      vs -> pick vs

randomMono :: Ref -> Test (Type.Monotype TV ())
randomMono ref = mono <$> go 2
  where
    go :: Int -> Test Ty
    go depth = do
      k <- int' 0 (if depth == (0 :: Int) then 2 else 3)
      case k of
        0 -> exVar <$> boundVar isExistential
        1 -> unVar <$> boundVar isUniversal
        2 -> do
          v <- pick pool
          pick [exVar v, unVar v]
        _ -> Type.arrow () <$> go (depth - 1) <*> go (depth - 1)
    boundVar p = case [Ctx.variable e | e <- ref, p e] of
      [] -> pick pool
      vs -> pick vs

randomElement :: Ref -> Test El
randomElement ref = do
  k <- int' 0 7
  v <- freshish ref
  case k of
    0 -> pure (universal v)
    1 -> existential v <$ pure ()
    2 -> Var . (`TypeVar.Existential` v) <$> randomBlank
    3 -> Solved <$> randomBlank <*> pure v <*> randomMono ref
    4 -> Ann v () <$> randomType ref 3
    5 -> do
      u <- case [Ctx.variable e | e <- ref, isUniversal e] of
        [] -> pick pool
        us -> pick us
      -- A refinement's type is closed: a refinement to a type that reaches
      -- the refined universal again (directly, or through an existential
      -- solved to it) would make `apply` loop, and the typechecker never
      -- makes one, so the generator doesn't either.
      Refined u <$> pick [Type.nat (), Type.text (), Type.arrow () (Type.nat ()) (Type.text ())]
    6 -> pure (Inconsistent v)
    _ -> pure (Marker v)

randomOp :: Ref -> Test Op
randomOp ref = do
  k <- int' 0 11
  case k of
    _ | k <= 5 -> Extend <$> randomElement ref
    6 -> instantiate
    7 -> instantiate
    8 -> solve
    9 -> case mapMaybe Ctx.binds ref of
      [] -> Extend <$> randomElement ref
      vs -> Retract <$> pick vs
    10 -> do
      n <- int' 0 4
      Mark . Set.fromList <$> listOf n (pick pool)
    _ -> do
      v <- pick pool
      Replace v . pure . existential <$> freshish ref
  where
    unsolved = [v | Var (TypeVar.Existential _ v) <- ref]
    -- as `instantiateL` does: fresh existentials before the solved one
    instantiate = case unsolved of
      [] -> Extend <$> randomElement ref
      vs -> do
        v <- pick vs
        a <- freshish ref
        b <- freshish ref
        shape <- int' 0 4
        pure . Replace v $ case shape of
          0 -> [existential a, existential b, Solved B.Blank v (mono (Type.arrow () (exVar a) (exVar b)))]
          1 -> [existential a, Solved B.Blank v (mono (exVar a))]
          2 -> [existential a, existential b] -- forgets to bind v again
          3 -> [existential a, Solved B.Blank v (mono (exVar b))] -- b is not bound before v
          _ -> [Solved B.Blank v (mono (exVar a)), existential a] -- a is bound after its use
    solve = case unsolved of
      [] -> Extend <$> randomElement ref
      vs -> do
        v <- pick vs
        -- a type over everything bound, which may well mention something after v
        t <- randomMono ref
        b <- randomBlank
        pure (Replace v [Solved b v t])
