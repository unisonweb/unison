module Unison.Edit.Term (
  admissibleTypeOf, abstract, applications, step, eta, interpret, letFloat, locals, typeOf) where

import Control.Applicative
import Data.Traversable
import qualified Data.Set as S
import Unison.Edit.Term.Action as A
import qualified Unison.Edit.Term.Path as P
import qualified Unison.Edit.Term.Eval as Eval
import Unison.Edit.Term.Eval (Eval)
import qualified Unison.Syntax.Var as V
import qualified Unison.Note as N
import Unison.Note (Noted)
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Type as T
import Unison.Type (synthesize)

-- | Interpret the given 'Action'
interpret :: (Applicative f, Monad f)
          => Eval (Noted f)
          -> (H.Hash -> Noted f E.Term)
          -> P.Path -> Action -> E.Term -> Noted f E.Term
interpret eval readTerm loc f ctx = go f where
  go Abstract = abstract loc ctx
  go Eta = eta loc ctx
  go Step = step eval readTerm loc ctx
  go LetFloat = fst <$> letFloat loc ctx
  go WHNF = whnf eval readTerm loc ctx

invalid :: (Show a1, Show a) => a -> a1 -> String
invalid loc ctx = "invalid path " ++ show loc ++ " in:\n" ++ show ctx

-- | Pull the given path location in the term out into the outermost
-- function parameter
-- abstract f [[42]] => (\x -> f x) 42
-- abstract (\f x -> [[g x + 1]] * 42) => (\gx1 -> (\f x -> gx x * 42)) (\x -> g x + 1)
-- todo: if the location references any free variables, make these
-- function parameters of the
abstract :: Applicative f => P.Path -> E.Term -> Noted f E.Term
abstract loc ctx =
  N.liftMaybe (invalid loc ctx) $ E.lam1 <$> P.set' loc ctx

-- | Compute the allowed type of a replacement for a given subterm.
-- Example, in @\g -> map g [1,2,3]@, @g@ has an admissible type of
-- @forall r . Int -> r@, which means that an @Int -> Bool@, an
-- @Int -> String@, etc could all be substituted for @g@.
--
-- Algorithm works by replacing the subterm, @e@ with
-- @(f e)@, where @f@ is a fresh function parameter. We then
-- read off the type of @e@ from the inferred type of @f@.
admissibleTypeOf :: Applicative f
                 => T.Env f
                 -> P.Path
                 -> E.Term
                 -> Noted f T.Type
admissibleTypeOf synthLit loc ctx = case P.at' loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just (sub,replace) ->
    let ctx = E.lam1 $ \f -> replace (f `E.App` sub)
        go (T.Arrow (T.Arrow _ tsub) _) = tsub
        go (T.Forall n t) = T.Forall n (go t)
        go _ = error "impossible, f had better be a function"
    in go <$> synthesize synthLit ctx

-- | Beta-reduce the target, @(\x -> x+1) p@ becomes @p+1@.
-- This noops if target is not beta-reducible.
step :: Applicative f
     => Eval (Noted f)
     -> (H.Hash -> Noted f E.Term)
     -> P.Path
     -> E.Term
     -> Noted f E.Term
step eval readTerm loc ctx = case P.at' loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just (sub,replace) -> replace <$> Eval.step eval readTerm sub

-- | Eta-reduce the target; @\x -> f x@ becomes @f@.
-- This noops if target is not eta-reducible.
eta :: Applicative f => P.Path -> E.Term -> Noted f E.Term
eta loc ctx =
  N.liftMaybe (invalid loc ctx) $ P.modify loc E.etaReduce ctx


-- | Extract the given subterm into a let (implemented with a lambda)
-- floated out as far as possible while ensuring access to all the
-- variables used by the subterm. Examples:
--
-- * @\x -> f x 42@, 'letFloat' targeting @42@ (which has no free variables)
--   will float this all the way to the outside, yielding @\y -> (\x -> f x y) 42@.
-- * Targeting @f x@ in the same expression will float the let out to the
--   @\x -> ..@ lambda (since @f x@ references the variable bound by that lambda),
--   yielding @\x -> (\y -> y 42) (f x)@
--
-- This function returns a path to the floated subexpression (@42@ and @f x@ in
-- the above examples.)
letFloat :: Applicative f => P.Path -> E.Term -> Noted f (E.Term, P.Path)
letFloat loc ctx = case P.at loc ctx of
  Nothing  -> N.failure $ invalid loc ctx
  Just sub ->
    let
      free = E.freeVars sub
      minVar = if S.null free then Nothing else Just (S.findMin free)
      trimmedPath = P.trimToV minVar loc
      remainderPath = P.Path $ drop (length . P.elements $ trimmedPath) (P.elements loc)
      letBody = do
        body <- P.at trimmedPath ctx
        E.lam1 <$> P.set' remainderPath body
    in
      N.liftMaybe (invalid loc ctx) $ do
        body <- letBody
        ctx' <- P.set trimmedPath (body `E.App` sub) ctx
        loc' <- pure $ P.extend P.Arg trimmedPath
        pure (ctx', loc')

-- | Return the type of all local variables in scope at the given location
locals :: Applicative f => T.Env f -> P.Path -> E.Term -> Noted f [(V.Var, T.Type)]
locals synthLit path ctx | E.isClosed ctx = pushDown <$> lambdaTypes
  where
    pointsToLambda path = case P.at path ctx of
      Just (E.Lam _ _) -> True
      _ -> False

    lambdas :: [P.Path]
    lambdas = filter pointsToLambda (P.prefixes path)

    notedAt path expr = maybe (N.failure "invalid path") pure (P.at path expr)

    lambdaTypes = traverse t lambdas
      where t path = liftA2 extract (notedAt path ctx) (typeOf synthLit path ctx)

    -- not sure about this impl, or if it matters
    -- we prefer type information obtained higher in the syntax tree
    pushDown :: [[(V.Var, T.Type)]] -> [(V.Var, T.Type)]
    pushDown [] = []
    pushDown (env0 : tl) = env0 ++ pushDown (drop (length env0) tl)

    extract :: E.Term -> T.Type -> [(V.Var, T.Type)]
    extract (E.Lam n body) (T.Arrow i o) = (n, i) : extract body o
    extract ctx (T.Forall _ t) = extract ctx t
    extract _ _ = []
locals _ _ ctx =
  N.failure $ "Term.locals: term contains free variables - " ++ show (E.freeVars ctx)

-- | Produce `e`, `e _`, `e _ _`, `e _ _ _` and so on,
-- until the result is no longer a function type
applications :: E.Term -> T.Type -> [E.Term]
applications e t = e : go e t
  where
    go e (T.Forall _ t) = go e t
    go e (T.Arrow _ t) = let e' = E.App e E.Blank in go e' t
    go _ _ = []

-- | Compute the type of the given subterm, unconstrained as much
-- as possible by any local usages of that subterm. For example, in
-- @\g -> map g [1,2,3]@, @g@ will have a type of @forall r . Int -> r@,
-- and @map@ will have a type of @forall a b . (a -> b) -> [a] -> [b]@.
typeOf :: Applicative f => T.Env f -> P.Path -> E.Term -> Noted f T.Type
typeOf synthLit (P.Path []) ctx = synthesize synthLit ctx
typeOf synthLit loc ctx = case P.at' loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just (sub,replace) ->
    let ctx = E.lam1 $ \f -> replace (ksub f)
        -- we annotate `f` as returning `Number` so as not to introduce
        -- any new quantified variables in the inferred type
        -- copy the subtree so type is unconstrained by local usage
        ksub f = E.lam2 (\x _ -> x) `E.App` sub `E.App` (f `E.App` sub `E.Ann` T.Unit T.Number)
        go (T.Arrow (T.Arrow tsub _) _) = tsub
        go (T.Forall n t) = T.Forall n (go t)
        go _ = error "impossible, f had better be a function"
    in go <$> synthesize synthLit ctx

-- | Evaluate the given location to weak head normal form.
-- If the location contains any free variables, this noops.
whnf :: Applicative f
     => Eval (Noted f)
     -> (H.Hash -> Noted f E.Term)
     -> P.Path
     -> E.Term
     -> Noted f E.Term
whnf eval readTerm loc ctx = case P.at' loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just (sub,replace) | S.null (E.freeVars sub) -> replace <$> Eval.whnf eval readTerm sub
  Just _             | otherwise               -> pure ctx
