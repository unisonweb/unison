module Unison.Edit.Term (
  admissibleTypeOf, abstract, beta, eta, interpret, letFloat, locals, typeOf) where

import Control.Applicative
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
import qualified Unison.Syntax.Reference as R
import qualified Unison.Syntax.Type as T
import Unison.Type (synthesize)

-- | Interpret the given 'Action'
interpret :: (Applicative f, Monad f)
          => Eval (Noted f)
          -> (H.Hash -> Noted f E.Term)
          -> T.Env f
          -> P.Path -> Action E.Term -> E.Term -> Noted f E.Term
interpret eval readTerm readType loc f ctx = go f where
  go Abstract = abstract loc ctx
  go Eta = eta loc ctx
  go Beta = beta eval readTerm loc ctx
  go LetFloat = fst <$> letFloat loc ctx
  go WHNF = whnf eval readTerm loc ctx
  go (Apply e) = case P.modify loc (E.App e) ctx of
    Nothing -> N.failure $ invalid loc ctx
    Just e -> const e <$> synthesize readType e

invalid :: (Show a1, Show a) => a -> a1 -> String
invalid loc ctx = "invalid path " ++ show loc ++ " in:\n" ++ show ctx

-- | Pull the given path location in the term out into the outermost
-- function parameter
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
beta :: Applicative f
     => Eval (Noted f)
     -> (H.Hash -> Noted f E.Term)
     -> P.Path
     -> E.Term
     -> Noted f E.Term
beta eval readTerm loc ctx = case P.at' loc ctx of
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

-- | Return the type of all local variables introduced by the
-- given lambda, assuming that lambda has the annotated type
locals :: E.Term -> T.Type -> [(V.Var, T.Type)]
locals (E.Lam n body) (T.Arrow i o) = (n, i) : locals body o
locals ctx (T.Forall _ t) = locals ctx t
locals _ _ = []

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
