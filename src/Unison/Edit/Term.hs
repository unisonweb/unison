module Unison.Edit.Term (interpret, abstract, eta, beta, letFloat) where

import Control.Applicative
import qualified Data.Set as S
import Unison.Edit.Term.Action as A
import qualified Unison.Edit.Term.Path as P
import Unison.Edit.Term.Eval as Eval
import qualified Unison.Syntax.Hash as H
import qualified Unison.Note as N
import Unison.Note (Noted)
import qualified Unison.Syntax.Var
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Type as T
import Unison.Type (synthesize)

-- | Interpret the given 'Action'
interpret :: (Applicative f, Monad f)
          => Eval f -> P.Path -> Action E.Term -> E.Term -> Noted f E.Term
interpret eval loc f ctx = go f where
  go Abstract = abstract loc ctx
  go Eta = eta loc ctx
  go Beta = beta eval loc ctx
  go LetFloat = fst <$> letFloat loc ctx
  go _ = error "todo: Apply, WHNF, HNF, Apply will have to invoke typechecker"

invalid :: (Show a1, Show a) => a -> a1 -> String
invalid loc ctx = "invalid path " ++ show loc ++ " in:\n" ++ show ctx

-- | Pull the given path location in the term out into the outermost
-- function parameter
abstract :: Applicative f => P.Path -> E.Term -> Noted f E.Term
abstract loc ctx =
  N.liftMaybe (invalid loc ctx) $ E.lam1 <$> P.set' loc ctx

-- | Eta-reduce the target; @\x -> f x@ becomes @f@.
-- This noops if target is not eta-reducible.
eta :: Applicative f => P.Path -> E.Term -> Noted f E.Term
eta loc ctx =
  N.liftMaybe (invalid loc ctx) $ P.modify loc E.etaReduce ctx

-- | Beta-reduce the target, @(\x -> x+1) p@ becomes @p+1@.
-- This noops if target is not beta-reducible.
beta :: Applicative f => Eval f -> P.Path -> E.Term -> Noted f E.Term
beta eval loc ctx = case P.at' loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just (sub,replace) -> replace <$> step eval sub

-- | Compute the allowed type of a replacement for a given subterm.
-- Example, in @\g -> map g [1,2,3]@, @g@ has an admissible type of
-- @forall r . Int -> r@, which means that an @Int -> Bool@, an
-- @Int -> String@, etc could all be substituted for @g@.
--
-- Algorithm works by replacing the subterm, @e@ with
-- @const e (f e)@, where @f@ is a fresh function parameter. We then
-- read off the type of @e@ from the inferred type of @f@.
admissibleTypeAt :: Applicative f
                 => (H.Hash -> Noted f T.Type)
                 -> P.Path
                 -> E.Term
                 -> Noted f T.Type
admissibleTypeAt synthLit (P.Path []) ctx = synthesize synthLit ctx
admissibleTypeAt synthLit loc ctx = case P.at' loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just (sub,replace) ->
    let ctx = E.lam1 $ \f -> replace (ksub f)
        -- we annotate `f` as returning `Number` so as not to introduce
        -- any new quantified variables in the inferred type
        ksub f = E.lam2 (\x y -> x) `E.App` (f `E.App` sub `E.Ann` T.Unit T.Number)
        go (T.Arrow (T.Arrow tsub _) _) = tsub
        go (T.Forall n t) = T.Forall n (go t)
    in go <$> synthesize synthLit ctx

-- | Compute the type of the given subterm, unconstrained as much
-- as possible by any local usages of that subterm. For example, in
-- @\g -> map g [1,2,3]@, @g@ will have a type of @forall r . Int -> r@,
-- and @map@ will have a type of @forall a b . (a -> b) -> [a] -> [b]@.
typeAt :: (Monad f, Applicative f)
       => (H.Hash -> Noted f T.Type)
       -> P.Path
       -> E.Term
       -> Noted f T.Type
typeAt synthLit (P.Path []) ctx = synthesize synthLit ctx
typeAt synthLit loc ctx = case P.at loc ctx of
  Nothing -> N.failure $ invalid loc ctx
  Just sub -> letFloat loc ctx >>= \(ctx,loc) ->
    admissibleTypeAt synthLit loc ctx

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
    in
      N.liftMaybe (invalid loc ctx) $ do
        ctx' <- P.modify trimmedPath (\body -> E.lam1 (\x -> body) `E.App` sub) ctx
        loc' <- pure $ P.extend P.Arg trimmedPath
        pure (ctx', loc')
