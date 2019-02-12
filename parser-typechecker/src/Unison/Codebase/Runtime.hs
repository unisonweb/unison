{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.Runtime where

import           Data.Foldable
import           Data.Traversable               ( for )
import qualified Unison.ABT                    as ABT
import           Data.Functor                   ( void )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.IO.Class         ( MonadIO )
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.CodeLookup    as CL
import           Unison.UnisonFile              ( UnisonFile )
import qualified Unison.Term                   as Term
import           Unison.Term                    ( Term
                                                , AnnotatedTerm
                                                )
import           Unison.Var                     ( Var )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.UnisonFile             as UF

data Runtime v = Runtime
  { terminate :: forall m. MonadIO m => m ()
  , evaluate
      :: forall a m
      .  (MonadIO m, Monoid a)
      => CL.CodeLookup m v a
      -> AnnotatedTerm v a
      -> m (Term v)
  }

type IsCacheHit = Bool

-- Evaluates the watch expressions in the file, returning a `Map` of their
-- results. This has to be a bit fancy to handle that the definitions in the
-- file depend on each other and evaluation must proceed in a way that respects
-- these dependencies.
--
-- Note: The definitions in the file are hashed and looked up in
-- `evaluationCache`. If that returns a result, evaluation of that definition
-- can be skipped.
evaluateWatches :: forall m v a . (Var v, MonadIO m)
                => Codebase m v a
                -> (Reference -> m (Maybe (Term v)))
                -> Runtime v
                -> UnisonFile v a
                -> m (Map v (a, Reference, Term v, Term v, IsCacheHit))
evaluateWatches code evaluationCache rt uf = do
  -- 1. compute hashes for everything in the file
  let
    m :: Map v (Reference, AnnotatedTerm v a)
    m = Term.hashComponents (Map.fromList (UF.terms uf <> UF.watches uf))
    watches = Set.fromList (fst <$> UF.watches uf)
    unann = Term.amap (const ())
  -- 2. use the cache to lookup things already computed
  m' <- fmap Map.fromList . for (Map.toList m) $ \(v, (r, t)) -> do
    o <- evaluationCache r
    case o of Nothing -> pure (v, (r, ABT.annotation t, unann t, False))
              Just t'  -> pure (v, (r, ABT.annotation t, t', True))
  -- 3. create a big ol' let rec whose body is a big tuple of all watches
  let
    rv :: Map Reference v
    rv = Map.fromList [(r,v) | (v, (r,_)) <- Map.toList m ]
    bindings :: [(v, Term v)]
    bindings = [ (v, unref rv b) | (v, (_,_,b,_)) <- Map.toList m' ]
    watchVars = [ Term.var() v | v <- toList watches ]
    bigOl'LetRec = Term.letRec' True bindings (Term.tuple watchVars)
    cl :: CL.CodeLookup m v ()
    cl = void $ CL.fromUnisonFile uf <> Codebase.toCodeLookup code
  -- 4. evaluate it and get all the results out of the tuple, then
  -- create the result Map
  out <- evaluate rt cl bigOl'LetRec
  case out of
    Term.Tuple' results -> pure $
      let go eval (ref, a, uneval, isHit) = (a, ref, uneval, eval, isHit)
      in Map.intersectionWith go (Map.fromList (toList watches `zip` results)) m'
    _ -> fail $ "Evaluation should produce a tuple, but gave: " ++ show out
  where
    -- unref :: Map Reference v -> AnnotatedTerm v a -> AnnotatedTerm v a
    unref rv t = ABT.visitPure go t
     where
      go t@(Term.Ref' r@(Reference.DerivedId _)) = case Map.lookup r rv of
        Nothing -> Nothing
        Just v -> Just (Term.var (ABT.annotation t) v)
      go _ = Nothing
