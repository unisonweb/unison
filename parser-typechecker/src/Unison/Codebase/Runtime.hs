{-#LANGUAGE RankNTypes#-}
module Unison.Codebase.Runtime where

import Control.Applicative
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.IO.Class         ( MonadIO )
import           Unison.Codebase                ( Codebase, Decl )
import qualified Unison.Codebase.CodeLookup     as CL
import           Unison.UnisonFile              ( UnisonFile )
import qualified Unison.UnisonFile              as UF
import qualified Unison.Term                    as Term
import           Unison.Term                    ( Term
                                                , AnnotatedTerm
                                                )
import           Unison.Var                     ( Var )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference               as Reference

data Runtime v = Runtime
  { terminate :: forall m. MonadIO m => m ()
  , evaluate
      :: forall a b m
      .  (MonadIO m, Monoid a)
      => CL.CodeLookup m v a
      -> AnnotatedTerm v a
      -> m (Term v)
  }

-- Evaluates the watch expressions in the file, returning a `Map` of their
-- results. This has to be a bit fancy to handle that the definitions in the
-- file depend on each other and evaluation must proceed in a way that respects
-- these dependencies.
--
-- Note: The definitions in the file are hashed and looked up in
-- `evaluationCache`. If that returns a result, evaluation of that definition
-- can be skipped.
evaluateWatches :: (Var v, MonadIO m)
                => Codebase m v a
                -> (Reference -> m (Maybe (Term v)))
                -> Runtime v
                -> UnisonFile v a
                -> m (Map v (a, Term v))
evaluateWatches evaluationCache rt uf = error "todo"
