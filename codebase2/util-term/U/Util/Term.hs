{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module U.Util.Term where

import Control.Monad.Writer (execWriter, tell)
import Data.Foldable (for_, traverse_)
import U.Codebase.Term (F' (..), MatchCase (..), Pattern (..))
import qualified U.Codebase.Term as Term
import qualified U.Core.ABT as ABT

text :: (Ord v) => ABT.Term (Term.F' text termRef typeRef termLink typeLink vt) v a -> [text]
text =
  execWriter . ABT.visit_ \case
    Text t -> tell [t]
    _ -> pure ()

dependencies ::
  (Ord v) =>
  ABT.Term (Term.F' text termRef typeRef termLink typeLink vt) v a ->
  ([termRef], [typeRef], [termLink], [typeLink])
dependencies =
  execWriter . ABT.visit_ \case
    Ref r -> termRef r
    Constructor r _ -> typeRef r
    Request r _ -> typeRef r
    Match _ cases -> for_ cases \case
      MatchCase pat _guard _body -> go pat
        where
          go = \case
            PConstructor r _i args -> typeRef r *> traverse_ go args
            PAs pat -> go pat
            PEffectPure pat -> go pat
            PEffectBind r _i args k -> typeRef r *> traverse_ go args *> go k
            PSequenceLiteral pats -> traverse_ go pats
            PSequenceOp l _op r -> go l *> go r
            _ -> pure ()
    TermLink r -> termLink r
    TypeLink r -> typeLink r
    _ -> pure ()
  where
    termRef r = tell (pure r, mempty, mempty, mempty)
    typeRef r = tell (mempty, pure r, mempty, mempty)
    termLink r = tell (mempty, mempty, pure r, mempty)
    typeLink r = tell (mempty, mempty, mempty, pure r)
