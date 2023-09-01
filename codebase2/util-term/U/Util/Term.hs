{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module U.Util.Term where

import Control.Monad.Writer (execWriter, tell)
import Data.Foldable (for_, toList, traverse_)
import U.Codebase.Term (F' (..), MatchCase (..), Pattern (..))
import U.Codebase.Term qualified as Term
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT

text :: (Ord v) => ABT.Term (Term.F' text termRef typeRef ctorRef termLink typeLink vt) v a -> [text]
text =
  execWriter . ABT.visit_ \case
    Text t -> tell [t]
    _ -> pure ()

dependencies ::
  (Ord typeRef, Ord v, Ord vt) =>
  ABT.Term (Term.F' text termRef typeRef ctorRef termLink typeLink vt) v a ->
  ([termRef], [typeRef], [ctorRef], [termLink], [typeLink])
dependencies =
  execWriter . ABT.visit_ \case
    Ann _ typ -> annDeps typ
    Ref r -> termRef r
    Constructor r _ -> ctorRef r
    Request r _ -> ctorRef r
    Match _ cases -> for_ cases \case
      MatchCase pat _guard _body -> go pat
        where
          go = \case
            PConstructor r _i args -> ctorRef r *> traverse_ go args
            PAs pat -> go pat
            PEffectPure pat -> go pat
            PEffectBind r _i args k -> ctorRef r *> traverse_ go args *> go k
            PSequenceLiteral pats -> traverse_ go pats
            PSequenceOp l _op r -> go l *> go r
            _ -> pure ()
    TermLink r -> termLink r
    TypeLink r -> typeLink r
    _ -> pure ()
  where
    annDeps typ = tell (mempty, toList $ Type.dependencies typ, mempty, mempty, mempty)
    termRef r = tell (pure r, mempty, mempty, mempty, mempty)
    ctorRef r = tell (mempty, mempty, pure r, mempty, mempty)
    termLink r = tell (mempty, mempty, mempty, pure r, mempty)
    typeLink r = tell (mempty, mempty, mempty, mempty, pure r)
