{-# Language OverloadedStrings #-}

module Main where

import Control.Monad
import Unison.Hash (Hash)
import Unison.NodeProtocol.V0 (protocol)
import Unison.NodeWorker as W
import Unison.SerializationAndHashing (TermV)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.Cryptography as C
import qualified Unison.Eval as Eval
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Node.Builtin as Builtin
import qualified Unison.Note as Note
import qualified Unison.Remote as RT
import qualified Unison.Runtime.ExtraBuiltins as ExtraBuiltins
import qualified Unison.Runtime.Remote as R
import qualified Unison.Term as Term
import qualified Unison.Typechecker as Typechecker

main :: IO ()
main = W.make protocol crypto (pure lang) where
  crypto keypair = C.noop (W.public keypair)
  lang crypto blockstore = do
    let b0 = Builtin.makeBuiltins
    b1 <- ExtraBuiltins.makeAPI blockstore crypto
    pure $ go b0 b1
    where
      go b0 b1 = (lang, typecheck) where
        lang :: R.Language TermV Hash
        lang = R.Language localDependencies eval apply node unit channel local unRemote remote
        codestore = R.makeCodestore blockstore :: R.Codestore TermV Hash
        localDependencies _ = Set.empty -- todo, compute this for real
        evaluator = I.eval allprimops
        whnf = Eval.whnf evaluator gethash
        allbuiltins = b0 whnf ++ b1 whnf
        allprimops = Map.fromList [ (r, op) | Builtin.Builtin r (Just op) _ _ <- allbuiltins ]
        gethash h = Note.lift $ do
          [(h',t)] <- R.getHashes codestore (Set.singleton h)
          guard $ h == h'
          pure t
        typeEnv ref = case lookup ref [ (r, t) | Builtin.Builtin r _ t _ <- allbuiltins ] of
          Nothing -> fail $ "unknown reference " ++ show ref
          Just t -> pure t
        eval t = Note.run (whnf t)
        typecheck term = Note.attemptRun . void $ Typechecker.synthesize typeEnv term
      apply = Term.app
      node = Term.node
      unit = Term.builtin "()"
      channel = Term.channel
      local l = Term.remote (RT.Step (RT.Local l))
      unRemote (Term.Distributed' (Term.Remote r)) = Just r
      unRemote _ = Nothing
      remote = Term.remote
