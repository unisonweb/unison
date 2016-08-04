{-# Language OverloadedStrings #-}

module Main where

import Control.Monad
import Unison.NodeProtocol.V0 (protocol)
import Unison.NodeWorker as W
import Unison.SerializationAndHashing (TermV)
import qualified Data.Set as Set
import qualified Unison.Cryptography as C
import qualified Unison.Remote as RT
import qualified Unison.Runtime.Remote as R
import qualified Unison.Term as Term
import qualified Unison.Runtime.ExtraBuiltins as ExtraBuiltins
import qualified Unison.Node.Builtin as Builtin
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Eval as Eval
import qualified Data.Map as Map
import qualified Unison.Note as Note
import Unison.Hash (Hash)

main :: IO ()
main = W.make protocol crypto (pure lang) where
  crypto keypair = C.noop (W.public keypair)
  lang crypto blockstore = do
    let b0 = Builtin.makeBuiltins
    b1 <- ExtraBuiltins.makeAPI blockstore crypto
    pure (R.Language localDependencies (eval b0 b1) apply node unit channel local unRemote remote
      :: R.Language TermV Hash)
    where
      codestore = R.makeCodestore blockstore :: R.Codestore TermV Hash
      localDependencies _ = Set.empty -- todo, compute this for real
      eval b0 b1 =
        let
          evaluator = I.eval allprimops
          whnf = Eval.whnf evaluator gethash
          allbuiltins = b0 whnf ++ b1 whnf
          allprimops = Map.fromList [ (r, op) | Builtin.Builtin r (Just op) _ _ <- allbuiltins ]
          gethash h = Note.lift $ do
            [(h',t)] <- R.getHashes codestore (Set.singleton h)
            guard $ h == h'
            pure t
        in \t -> Note.run (whnf t)
      apply = Term.app
      node = Term.node
      unit = Term.builtin "()"
      channel = Term.channel
      local l = Term.remote (RT.Step (RT.Local l))
      unRemote (Term.Distributed' (Term.Remote r)) = Just r
      unRemote _ = Nothing
      remote = Term.remote
