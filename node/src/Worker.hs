{-# Language OverloadedStrings #-}

module Main where

import Control.Concurrent.STM.TVar
import Control.Monad
import System.Directory (doesFileExist)
import System.IO (stderr)
import Unison.Hash (Hash)
import Unison.NodeProtocol.V0 (protocol)
import Unison.NodeWorker as W
import Unison.SerializationAndHashing (TermV)
import qualified Data.Map as Map
import qualified Control.Concurrent.STM as STM
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Unison.Config as Config
import qualified Unison.Cryptography as C
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Node as Node
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.Builtin as Builtin
import qualified Unison.Node.FileStore as Store
import qualified Unison.Note as Note
import qualified Unison.Parsers as Parsers
import qualified Unison.Reference as Reference
import qualified Unison.Remote as RT
import qualified Unison.Runtime.ExtraBuiltins as ExtraBuiltins
import qualified Unison.Runtime.Remote as R
import qualified Unison.SerializationAndHashing as SAH
import qualified Unison.Term as Term
import qualified Unison.Util.Logger as L

main :: IO ()
main = do
  logger <- L.scope "worker-main" <$> Config.loggerTo stderr
  W.make protocol crypto (pure $ lang logger) where
  crypto keypair = C.noop (W.public keypair)
  lang logger crypto blockstore = do
    let b0 = Builtin.makeBuiltins
    b1 <- ExtraBuiltins.makeAPI blockstore crypto
    store <- Store.make "codestore"
    backend <- BasicNode.make SAH.hash store (\whnf -> b0 whnf ++ b1 whnf)
    initialized <- STM.atomically $ newTVar False
    pure $ go backend initialized b0 b1
    where
      go backend initialized b0 b1 =
        let
          lang :: R.Language TermV Hash
          lang = R.Language localDependencies eval apply node unit channel local unRemote remote
          codestore = R.makeCodestore blockstore :: R.Codestore TermV Hash
          localDependencies _ = Set.empty -- todo, compute this for real
          whnf e = do -- todo: may want to have this use evaluator + codestore directly
            Note.lift . STM.atomically $ readTVar initialized >>= \ok ->
              if ok then pure ()
              else STM.retry
            [(_,_,e)] <- Node.evaluateTerms backend [([], e)]
            pure e
          eval t = Note.run (whnf t)
          -- evaluator = I.eval allprimops
          -- allbuiltins = b0 whnf ++ b1 whnf
          -- allprimops = Map.fromList [ (r, op) | Builtin.Builtin r (Just op) _ _ <- allbuiltins ]
          typecheck e = do
            bindings <- Note.run $ Node.allTermsByVarName Term.ref backend
            let e' = Parsers.bindBuiltins bindings [] e
            Note.unnote (Node.typeAt backend e' []) >>= \t -> case t of
              Left note -> pure $ Left (show note)
              Right _ -> pure (Right e')
          initialize = do
            L.info logger "checking if base libraries loaded"
            alreadyInitialized <- doesDirectoryExist "codestore"
            when (not alreadyInitialized) $ do
              L.info logger "codestore/ directory not found, loading base libraries..."
              loadDeclarations "unison-src/base.u" backend
              loadDeclarations "unison-src/extra.u" backend
              hs <- Note.run (Node.allTerms backend)
              R.saveHashes codestore [ (h,v) | (Reference.Derived h, v) <- hs ]
            STM.atomically $ writeTVar initialized True
        in (lang, typecheck, initialize)
      apply = Term.app
      node = Term.node
      unit = Term.builtin "()"
      channel = Term.channel
      local l = Term.remote (RT.Step (RT.Local l))
      unRemote (Term.Distributed' (Term.Remote r)) = Just r
      unRemote _ = Nothing
      remote = Term.remote
      loadDeclarations path node = do
        txt <- Text.IO.readFile path
        let str = Text.unpack txt
        L.info logger $ "loading " ++ path
        r <- Note.run $ Node.declare' Term.ref str node
        L.info logger $ "done loading " ++ path
        pure r
