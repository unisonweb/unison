{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.Runtime.Remote where

import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Data.Functor
import Data.IORef
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import qualified Control.Concurrent as Concurrent
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

{-
Implementation of the Unison distributed programming API.

    data Node
    data Local a

    data Remote a
    at : Node -> a -> Remote a
    instance Monad Remote

    instance Monad Local
    fork : Remote a -> Remote ()
    local : Local a -> Remote a

    root : Node -> Channel Packet
    packet : Remote a -> Packet

`Local` is roughly `IO`, the type of local effects, but has
a few additional functions:

    channel : Local (Channel a)
    send : a -> Channel a -> Local ()
    -- | Registers a callback in a map as a weak ref that sets an MVar
    -- if weak ref becomes garbage, remove from the map
    receiveAsync : Channel a -> Local (Local a)
    -- Can be done a bit more efficiently perhaps
    recieve : Channel a -> Local a
    awaitAsync : Remote a -> Local (Local a)
    await : Remote a -> Local a

For the implementation, a remote computation consists of a step, which evaluates
locally or transfers control to another node, or a step along with a continuation
when the step's result is available. Conceptually represented by the following Haskell type:

    data Remote r
      = Step r
      | forall x . Bind (Step x) (x -> Remote r)

    data Step r = Local r | At Node r

Since this data type would not be serializable as a Haskell
data type (would require serializing arbitrary functions), both
`Local` and the `x -> Remote r` are represented as _Unison_
terms, giving the type:
-}

-- `t` will be a Unison term, generally
data Remote t = Step (Step t) | Bind (Step t) t deriving (Generic,Show)
instance Serial t => Serial (Remote t)

data Step t = Local (Local t) | At Node t deriving (Generic,Show)
instance Serial t => Serial (Step t)

data Local t
  -- fork : Remote a -> Local ()
  = Fork (Remote t)
  -- channel : Local (Channel a)
  | CreateChannel
  -- here : Local Node
  | Here
  -- receiveAsync : Channel a -> Local (Local a)
  | ReceiveAsync Channel Timeout
  -- receive : Channel a -> Local a
  | Receive Channel
  -- send : a -> Channel a -> Local ()
  | Send t Channel
  | Pure t deriving (Generic,Show)
instance Serial t => Serial (Local t)

newtype Timeout = Seconds { seconds :: Double } deriving (Eq,Ord,Show,Generic)
instance Serial Timeout

{-
When sending a `Remote` value to a `Node` for evaluation,
the implementation syncs any needed hashes for just the
outermost `Local`, then begins evaluation of the `Local`.
Concurrent with evaluation it syncs any needed hashes for
the continuation of the `Bind` (ignoring this step for a
purely `Local` computation).

When both the `Local` portion of the computation has completed
and any hashes needed by the continuation have also been
synced, the continuation is invoked and evaluated and the
computation is sent to the specified `Node` for its next step.
Note that the computation never 'returns', and it may run forever,
hopping between different nodes. To return a result to some node,
we use some of the effects in `Local` to write the final step to
a channel from which we `receive`.
-}

newtype Base64 = Base64 Text deriving (Eq,Ord,Generic,Show)
instance Serial Base64

-- | A node is a host and a public key. For instance: `Node "unisonweb.org" key`
data Node = Node { host :: String, publicKey :: Base64 } deriving (Eq,Ord,Generic)
instance Serial Node

instance Show Node where
  show (Node host (Base64 key)) = "http://" ++ host ++ "/" ++ Text.unpack key

newtype Channel = Channel Base64 deriving (Eq,Ord,Generic,Show)
instance Serial Channel where

data Language t h
  = Language
    { localDependencies :: t -> Set h
    , eval :: t -> IO t
    , apply :: t -> t -> t
    , node :: Node -> t
    , unit :: t
    , channel :: Channel -> t
    , local :: Local t -> t
    , unRemote :: t -> Maybe (Remote t)
    , remote :: Remote t -> t }

newtype Err = Err String
type Callbacks t h = IORef (Map Channel (IO (), MVar (Result t h)))
data Result t h = Error Err | Evaluated t | Syncing Channel Node [(h,t)]

newtype Universe = Universe ByteString deriving (Show,Eq,Ord,Generic)
instance Serial Universe

data Packet t h
  = Eval Universe Node (Remote t)
  | Need Channel Node (Set h)
  | Provide Channel Node [(h,t)]
  deriving (Show,Generic)
instance (Serial t, Serial h, Ord h, Eq h) => Serial (Packet t h)

data Env t h
  = Env { callbacks :: Callbacks t h
        , saveHashes :: [(h,t)] -> IO ()
        , getHashes :: Set h -> IO [(h,t)]
        , missingHashes :: Set h -> IO (Set h)
        , universe :: Universe
        -- Returns a `(send, cleanup)`, where the `cleanup` should be invoked when
        -- finished sending packets via `send`
        , connect :: Node -> IO (Packet t h -> IO (), IO ())
        , keygen :: Int -> IO ByteString
        , currentNode :: Node }

sendPacketTo :: Env t h -> Node -> Packet t h -> IO ()
sendPacketTo env node packet = do
  (send, cleanup) <- connect env node
  send packet `finally` cleanup

defaultSyncTimeout :: Timeout
defaultSyncTimeout = Seconds 10

-- | Handle a packet. Does not return a meaningful response; it is expected that
-- processing of the packet will cause further progress of the computation either on
-- the current node or elsewhere (for instance, by causing packets to be sent to other nodes).
handle :: Ord h => Language t h -> Env t h -> Packet t h -> IO ()
handle _ env (Provide chan sender hashes) = do
  m <- readIORef (callbacks env)
  case Map.lookup chan m of
    Nothing -> pure ()
    Just (cancelGC, r) -> do
      cancelGC
      atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
      void (tryPutMVar r (Syncing chan sender hashes))
handle _ env (Need chan sender hashes) = do
  sources <- getHashes env hashes
  -- todo: separate error if missing requested hashes
  sendPacketTo env sender (Provide chan (currentNode env) sources)
handle lang env (Eval u sender r) = do
  missingDeps <-
    if (u == universe env) then pure Set.empty
    else do -- might need to fetch some dependencies
      let deps = localDependencies lang (remote lang r)
      needs <- missingHashes env deps
      pure needs
  sync0 missingDeps
  where
  sync0 missingDeps = newChannel >>= \chan -> sync chan missingDeps
  sync _ missingDeps | Set.null missingDeps = afterSync
  sync chan missingDeps = do
    resultMVar <- newEmptyMVar
    gcThread <- Concurrent.forkIO $ do
      Concurrent.threadDelay (floor $ 1000000 * seconds defaultSyncTimeout)
      putMVar resultMVar (Error (Err "Timeout during fetching of dependencies"))
      atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
    atomicModifyIORef' (callbacks env) (\m -> (Map.insert chan (Concurrent.killThread gcThread, resultMVar) m, ()))
    sendPacketTo env sender (Need chan (currentNode env) missingDeps)
    s <- takeMVar resultMVar
    case s of
      Error (Err err) -> fail err
      Evaluated _ -> fail "expected a `Syncing` message or an error, got an `Evaluated`"
      Syncing chan _ hashes -> do
        saveHashes env hashes
        stillMissing <- traverse (\(_,t) -> missingHashes env (localDependencies lang t)) hashes
        sync chan (Set.unions stillMissing)
  afterSync = case r of
    Step (Local l) -> void $ runLocal l
    Step (At n r) -> transfer n r Nothing
    Bind (At n r) k -> transfer n r (Just k)
    Bind (Local l) k -> do
      arg <- runLocal l
      r <- eval lang (apply lang k arg)
      case (unRemote lang r) of
        Just r -> handle lang env (Eval u sender r)
        Nothing -> fail "typechecker bug; function passed to Remote.bind did not return a Remote"
  newChannel :: IO Channel
  newChannel = Channel . Base64 . decodeUtf8 . Base64.encode <$> keygen env 64
  transfer n t k =
    sendPacketTo env n (Eval (universe env) (currentNode env) r) where
      r = case k of
        Nothing -> Step (Local (Pure t))
        Just k -> Bind (Local (Pure t)) k
  runLocal (Fork r) = Concurrent.forkIO (handle lang env (Eval u sender r)) $> unit lang
  runLocal CreateChannel = channel lang <$> newChannel
  runLocal Here = pure $ node lang (currentNode env)
  runLocal (Pure t) = eval lang t
  runLocal (Send a chan) = do
    m <- readIORef (callbacks env)
    case Map.lookup chan m of
      Nothing -> pure (unit lang)
      Just (_, result) -> do
        -- NB: we do not cancel GC, need to block on the read side before timeout
        _ <- tryPutMVar result (Evaluated a)
        atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
        pure (unit lang)
  runLocal (ReceiveAsync chan (Seconds seconds)) = do
    -- todo: think about whether to allow multiple concurrent listeners on a channel
    resultMVar <- newEmptyMVar
    gcThread <- Concurrent.forkIO $ do
      Concurrent.threadDelay (floor $ 1000000 * seconds)
      atomicModifyIORef' (callbacks env) (\m -> (Map.delete chan m, ()))
      putMVar resultMVar (Error (Err "Timeout on receiving from a channel"))
    atomicModifyIORef' (callbacks env) (\m -> (Map.insert chan (Concurrent.killThread gcThread, resultMVar) m, ()))
    pure (remote lang (Step (Local (Receive chan))))
  runLocal (Receive chan) = do
    m <- readIORef (callbacks env)
    case Map.lookup chan m of
      Nothing -> pure (unit lang)
      Just (cancelGC, result) -> do
        r <- takeMVar result
        cancelGC
        case r of
          Error (Err err) -> fail err
          Evaluated r -> pure r
          Syncing _ _ _ -> fail "expected an `Evaluated` message or an error, got a `Syncing`"
