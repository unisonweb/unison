{-# Language DeriveGeneric #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}

module Unison.Remote where

import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import Unison.Hashable (Hashable, Hashable1)
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Text as Text
import qualified Unison.Hashable as H
import qualified Data.Hashable as DH

{-
Representation of the Unison distributed programming API.

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
data Remote t = Step (Step t) | Bind (Step t) t deriving (Generic,Generic1,Show,Eq,Foldable,Functor,Traversable)
instance ToJSON t => ToJSON (Remote t)
instance FromJSON t => FromJSON (Remote t)

newtype Universe = Universe ByteString deriving (Show,Eq,Ord,Generic)

-- Note: start each layer with leading `2` byte, to avoid collisions with
-- terms/types, which start each layer with leading `0`/`1`.
-- See `Hashable1 Type.F`
instance Hashable1 Remote where
  hash1 hashCycle hash r = H.accumulate $ tag 2 : case r of
    Step s -> [tag 0, hashed1 s]
    Bind s t -> [tag 1, hashed1 s, hashed t]
    where
      tag = H.Tag
      hashed1 = H.Hashed . (H.hash1 hashCycle hash)
      hashed = H.Hashed . hash

data Step t = Local (Local t) | At Node t deriving (Generic,Generic1,Show,Eq,Foldable,Functor,Traversable)
instance ToJSON t => ToJSON (Step t)
instance FromJSON t => FromJSON (Step t)
instance Hashable1 Step where
  hash1 hashCycle hash s = H.accumulate $ case s of
    Local l -> [tag 0, hashed1 l]
    At n t -> [tag 1, H.accumulateToken n, hashed t]
    where
      tag = H.Tag
      hashed1 = H.Hashed . (H.hash1 hashCycle hash)
      hashed = H.Hashed . hash

data Local t
  -- fork : Remote a -> Local ()
  = Fork (Remote t)
  -- channel : Local (Channel a)
  | CreateChannel
  -- here : Local Node
  | Here
  -- sleep : Duration -> Local ()
  | Sleep Duration
  -- receiveAsync : Channel a -> Duration -> Local (Local a)
  | ReceiveAsync Channel Duration
  -- receive : Channel a -> Local a
  | Receive Channel
  -- send : Channel a -> a -> Local ()
  | Send Channel t
  -- spawn : Local Node
  | Spawn
  | Pure t deriving (Generic,Generic1,Show,Eq,Foldable,Functor,Traversable)

instance ToJSON t => ToJSON (Local t)
instance FromJSON t => FromJSON (Local t)
instance Hashable1 Local where
  hash1 hashCycle hash l = H.accumulate $ case l of
    Fork r -> [tag 0, hashed1 r]
    CreateChannel -> [tag 1]
    Here -> [tag 2]
    ReceiveAsync c t -> [tag 3, H.accumulateToken c, H.accumulateToken t]
    Receive c -> [tag 4, H.accumulateToken c]
    Send c t -> [tag 5, H.accumulateToken c, hashed t]
    Spawn -> [tag 6]
    Sleep (Seconds d) -> [tag 7, H.Double d]
    Pure t -> [tag 8, hashed t]
    where
      tag = H.Tag
      hashed1 = H.Hashed . (H.hash1 hashCycle hash)
      hashed = H.Hashed . hash

newtype Duration = Seconds { seconds :: Double } deriving (Eq,Ord,Show,Generic)
instance ToJSON Duration
instance FromJSON Duration
instance Hashable Duration where
  tokens (Seconds seconds) = [H.Double seconds]


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

-- | A node is a host and a public key. For instance: `Node "unisonweb.org" key`
data Node = Node { host :: Text, publicKey :: ByteString } deriving (Eq,Ord,Generic)

instance DH.Hashable Node
instance ToJSON Node where toJSON (Node host key) = toJSON (host, decodeUtf8 (Base64.encode key))
instance FromJSON Node where
  parseJSON v = do
    (host,key) <- parseJSON v
    either fail (pure . Node host) (Base64.decode (encodeUtf8 key))

instance Hashable Node where
  tokens (Node host key) = [H.Text host, H.Bytes key]

instance Show Node where
  show (Node host key) = "http://" ++ Text.unpack host ++ "/" ++ Text.unpack (decodeUtf8 (Base64.encode key))

newtype Channel = Channel ByteString deriving (Eq,Ord,Generic)
instance Show Channel where
  show (Channel id) = Text.unpack (decodeUtf8 (Base64.encode id))

instance ToJSON Channel where toJSON (Channel c) = toJSON (decodeUtf8 (Base64.encode c))

instance FromJSON Channel where
  parseJSON v = do
    txt <- parseJSON v
    either fail (pure . Channel) (Base64.decode (encodeUtf8 txt))

instance Hashable Channel where tokens (Channel c) = H.tokens c
