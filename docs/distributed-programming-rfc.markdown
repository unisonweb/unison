This document describes a set of core Unison functions for expressing fault-tolerant multi-node systems, including systems that must be dynamically updated and redeployed without downtime. You can jump to [the full api](#full-api) (about 30 functions) or keep reading for a guided explanation of the whole thing.

Remarks:

* There's a mixture of old and new stuff here, see the [history section](#history) for background.
* Some version of these APIs will be implemented on the new Unison runtime. We are looking gather design feedback and possibly iterate the design before starting on the implementation.
* Some of these primitives are rather low-level and imperative; it's expected that people will use them to build nicer APIs in pure Unison.
* Questions are good if you have them; ask away in the comments and we'll curate that into a Q&A section of the document.

Lastly, thank you to all who have contributed to this design or worked on earlier iterations!

__Contents:__

1. [The basics of multi-node computation: `Remote` and `Remote.transfer`](#basics)
2. [Ephemeral `Node` local state: `Channel` values, `Remote.send` and `Remote.receive`](#ephemeral-state)
3. [Heartbeats, supervision, and lifecycle management](#heartbeats)
4. [Capabilities and dynamic loading and linking](#capabilities)
5. [Durable state](#durable-state)
6. [Node spawning and sandboxing](#node-spawning)
7. [The full API](#full-api)
8. [Appendix: History and context](#history)

### <a id="basics"></a>The basics of multi-node computation: `Remote` and `Remote.transfer`

Let's start with the basics. We have a type, `Remote a`, which forms a monad:

```Haskell
-- Promote a pure value to `Remote`
Remote.pure : ∀ a . a -> Remote a

-- Sequencing of remote computations
Remote.bind : ∀ a b . (a -> Remote b) -> Remote a -> Remote b
```

We can write remote computations using a `do` block, which desugars in the usual way:

```Haskell
do Remote
  x = 23 -- just use single `=` rather than `let x =` as in Haskell
  y := pure 42 -- use `:=` rather than `<-` as in Haskell
  pure (x + y)
```

_Note on syntax differences from Haskell:_ We use a single `=` rather than `let x = ...`, and we use `:=` rather than `<-` for monadic bind.

Remote computations may proceed on multiple values of type `Node`. A `Node` is conceptually a "location where computation can occur". At runtime, a representation of `Node` might be a hostname + public key.

To move a `Remote` computation to a different node, we use `Remote.transfer`:

```Haskell
-- Transfer control of remainder of computation to target node
Remote.transfer : Node -> Remote Unit

ex1 : Node -> Node -> Remote Number
ex1 alice bob = do Remote
  xs = [92,3,145,9,2,64]
  Remote.transfer alice
  sorted-xs = sort Number.Order xs -- sorting occurs on `alice` node
  Remote.transfer bob
  pure (sum sorted-xs) -- summation occurs on `bob` node
```

Implementation note: if we desugar the `do` block, the continuation following a `Remote.transfer` is an arbitrary function `Unit -> Remote a`. This function is sent to the recipient `Node` over a [forward-secret][], mutually authenticated encrypted pipe (using one of the [Noise protocols][]), any missing needed dependencies (as determined via Unison's "nameless" cryptographic hashing scheme) are synced and cached, and the computation then proceeds on the recipient. Note that the sender does not wait around for the recipient to "complete" the computation (which might be forever); it is "fire and forget". The sender transfers the computation and its job is done. Error handling and supervision will be handled separately.

[forward-secret]: https://en.wikipedia.org/wiki/Forward_secrecy
[Noise protocols]: https://noiseprotocol.org/

Remote computations can be forked:

```Haskell
Remote.fork : ∀ a . Remote a -> Remote Unit
```

This starts the computation running asynchronously, purely for its effects.

And computations can fail, explicitly.

```Haskell
Remote.fail : ∀ a . Cause -> Remote a

-- this is TBD
type Cause = Error Text Node | Stopped | Expired | Unresponsive Node
```

_Note on syntax:_ We use `type` rather than `data` to introduce the declaration of a new type.

Error handling and supervision is discussed later.

### <a id="ephemeral-state"></a>Ephemeral `Node` local state: `Channel` values, `Remote.send` and `Remote.receive`

We can create (typed) channels, and send and receive values on these channels:

```Haskell
-- Create a `Channel`; just a GUID at runtime
channel : ∀ a . Remote (Channel a)

-- Send a value to a `Channel` on the current node
Channel.send : ∀ a . Channel a -> a -> Remote Unit

-- Receive a value from a `Channel` on the current node,
-- with an expiration controlled by the supplied heartbeat
-- Note: when the outer `Remote` is bound, listener is registered
Channel.receive : ∀ a . Heartbeat -> Channel a -> Remote (Remote a)

-- Receive multiple values from a `Channel` on the current node,
-- with an expiration controlled by the supplied heartbeat
-- Note: when the outer `Remote` is bound, listener is registered
Channel.subscribe : ∀ a . Heartbeat -> Channel a -> Remote (Remote a)
```

A `Channel` is just a random, unique tag, and `receive` and `subscribe` just register a callback associated with that tag in the node runtime. They differ only in what happens when a value is received: `receive` deregisters this callback as soon as a value is produced, while `subscribe` keeps the callback around, and enqueues any values it receives. Both `receive` and `subscribe` will deregister once the associated `Heartbeat` is stopped or expires on its own, and using the resulting `Remote a` after this point will fail the computation.

Primitives for creating and manipulating `Heartbeat` values are covered next.

### <a id="heartbeats"></a>Heartbeats, supervision, and lifecycle management

A `Heartbeat` is a value used to control the _lifecycle_ of a computation (or, we'll see later, durable state), and is used as a handle for supervision. The simplest way to create a heartbeat is from a duration of time:

```Haskell
-- Create a `Heartbeat` that stops beating after a duration,
-- unless a `Heartbeat.reset` is performed
heartbeat : Duration -> Remote Heartbeat

Duration.seconds : Number -> Duration
```

After the given duration elapses, the heartbeat is said to be _stopped_, and anything _linked_ to the heartbeat will be stopped as well. Here are some other functions on `Heartbeat`:

```Haskell
-- Stop the heartbeat; anything linked to the heartbeat will be terminated / cleaned up
Heartbeat.stop : Heartbeat -> Remote Unit

-- Kill the heartbeat with an explicit error; anything linked to the heartbeat will be terminated / cleaned up
Heartbeat.fail : Cause -> Remote Unit

-- Ensure that the `Heartbeat` has at least the provided duration
-- to live. If the heartbeat has greater than this duration still
-- remaining, then this is a no-op.
Heartbeat.reset : Duration -> Heartbeat -> Remote Unit

-- The resulting heartbeat is live only if _both_ heartbeats are live
Heartbeat.both : Heartbeat -> Heartbeat -> Heartbeat

-- The resulting heartbeat is live if _either_ heartbeat is live
Heartbeat.either : Heartbeat -> Heartbeat -> Heartbeat

-- Be notified when a heartbeat completes
-- When outer `Remote` is bound, the supervisor is registered
Remote.supervise : Heartbeat -> Remote (Remote Cause)

-- Cancel the lexically scoped computation if/when the heartbeat stops,
-- and notify any supervisors of completion or errors
Remote.link : ∀ a . Heartbeat -> Remote a -> Remote a
```

We can think of a heartbeat as being in one of three states:

* It can be _live_, for instance, after being created with a 10 second duration
* It can be _stopped_ due to a `Heartbeat.stop` ('normal' termination)
* It can be _stopped with an error_ due to an explicit `Heartbeat.fail` or due to expiration

`Remote.supervise` waits until the heartbeat stops, and notifies us of the cause. This is useful for setting up supervision trees.

In general we use heartbeats rather than explicit cleanup actions to manage lifecycle of computations and durable data.

#### An example

In the following code, if the forked computation takes more than 10 seconds, the `Channel.subscribe` operation will be cancelled:

```Haskell
do Remote
  c := channel
  for-10s := heartbeat (Duration.seconds 10)
  read-c := Channel.subscribe for-10s c
  Remote.fork <| do Remote
    Remote.transfer alice
    r1 := some-huge-computation
    r2 := another-big-computation
    Channel.send c r1
    Channel.send c r2
  Remote.sequence [read-c, read-c]
```

In this case, it might be useful to also cancel the forked computation as soon as we know the other side will no longer be listening. We can do that with the primitive, `Remote.link`, given above. Putting it together, we can update the above code to cancel the remote computation if more than the 10 seconds elapse:

```Haskell
do Remote
  c := channel
  for-10s := heartbeat (Duration.seconds 10)
  read-c := Channel.subscribe for-10s c
  (Remote.fork `compose` Remote.link for-10s) <| do Remote
    Remote.transfer alice
    r1 := some-huge-computation
    r2 := another-big-computation
    Channel.send c r1
    Channel.send c r2
  Remote.sequence [read-c, read-c]
```

Since the forked computation is linked to the heartbeat, it will only continue as long as the heartbeat is live. In the event that it is killed, the `read-c` calls will also fail, and the callbacks associated with the channel will be deregistered.

#### Implementation notes

The above API is very flexible, but it implies some things about the Unison runtime. Consider this code:

Note: Thanks to @runarorama for helping to flesh out some of these issues

```Haskell
Remote.fork-linked : ∀ a . Heartbeat -> Remote a -> Remote Unit
Remote.fork-linked h = Remote.fork `compose` Remote.link h

-- h1 : Heartbeat
do Remote
  get-cause := Remote.supervise h1
  Remote.fork-linked h1 <| do Remote
    Remote.transfer alice
    r1 := some-huge-computation
    r2 := another-big-computation
    Channel.send c r1
    Channel.send c r2
  Remote.transfer bob
  cause := get-cause
  case cause of
    Error err node -> ...
    ...
```

Think about what happens if the `alice` node were to get hit by an asteroid while in the middle of `some-huge-computation`. At that time, the computation is in a scope where `h1` is a linked heartbeat, so we'd hope that the supervising computation on `bob` which calls `get-cause` gets notified with either an `Unresponsive alice` (if the asteroid strikes before `alice` can get out a reply) or an `Error "Asteroid strike!!" alice` if the asteroid strike gets detected in time to send out a reply to any supervisors.

What's needed to make this happen? Here's a sketch:

* At runtime, a `Heartbeat` is just a GUID, and the runtime keeps a few pieces of state:
  * A mutable _heartbeat status map_, records the state of each heartbeat, along with a timestamp for when that status was last updated.
    * Its type might be something like: `Map Heartbeat (TVar (Status, Timestamp))`
    * Stopped heartbeats and _transferred_ heartbeats are kept around for a few seconds in the status map, before being cleared out by a reaper thread.
  * A _heartbeat reset map_, used to implement `Heartbeat.reset`
  * A "reader monad" stack of `Heartbeat` values, which `Remote.link` pushes onto.
  * A _heartbeat forwarding map_ of type `Map Heartbeat Node`, with a reaper thread which deletes entries older than a few seconds.
* On `Remote.transfer n1`, the current stack of heartbeat values is read, paired up with `n1`, and added to the heartbeat forwarding map. This ensures that anyone looking for a status update on these heartbeats is directed to the correct node.
* On `Remote.supervise`, we set up a thread which will repeatedly check for liveness of the heartbeat, by asking, in this order:
  * The current node for a "recent" status (checking the current node heartbeat status map)
  * If the current node doesn't have a recent enough status, it checks the heartbeat forwarding map to see if the heartbeat has been transferred, and if so, contacts the new node for status. The Unison network protocol includes messages for asking for the status of a heartbeat.
  * If none of these choices can provide status for the heartbeat, the status of the heartbeat is changed to `Nonresponsive`.
* On `Remote.fail`, any heartbeats in scope get updated with the cause. This works the same whether the `Remote.fail` is called explicitly by the programmer, or implicitly, due to an unhandled error.

Because the status map is a bit 'sticky', this protocol seems to work even if a computation is bouncing rapidly between multiple Unison nodes, and avoids the race condition of a heartbeat status request "chasing" the a computation as it hops between nodes.

__Concern:__ I have a vague concern that this heartbeat and supervision API is too expressive and ends up being unimplementable. Can anyone think of programs whose meaning is unclear or seem like nonsense? (In particular, what if `Heartbeat` values are shared and supervised in multiple places...?) There are likely ways to make the API a little less expressive to address these things. For instance, in Erlang, supervision is tied to process creation; you don't get a first class "supervision handle" that multiple processes can supervise, so the supervision graph is necessarily a tree. (I am not an Erlang expert, anyone who is feel free to pipe in) We could likely mimic that design here.

### <a id="capabilities"></a>Capabilities and dynamic loading and linking

Nodes can load and link against values dynamically:

```Haskell
-- Introduce a new, globally unique capability, of a particular type
capability : ∀ a . Remote (Capability a)

-- Introduce a capability on the current node, so long as the heartbeat is live
Capability.provide : ∀ a . Heartbeat -> Capability a -> a -> Remote Unit

-- Update a capability on the current node, if it exists
Capability.update : ∀ a . Capability a -> (a -> a) -> Remote Unit

-- Ask for a capability dynamically
Capability.ask : ∀ a . Capability a -> Remote (Optional a)
```

It's probably not obvious, but I believe this is the key building block for doing live redeployments and upgrades of a running Unison system, since we can now bind against dependencies dynamically. I also suspect you can do interesting things like build '[IPFS](http://ipfs.io/) in Unison', replace DNS, build one (or more) P2P computing fabrics for sharing compute resources (something like [Golem](https://golem.network/)), and more (I think!). Imagine:

```Haskell
peers : Capability (Vector (Node, BloomFilter))

-- Ask for the capability at the current node, or use bloom filter
-- optimized traversal of peers to find a node with requested capability
discover : ∀ a . Capability a -> Remote a
```

Capabilities are also used as the sole FFI for Unison. Nodes with access to external C libraries, a local database, or whatever, are just nodes with more provided capabilities. The unsafe linear algebra function written in C that you want to call from Unison would have an associated `Capability (Matrix -> Remote Matrix)` for using the function on whatever nodes have access to it. [This post](http://unisonweb.org/2016-05-18/iot.html) has a bit more detail.

### <a id="durable-state"></a>Durable state

There are a couple new things here:

```Haskell
-- Create a new key-value store, encrypted with the key.
-- The data will be deleted when the provided heartbeat stops.
Index.empty : ∀ k v . Heartbeat -> Key -> Remote (Index k v)

-- Insert a value, returning a `Clock` which has visibility of this update
Index.insert : ∀ k v . Clock -> k -> v -> Index k v -> Remote Clock

-- Lookup a value associated with a key
Index.lookup : ∀ k v . Clock -> k -> Index k v -> Remote (Optional v)
```

`Key` is just a symmetric encryption key; durable storage is always encrypted. We might generate keys via:

```Haskell
AES256.key : Remote Key
Blowfish.key : Remote Key
-- etc
```

The `Clock` controls visibility of updates, allowing for updates to happen asynchronously and/or on eventually consistent storage. `Index.lookup` waits until the 'committed' version of the storage is greater than or equal to the given clock value. Likewise, `Index.insert` waits to insert until the 'commited' version is greater than or equal to the given clock before it performs the insert.

_Note:_ The `Index.insert` and `Insert.lookup` functions could take a `Heartbeat` to control how long to wait for the clock before proceeding with the operation, but I think this is overkill and it's fine to set this time globally (to, say, a few seconds).

#### The `Clock` API

Here's the API for working with clocks:

```Haskell
-- True if the first clock occurs at or after the second clock
Clock.>= : Clock -> Clock -> Boolean

-- ∀ c, c >=_Clock zero is true
Clock.zero : Clock

-- increment c >= c
Clock.increment : Clock -> Clock

-- Returns two clocks, c1, c2 that are incomparable
-- Inverted by merge
Clock.split : Clock -> (Clock, Clock)

-- merge c1 c2 >= c1 and merge c1 c2 >= c2
Clock.merge : Clock -> Clock -> Clock
```

A reasonable implementation might be based on [interval tree clocks](http://gsd.di.uminho.pt/members/cbm/ps/itc2008.pdf)

### <a id="node-spawning"></a>Node spawning and sandboxing

Nodes can be created dynamically:

```Haskell
-- Create a new node 'in the same location' as the current node
Remote.spawn : Heartbeat -> Optional Key -> Sandbox -> Remote Node

-- TBD
type Sandbox =
  Sandbox CPU% Memory Storage (∀ a . Node -> Remote a -> Remote a)
```

Nodes are spawned with a sandbox, controlling who has what access to the node, and an optional symmetric key. If no key is provided, a new asymmetric keypair will be generated for the node, and the node reference will just include the public key. If a symmetric key is provided, the `Node` value itself will contain a reference to this key at runtime, and the node reference itself should be considered secret. Nodes with symmetric keys are useful for temporary nodes where the cost of generating asymmetric keys and doing asymmetric crypto is too high.

### <a id="full-api"></a>The full API

Here's the full set of primitive functions:

```Haskell
-- Promote a pure value to `Remote`
Remote.pure : ∀ a . a -> Remote a

-- Sequencing of remote computations
Remote.bind : ∀ a b . (a -> Remote b) -> Remote a -> Remote b

-- Transfer control of remainder of computation to target node
Remote.transfer : Node -> Remote Unit

-- Start running a remote computation asynchronously
Remote.fork : ∀ a . Remote a -> Remote Unit

-- Explicitly fail a computation
Remote.fail : ∀ a . Cause -> Remote a

-- this is TBD
type Cause = Error Text Node | Stopped | Expired | Unresponsive Node

-- Create a `Channel`; just a GUID at runtime
channel : ∀ a . Remote (Channel a)

-- Send a value to a `Channel` on the current node
Channel.send : ∀ a . Channel a -> a -> Remote Unit

-- Receive a value from a `Channel` on the current node,
-- with an expiration controlled by the supplied heartbeat
-- Note: when the outer `Remote` is bound, listener is registered
Channel.receive : ∀ a . Heartbeat -> Channel a -> Remote (Remote a)

-- Receive multiple values from a `Channel` on the current node,
-- with an expiration controlled by the supplied heartbeat
-- Note: when the outer `Remote` is bound, listener is registered
Channel.subscribe : ∀ a . Heartbeat -> Channel a -> Remote (Remote a)

-- Create a `Heartbeat` that stops beating after a duration,
-- unless a `Heartbeat.reset` is performed
heartbeat : Duration -> Remote Heartbeat

Duration.seconds : Number -> Duration

-- Stop the heartbeat; anything linked to the heartbeat will be terminated / cleaned up
Heartbeat.stop : Heartbeat -> Remote Unit

-- Kill the heartbeat with an explicit error; anything linked to the heartbeat will be terminated / cleaned up
Heartbeat.fail : Cause -> Remote Unit

-- Reset the amount of time remaining in the heartbeat
Heartbeat.reset : Duration -> Heartbeat -> Remote Unit

-- The resulting heartbeat is live only if _both_ heartbeats are live
Heartbeat.both : Heartbeat -> Heartbeat -> Heartbeat

-- The resulting heartbeat is live if _either_ heartbeat is live
Heartbeat.either : Heartbeat -> Heartbeat -> Heartbeat

-- Be notified when a heartbeat completes
-- When outer `Remote` is bound, the supervisor is registered
Remote.supervise : Heartbeat -> Remote (Remote Cause)

-- Cancel the lexically scoped computation if/when the heartbeat stops,
-- and notify any supervisors of completion or errors
Remote.link : ∀ a . Heartbeat -> Remote a -> Remote a

-- Introduce a new, globally unique capability, of a particular type
capability : ∀ a . Remote (Capability a)

-- Introduce a capability on the current node, so long as the heartbeat is live
Capability.provide : ∀ a . Heartbeat -> Capability a -> a -> Remote Unit

-- Update a capability on the current node, if it exists
Capability.update : ∀ a . Capability a -> (a -> a) -> Remote Unit

-- Ask for a capability dynamically
Capability.ask : ∀ a . Capability a -> Remote (Optional a)

-- Create a new key-value store, encrypted with the key.
-- The data will be deleted when the provided heartbeat stops.
Index.empty : ∀ k v . Heartbeat -> Key -> Remote (Index k v)

-- Insert a value, returning a `Clock` which has visibility of this update
Index.insert : ∀ k v . Clock -> k -> v -> Index k v -> Remote Clock

-- Lookup a value associated with a key
Index.lookup : ∀ k v . Clock -> k -> Index k v -> Remote (Optional v)

-- Symmetric key generation
AES256.key : Remote Key
Blowfish.key : Remote Key

-- True if the first clock occurs at or after the second clock
Clock.>= : Clock -> Clock -> Boolean

-- ∀ c, c >=_Clock zero is true
Clock.zero : Clock

-- increment c >= c
Clock.increment : Clock -> Clock

-- Returns two clocks, c1, c2 that are incomparable
-- Inverted by merge
Clock.split : Clock -> (Clock, Clock)

-- merge c1 c2 >= c1 and merge c1 c2 >= c2
Clock.merge : Clock -> Clock -> Clock

-- Create a new node 'in the same location' as the current node
Remote.spawn : Heartbeat -> Optional Key -> Sandbox -> Remote Node

-- TBD
type Sandbox =
  Sandbox CPU% Memory Storage (∀ a . Node -> Remote a -> Remote a)
```

### <a id="history"></a>Appendix: History and context

[This post](http://unisonweb.org/2015-06-02/distributed-evaluation.html) has an early writeup of how Unison's hashing scheme could be used to build a robust multi-node computation story. That eventually got an implementation, and as a demo I put together [a simple multi-node search engine](http://unisonweb.org/2016-10-12/search.html#post-start) in Unison. That raised a couple issues and questions, some discussed in that post, some discussed [in this post about microservices](http://unisonweb.org/2016-10-12/microservices.html#post-start), and some that I have just been ruminating on. 🤔

The big questions were around:

* Lifecycle management of nodes and durable data---when is durable data destroyed, and when are nodes destroyed? This led to the `Heartbeat` design.
* Encryption: how are things encrypted, both at rest (in durable storage) and in transit (when moving between nodes). The solution given here is to have 'in transit' encryption handled transparently by the runtime, but to have encryption keys for durable state to be managed explicitly by the programmer. This allows for multiple nodes to use a common storage layer, without all reads needing to go through a common node.
* Dynamic updates and redeployment---how is this done? Solution given is the `Capability` stuff.
