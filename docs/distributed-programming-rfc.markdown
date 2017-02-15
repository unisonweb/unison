This document describes a set of core Unison functions for expressing fault-tolerant multi-node systems, including systems that must be dynamically updated and redeployed without downtime.

Remarks:

* There's a mixture of old and new stuff here, see the [history section](#history) for background.
* Some version of these APIs will be implemented on the new Unison runtime. We are looking to gather design feedback and possibly iterate the design before starting on the implementation.
* Some of these primitives are rather low-level and imperative; it's expected that people will use them to build nicer APIs in pure Unison.
* Questions are good if you have them; ask away in the comments and we'll curate that into a Q&A section of the document.

Lastly, thank you to all who have contributed to this design or worked on earlier iterations!

__Contents:__

1. [The full API](#full-api)
  * [Basics of multi-node computation](#basics)
  * [Provisioning of nodes](#provisioning)
  * [Encryption](#encryption)
  * [`Box`: mutable concurrent variables](#box)
  * [`Heartbeat` and supervision](#heartbeats)
  * [`Capability`: dynamic binding of dependencies](#capabilities)
  * [Durable storage](#durable)
3. [Appendix: History and context](#history)

### <a id="full-api"></a>The full API

Here's the full set of primitive functions:

#### <a id="basics"></a>Basics of multi-node computation

Unison computations can hop between nodes, can fail, and can be forked to execute asynchronously:

```Haskell
-- Promote a pure value to `Remote`
Remote.pure : ∀ a . a -> Remote a

-- Sequencing of remote computations
Remote.bind : ∀ a b . (a -> Remote b) -> Remote a -> Remote b

-- The current node where the computation is executing
Remote.here : Remote Node

-- Transfer control of remainder of computation to target node
Remote.transfer : Node -> Remote Unit

-- Start running a remote computation asynchronously
Remote.fork : ∀ a . Remote a -> Remote Unit

-- Explicitly fail a computation
Remote.fail : ∀ a . Cause -> Remote a

-- this is TBD
type Cause = Error Text Node | Stopped | Expired | Unresponsive Node
```

<details><summary><b>Expand for details</b></summary><p>

So, we have a type, `Remote a`, which forms a monad (`Remote.pure` and `Remote.bind`).

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

Remote computations can be concurrent, using `Remote.fork`:

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
</p></details>

#### <a id="provisioning"></a>Node provisioning

Unison computations can provision new nodes:

```Haskell
-- Like `Remote.spawn`, but create the node inside a fresh sandbox
Remote.spawn-sandboxed : Sandbox -> Remote Node
Remote.spawn-sandboxed' : Key -> Sandbox -> Remote Node

-- Create a new node 'in the same location' as the current node, sharing
-- current sandbox resources
Remote.spawn : Remote Node
Remote.spawn' : Key -> Remote Node

-- TBD
type Sandbox =
  Sandbox CPU% Memory Storage (∀ a . Node -> Remote a -> Remote a)
```

<details><summary><b>Notes</b></summary><p>
* A node sandbox will likely be backed by a single OS process, with the given CPU / memory resources. Computations at different nodes in the sandbox can't interfere with each other, other than hogging CPU or memory available to the sandbox. A failed computation in one node can't take down other nodes.
* `Key` is a symmetric encryption key (see [encryption](#encryption)). If none is provided, the provisioned node uses a freshly generated asymmetric keypair.
* Nodes with symmetric keys should be treated as secret. The `Node` itself is basically an address plus a GUID plus the symmetric key. Use case for this is temporary or ephemeral nodes for which the temporary.
* The lifetime of a dynamically provisioned node is linked to the current in scope [heartbeat, discussed in a later section](#heartbeats).
</p></details>

#### <a id="encryption"></a>Encryption

We can encrypt / decrypt any value at all:

```Haskell
-- Encrypt a value, requires `Remote` since we use random IV / nonce
encrypt : ∀ a . Key -> a -> Remote (Encrypted a)

-- Decrypt a value, or return `None` if key is incorrect
decrypt : ∀ a . Key -> Encrypted a -> Either DecryptionFailure a

-- `Key` is just a symmetric encryption key. We might generate keys via:

AES256.key : Remote Key
Blowfish.key : Remote Key
-- etc

-- TBD
type DecryptionFailure = WrongKey | AlgorithmMismatch | IntegrityFailure
```

<details><summary><b>Notes</b></summary><p>
I chose not to make the encryption algorithm part of the type. IMO this is overkill. There's only one key type, `Key`, which at runtime will just have a tag of some sort indicating the algorithm. Trying to decrypt an `AES256`-encrypted value using a `Blowfish` key is just another decryption failure, we don't prevent it at the type level. (Users are free to wrap this in a more typeful API if they wish, am just keeping the primitives simple)
</p></details>

#### <a id="box"></a>`Box`: mutable concurrent variables

Unison programs have access to mutable variables, which also serve as a concurrency primitive:

```Haskell
-- Create an ephemeral `Box` on the current node; just a (GUID, Node) at runtime
Box.empty : ∀ a . Remote (Box a)

-- Put a value into the box, or if the box is full,
-- wait until a `Box.take` empties the box.
Box.put : ∀ a . a -> Box a -> Remote Unit

-- Remove and return the value in the box, or if the box is empty,
-- wait until a `Box.put` fills the box.
Box.take : ∀ a . Box a -> Remote a

-- Like `Box.take`, but leaves the value inside the box
Box.read : ∀ a . Box a -> Remote a

-- Read the current value inside the box or return `None` immediately.
-- Also returns a setter which returns `True` if the set was successful.
Box.access : ∀ a . Box a -> Remote (Optional a, a -> Remote Bool)
```

<details><summary><b>Notes</b></summary><p>
A `Box` is a node-local mutable variable analogous to an `MVar` in Haskell: a bounded queue of max size 1. It's well-known that these can be used to implement just about any other concurrency primitive (for instance, unbounded asynchronous queues).

Here's a handy function, for creating a non-empty box:

```Haskell
Box.make : ∀ a . a -> Remote (Box a)
Box.make a = do Remote { b := Box.empty; Box.put a; pure b }
```

`Box` differs from `MVar` in how blocked threads get handled: in Haskell, a thread blocked on an `MVar.take` will be thrown a `BlockedIndefinitelyOnMVar` exception if no other live threads have access to that `MVar`. We can't do this since our programs are distributed, and the corresponding `put` might arrive at any time from some other node. Thus, in Unison, we rely on a different mechanism to control how long we wait for a blocking operation like `Box.put` or `Box.take` to complete: _heartbeats_. Primitives for creating and manipulating `Heartbeat` values are covered in the next section.

Important implementation note: All `Box` functions can be invoked from any node. If the `Box` being addressed lives elsewhere, the computation will be transported to the node where the box lives, the operation will take place, and the computation will hop back to the originating node, for instance:

```Haskell
do Remote
  Remote.transfer alice
  b := Box.empty
  Remote.transfer bob
  Box.put 23 b
  pure (1 + 1) -- takes place on `bob`
```

Lastly, boxes created via `Box.empty` don't survive node failure.
</p></details>

#### <a id="heartbeats"></a>`Heartbeat` and supervision

We can supervise (detect and respond to errors, control lifecycle) of computations, even computations running on other nodes:

```Haskell
-- Create a `Heartbeat` that stops beating after a duration,
-- unless a `Heartbeat.reset` is performed
Heartbeat.fromDuration : Duration -> Remote Heartbeat

-- Create a duration from a number of seconds
Duration.seconds : Number -> Duration

-- Stop the heartbeat; anything linked to the heartbeat will be terminated / cleaned up
Heartbeat.stop : Heartbeat -> Remote Unit

-- Kill the heartbeat with an explicit error; anything linked to the heartbeat will be terminated / cleaned up
Heartbeat.fail : Cause -> Remote Unit

-- Ensure the heartbeat stays alive for at least the given duration.
-- Commutative, so `bump 20s h; bump 10s h` results in `h` having 20s left,
-- and we could switch the order and have the same effect.
-- Also idempotent, so `bump 20s h; bump 20s h` results in `h` having 20s left
Heartbeat.bump : Duration -> Heartbeat -> Remote Unit

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

<details><summary><b>Notes</b></summary><p>
A `Heartbeat` is a value used to control the _lifecycle_ of a computation (or, we'll see later, durable state), and is used as a handle for supervision. The simplest way to create a heartbeat is from a duration of time, `Heartbeat.fromDuration`.

After the given duration elapses, the heartbeat is said to be _stopped_, and anything _linked_ to the heartbeat will be stopped as well.

We can think of a heartbeat as being in one of three states:

* It can be _live_, for instance, after being created with a 10 second duration
* It can be _stopped_ due to a `Heartbeat.stop` ('normal' termination)
* It can be _stopped with an error_ due to an explicit `Heartbeat.fail` or due to expiration

`Remote.supervise` waits until the heartbeat stops, and notifies us of the cause. This is useful for setting up supervision trees.

In general we use heartbeats rather than explicit cleanup actions to manage lifecycle of computations and durable data.

A couple notes:

* `Remote.fork` applies the current heartbeat to the forked computation.
* Likewise `Remote.spawn*` variants do the same.
* Potentially blocking operations like `Box.put` and `Box.take` also inherit the current lexically scoped heartbeat.

The general idea here is that `Remote.link` should be able to supervise and control the entirety of the computation passed in: no matter how many threads or nodes get spawned or how many blocking operations are underway, a single `Heartbeat.stop h` for some `Remote.link h r` is enough to terminate everying being done by `r`. Without this guarantee, it's impossible to reason generally about how to supervise and orchestrate computations, since the "full shutdown procedure" would be different for every computation. Nonuniformity like this is very bad for composability.

##### An example

In the following code, if the forked computation takes more than 10 seconds, the `Channel.subscribe` operation will be cancelled:

```Haskell
do Remote
  for-10s := heartbeat (Duration.seconds 10)
  b := Box.empty
  Remote.fork <| do Remote
    Remote.transfer alice
    r1 := some-huge-computation
    r2 := another-big-computation
    Box.put r1 b
    Box.put r2 b
  Remote.sequence [Box.take b, Box.take b]
```

In this case, it might be useful to also cancel the forked computation as soon as we know the other side will no longer be listening. We can do that with the primitive, `Remote.link`, given above. Putting it together, we can update the above code to cancel the remote computation if more than the 10 seconds elapse:

```Haskell
Remote.fork-linked : forall a . Heartbeat -> Remote a -> Remote Unit
Remote.fork-linked h r = Remote.fork (Remote.link h r)

do Remote
  for-10s := heartbeat (Duration.seconds 10)
  b := Box.empty
  Remote.fork-linked for-10s <| do Remote
    Remote.transfer alice
    r1 := some-huge-computation
    r2 := another-big-computation
    Box.put r1 b
    Box.put r2 b
  Remote.link for-10s <| Remote.sequence [Box.take b, Box.take b]
```

Since the forked computation is linked to the heartbeat, it will only continue as long as the heartbeat is live. In the event that it is killed, all waiting `Box.take` operations under a `Remote.link` will complete with failure, and the callbacks associated with the box GUID will be deregistered.

##### Implementation notes

The above API is very flexible, but it implies some things about the Unison runtime. Consider this code:

Note: Thanks to @runarorama for helping to flesh out some of these issues

```Haskell
-- h1 : Heartbeat
do Remote
  get-cause := Remote.supervise h1
  Remote.fork-linked h1 <| do Remote
    Remote.transfer alice
    r1 := some-huge-computation
    r2 := another-big-computation
    Box.put r1 b
    Box.put r2 b
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
  * A _heartbeat bump map_, used to implement `Heartbeat.bump`
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
</p></details>

#### <a id="capabilities"></a>`Capability`: dynamic binding of dependencies

Unison can bind dependencies dynamically:

```Haskell
-- Dynamically create a new, globally unique capability, of a particular type
Capability.make : ∀ a . Remote (Capability a)

-- Ask for a capability dynamically, returns a `Box`
-- allowing capability to be read/written
Capability.ask : ∀ a . Capability a -> Remote (Box a)

-- Syntax to declare a static capability, whose state does not survive node restart
ephemeral favorite-numbers : Capability (Vector Number)
```

<details><summary><b>Notes</b></summary><p>
* The key feature of a capability is it is _dynamically bound to the current node_. Asking for a capability tells you something about what the current node can do directly.
* Capabilities can be declared dynamically or statically. Static capabilities function as "standards" (imagine an "email address" or "favorite pizza" capability), but they can also be used as the the handle for external library functions that aren't pure Unison or builtins.
* We build on the `Box` API here. A `Capability` is just a `Box` that's dynamically looked up in the "current" node.
</p></details>

#### <a id="durable"></a>Durable storage

Unison can make any value durable, and dynamically bound variables can be declared durable (these are analogous to typed "files"):

```Haskell
-- Syntax to declare a static capability, whose state does survive node restart
-- Analogous to a "typed" file name!
durable emails : Capability (List Email)

-- Move any value from RAM to durable storage
Durable.store : ∀ a . a -> Remote (Remote a)

-- And the other direction
Durable.load : ∀ a . Durable a -> Remote a
```

<details><summary><b>Notes</b></summary><p>
* Durable data is immutable! You can have a mutable pointer (a `Capability`) but the data itself is immutable.
* You can build durable data structures just by nesting `Durable` values.
* Since durable data is immutable, it can be mobile and not have a single-node owner, and can be trivially replicated.
* I'm not sure if replication should happen transparently, the same way that Unison code is replicated / "deployed"---a `Remote.transfer alice` with a continuation which calls `load` on that data will replicate that data to `alice`.
  * Alternative would be that there's a `Durable.replicate : Durable a -> Remote (Durable a)` which makes replication more explicit.
* Somewhat controversial: durable data is garbage collected. The roots are currently running computations (more coarse-grained would be currently live nodes) as well as durable capabilities. Durable data can't contain cycles.
* The `Durable a` value itself is always constant space, at runtime it's just a hash and possibly some small set of nodes which are expected to have that hash in their storage layer.
* Implementation idea: each node container keeps a mapping from hash to value. When Alice receives a computation via `Remote.transfer`, she can scans the received computation for `Durable` values. Any `Durable` value with a hash Alice already knows can be updated, so that Alice is one of the nodes with a cached copy of that `Durable` and this info is reflected if she passes that value on to some other node.
  * May want the `Durable` value to just keep a small cache of "known mirrors". Question about eviction policy for this cache...
* It may be that we want some more optimized mutable storage, rather than just using this purely functional model. I could see a `Journal a` durable mutable storage type, which is based on super-fast append-only storage.
</p></details>

### <a id="history"></a>Appendix: History and context

__Most recently changes (after discussion in [#141](https://github.com/unisonweb/unison/issues/141)):__

* Got rid of `Clock` and `Index` in favor of immutable durable storage concept + mutable pointers.
* Got rid of `Channel` in favor of `Box`, also simplified `Capability` API to just build on `Box` directly.
* Got rid of `Heartbeat` arguments to a whole bunch of functions (like `Box.take`, etc), opting for just using the ambient lexically-scoped heartbeat established via `Remote.link`. 99% of the time this is what you want, and you can always push another `Heartbeat` onto the stack via a nested `Remote.link`.
* Clarified behavior around lifetimes of `Remote.fork`-ed computations and `Remote.spawn*` nodes--they always inherit the current ambient heartbeat. I believe this is key for composability, since it makes the interface for shutting down a subcomputation completely uniform.

__Previously:__

[This post](http://unisonweb.org/2015-06-02/distributed-evaluation.html) has an early writeup of how Unison's hashing scheme could be used to build a robust multi-node computation story. That eventually got an implementation, and as a demo I put together [a simple multi-node search engine](http://unisonweb.org/2016-10-12/search.html#post-start) in Unison. That raised a couple issues and questions, some discussed in that post, some discussed [in this post about microservices](http://unisonweb.org/2016-10-12/microservices.html#post-start), and some that I have just been ruminating on. 🤔

The big questions were around:

* Lifecycle management of nodes and durable data---when is durable data destroyed, and when are nodes destroyed? This led to the `Heartbeat` design.
* Encryption: how are things encrypted, both at rest (in durable storage) and in transit (when moving between nodes). The solution given here is to have 'in transit' encryption handled transparently by the runtime, but to have encryption keys for durable state to be managed explicitly by the programmer. This allows for multiple nodes to use a common storage layer, without all reads needing to go through a common node.
* Dynamic updates and redeployment---how is this done? Solution given is the `Capability` stuff.
