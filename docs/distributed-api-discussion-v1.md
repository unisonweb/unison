# Distributed programming API v1 discussion
Underlying the Unison distributed programming API is the desire to move computations to other Locations:

```haskell
Remote.forkAt : Loc {e} ->'({e} a) ->{Remote} Future a

-- example:
f1 = forkAt a 'let
	x = longRunningComputation 101
  Email.send x
y = otherLongComputation
-- x and y are computed in parallel
```

> We decided that automatically cancelling a child computation when its parent terminates or delaying termination of of the parent until its children complete would break associativity in terms of parallelism when chaining computations, therefore `forkAt` doesn’t enforce any such conditions.  See more about cancellation & termination below, in “Supervision and garbage-collection of Futures”  

## Locations
A Location is simply a computing context on some hardware somewhere, having access to certain computational resources.  Its runtime representation is essentially a collection of cryptographic tokens authorizing the use of these resources.

In Unison code, a Location is represented by a `Loc {e}`. A Unison value of type `Loc {}` supports only pure computations, whereas a `Loc {Remote, GPU}` provides the `Remote` and `GPU` abilities.

### Locations have a composite runtime representation
A `Loc` is represented by one or more host / port / auth tokens, along with ability use tokens.  The distributed runtime may use any algorithm in selecting a host to submit a task to, and the receiving host will run the computation provided the accompanying tokens are valid.

```haskell
-- Haskell runtime representation
-- individual Tokens should be cryptographically unguessable.
-- Tokens may correspond to or contain quota/other data.
data Loc = Loc Hosts Abilities Quota
type Token = TBD -- sufficient to prove authorization
type Hosts = Map (Hostname,Port) Token
type Abilities = Map Reference Token
type Quota = Set QUnit
data QUnit = QDisk QDiskUnit Token | QCpu QCpuUnit Token | QNet QNetUnit Token | ...
```
alternatively, if disk / cpu / network tokens only come in one size:
```haskell
type Quota = Map QType [Token]
data QType = QDisk | QCpu | QNetwork | ... 
```

Thanks to the composite representation, a `Loc` can be restricted if desired by simply dropping some ability tokens:
```haskell
Loc.restrict : Abilities {e} -> Loc {e,e2} -> Loc {e}
```
Similarly, disk/cpu quotas can be sub-allocated to various subtasks.
```haskell
Loc.take : QType -> Nat -> Loc {e} -> (Optional (Loc {e}), Loc {e})
-- or
Loc.take : QUnit -> Loc {e} -> (Optional (Loc {e}), Loc {e})
```
However, I don’t have a story for composing locations on the user side.  This may call for another type parameter on `Loc`, representing the provider.  More on this, below.

### Elastically producing new Locations
An elastic compute service “front-end” would expose:
	1. a function to `provision` new locations
	2. a Location at which the function could be run

* Can I have this `provision` function in my namespace, without having its implementation in my codebase?

- [ ] The implementation of `provision` would need some way to authenticate and validate the request. 
- [ ] It would need some way to construct a Unison `Loc` value (not yet discussed).
- [ ] It should provide a way for the front-end to monitor utilization and spin up or shut down physical resources as needed.

_Idea_: Maybe the `Token` value provided by the front-end is structured in a provider-specific way, with whatever data is needed to make these decisions.  Having a distinct `Token` type for distinct providers means another type parameter on the `Loc`, which could answer the question about consolidating `Loc`s on the user side.  If two Locations share the same provider type, they can be consolidated (hosts, quotas, abilities); otherwise they obviously couldn’t be.
```haskell
Remote.forkAt : Loc {e} p ->'({e} a) ->{Remote} Future a
Location.join : Loc {e} p -> Loc {e2} p -> Loc {e,e2} p
```

## Futures
A `Future` represents an asynchronous computation. `Remote.forkAt` takes a computation and returns immediately with a `Future`.  To wait for the computation’s output, use `Future.force`.
```haskell
Remote.forkAt : Loc {e} ->'({e} a) ->{Remote} Future a
Future.force  : Future a ->{Remote} (Either Err a)
type Err = TBD

-- example:
f1 = forkAt a 'let
	x = longRunningComputation 101
  makeHistogram x
y = otherLongComputation
x = Future.force f1
Database.save (x, y)
```

* How many times can a future be successfully forced?

### Supervision and garbage-collection of Futures
Unison Futures can be monitored or terminated using:
```haskell
Future.status : Future a ->{Remote} Future.Status
type Future.Status 
	= Running LastUpdate | Canceled | Finished 
  | Unreachable | Unresponsive 

Future.cancel : Future a ->{Remote} (Either Err2 ())
type Err2 = TBD
```

To the extent that an async computation should be canceled if there is no other computation interested in its result, we need some way of determining whether or not this is the case.  We discussed having a system of keep-alives, absent which a Future might be canceled by its host:
```haskell
-- these likely will just be handled by the interpreter 
-- of Remote, not by "user" code.
Future.keepalive : Duration -> Future a ->{Remote} Status
Future.remaining : Future a ->{Remote} Duration
```

Moreover, there will be cases where we want to transfer or delegate the keep-alive responsibility for a long-running tasks to a more available location. 
```haskell
Remote.supervise : Loc {e} -> Future a -> {Remote} ()
Remote.unsupervise : Loc {e} -> Future a -> {Remote} ()
```
  
> We discussed producing a `Heartbeat` identifier along with any `Future`, but decided there was no benefit to separating the two.  

We haven’t discussed how to prevent a delegate supervisor from accumulating and perpetuating many long-running Futures that will never actually be forced. With this in mind, have we gained anything from a GC perspective?

## Stationary data
We will need some notion of data that doesn't just move automatically with the computation, even if the computation references it.  We identified two reasons you might want to do this:
		* The data is big, and you don't want to copy it around willy-nilly.
		* The data is secret, and you don't want to accidentally ship it to another location, you want to be very explicit about when this happens (for instance, secret keys, etc).

More generally, we want a way of being explicit about when certain data is moved between locations, rather than implicitly relocating anything in lexical scope (this could be an API thing, a type-system thing, a code-analysis tool).
- - - -
## Notes/Desiderata
- [ ] Elastic computation - need to be able to talk about spawning new computing resources, and ideally this compute can be garbage collected as soon as you're done using it.
- [x] `fork` a task to run on a separate thread or at another "location"
- [x] Different locations may have access to different abilities (just pure computation, `IO`, `GPU`, etc)
- [ ] Need to be able to respond to location failures, with maximal flexibility. Allow different ways of doing failure detection/recovery.
* Locations are first-class, permissions, tasks, are first-class
	- [x] locations
	- [ ] permissions?
	- [x] tasks (futures)
- [ ] Some notion of data that doesn't just move automatically with the computation, even if the computation references it.
	* e.g., The data is big, and you don't want to copy it around willy nilly.
	* e.g., The data is secret, and you don't want to accidentally ship it to another location, you want to be very explicit about when this happens (for instance, secret keys, etc).
	* Might more generally want a way of being explicit about when data is moved to a location rather than just implicitly relocating anything in lexical scope (could be an API thing, a type system thing, a tool). 
- [x] Need to be able to launch a long-running computation and have it outlive the task / location / node that launches it. But then how do you interact with this computation later? (Say, to cancel it? Or to check if it's finished? Or more generally, how do you monitor it?)
- [ ] Need to be able to hash and serialize any Unison value, so that storage API(s) can be implemented in pure Unison. 
	* Should the hash of a value know the type of the value? `hash : a -> Hash a`)
- [x] How do you represent `Loc{e}` to be securely verified by the receiving node? The `Loc{e}` must be unguessable and tamper-proof.
	* This is achieved by making the component `Token`s unguessable and tamper-proof.
- [ ] Must be safe to say `at loc1 loc2` without allowing nefarious loc1 to abuse loc2.  (Needs clarification.)

- [x] The runtime needs an unguessable way (crypto?) to represent Locations and their abilities.
- [ ] Not all computations should have access to all data.
	* file system
	* individual durables
- [x] Not all Locations should provide unlimited resources to all users (arbitrary computation, time, storage, bandwidth, priority).
- [ ] Not all data should be portable to arbitrary locations (think secret keys, top secret clearance, hipaa).

_Misc?_:
* Mutable typed (durable if needed) state at each location
	* For v1, could not have this, just focus on batch computation
* Dealing with weird networks? (nat-busting)
	* Maybe in implementation, but not explicit in v1 API
* Well-defined semantics not just a bunch of implementation-defined gobbledygook
* Do we need globally-addressed mutable state? e.g. node `a` can refer to mutable data on node `b`; or node `c` can mutate data on node `d`. Yes, probably.

#unison