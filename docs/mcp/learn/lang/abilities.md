An ability is Unison's way to describe an effect a function can have, such as failing, random numbers, or IO. A function's type lists the abilities it uses, and a handler turns code that uses one into code that doesn't.

- The abilities of a function are in braces after an arrow: `Nat ->{IO, Exception} ()`. `{}` or nothing means pure. A lowercase name, as in `(a ->{g} b) ->{g} [b]`, stands for whatever abilities the function argument uses, and passes them through.
- An ability has operations you call like functions: `Random.natIn 0 10`, `Abort.abort`, `Exception.raise failure`. To see an ability's operations, `view <Ability>` (see [find](learn:ucm/find)).
- A function that uses an ability can only be called where something handles it, or from a function whose type lists it too.
- A **handler** is a function that runs a [delayed computation](learn:lang/delayed) using the ability and gives a result without it. Handlers by the ability: `catch` for `Exception`, `toEither` for `Throw`, `toOptional!` for `Abort`, `Random.run` or `splitmix seed` for `Random`.
- A handler that returns the plain value, not a delayed computation, is conventionally named with `!`. `toOptional! do ...` handles `Abort`, and gives `Optional a`.
- Nest handlers for several abilities. Each removes one.
- At the top level, an ability can't be left unhandled: a program is a delayed computation, like `'{IO, Exception} ()`, which `run` runs (see [running code](learn:ucm/run)).
- **A function that takes a function or a delayed computation passes its abilities through.** Name them with a variable, `g`, in the argument and in the result: `List.map : (a ->{g} b) -> [a] ->{g} [b]`, `Stream.map : (a ->{g} b) -> '{g, Stream a} r -> '{g, Stream b} r`. Without it, callers whose argument uses an ability (a counter, `IO`, a logger) can't use your function. Do this for every higher-order function and every function that takes a `'{...}` argument, and give *every* such argument the variable, not only the first: a function that interleaves two streams has the type `'{g, Stream a} r -> '{g, Stream a} s -> '{g, Stream a} ()`, so that either stream can use other abilities too. Look at a similar base function (`view Stream.map`) for the shape.
- **Return a delayed computation only for a computation that is itself the value,** like a `Stream` or a coroutine: take and return thunks, `'{g, Stream a} r -> '{g, Stream b} r`. For anything else, run the effects and put the abilities on the final arrow, `Nat ->{IO, Exception} Text`. The caller can write `do f x` to delay it.
- The common abilities are `IO`, `Exception`, `Abort`, `Throw`, `Random`, `Each`, `Stream` and `Label` (see [errors](learn:lang/errors) and [the testing abilities](learn:testing/abilities)).

## Detail

**Declaring an ability and writing a handler.**

````
``` unison
ability KVStore a b where
  get : a ->{KVStore a b} Optional b
  put : a -> b ->{KVStore a b} ()

inMemory : '{g, KVStore a b} r ->{g} r
inMemory program =
  impl : Map a b -> Request {KVStore a b} r -> r
  impl map = cases
    { pure } -> pure
    { KVStore.get key -> resume } ->
      handle resume (Map.get key map) with impl map
    { KVStore.put key value -> resume } ->
      handle resume() with impl (Map.insert key value map)
  handle program() with impl Map.empty

useStore : '{KVStore Text Nat} (Optional Nat)
useStore = do
  KVStore.put "id" 5
  KVStore.get "id"

> inMemory useStore
```
````

Each operation in the declaration is a request. The handler matches them as patterns: `{ pure }` is the case where the computation finishes with a value, and `{ Operation args -> resume }` is a request, where `resume` is the rest of the computation, to continue with the operation's answer. `handle resume value with impl state` carries on under the same handler, with new state. `handle program() with impl initial` starts it.

**Pure handlers.** A handler that ends in `!` runs the computation itself. `toOptional! : '{g, Abort} a ->{g} Optional a`. Without the `!`, `toDefault` returns a delayed computation instead.

**Ability-polymorphic functions.** `stopIfTrue : (a ->{g} Boolean) -> a ->{g, Abort} a` uses `Abort` and whatever `g` the predicate uses.
