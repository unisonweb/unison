Before you write a function, find out whether the base library already has it. For almost anything you would write by hand there is a base function, and using it is shorter and better.

- Search before you write, every time. Whatever you are about to write (a loop over a list, a parser, a shuffle, running things at the same time, a counter) most likely exists. A hand-written version is longer, slower to get right, and usually worse.
- **Search by name,** with a few words: the verb, the type, and what you'd call it elsewhere. `find.all shuffle`, `find.all parMap`, `find.all capture`. `find` alone leaves out libraries, and the base library is a library, so use `find.all` (see [find](learn:ucm/find)).
- **Search by type,** which is the best way to find a function when you know what goes in and what comes out. `find.all : (a -> b) -> [a] -> [b]` lists `List.map`. `find.all : Nat -> Nat -> Nat` lists functions on two `Nat`s. Use type variables for what is generic.
- **Read the documentation of the types you are working with,** which usually lists the functions for them: `docs List`, `docs Map`, `docs Text`, `docs Stream`, `docs Pattern`. Then `docs <function>` for one.
- **Names resolve by their last parts.** Use `docs List.map` or `view Threads.parMap`, not the long name under `lib`. The long name shown by `find.all` has a folder in the middle (`lib.unison_base_7_20_0.data.List.map`) that you can't guess, so don't type one out.
- **Where things are, in the base library:** collections (`List`, `Map`, `Set`, `Stream`, `Heap`), `Text` and text parsing (`Pattern`, see [text](learn:lang/text)), numbers (`Nat`, `Int`, `Float`, and `Natural` and `Integer` for numbers of any size), failure (`Exception`, `Throw`, `Abort`, see [errors](learn:lang/errors)), randomness (`Random`), mutable state (`Scope`, `Ref`, see [mutable state](learn:lang/mutable)), running things at the same time (`IO.Threads`, see [concurrency](learn:lang/concurrency)), and tests (see [testing](learn:testing)).
- **If a search by type finds a function with exactly the type you were asked for, and its documentation describes the behavior you were asked for, you have found it:** the task is asking for something that exists under another name. Define yours as that function, `myName = existingFunction` (or `myName f xs = existingFunction f xs`), and test it. Don't write the same logic again, and don't extend it with more behavior than was asked.
- If you find something close but not exact, use it and adapt: wrap it, or give it a different function. Don't copy its logic.
- A task's own words are a hint. "At the same time" is `IO.Threads`, "from a seed" is `Random`, "that can fail" is `Exception` or `Throw`, "numbers in text" is `Pattern`, "a counter" is `Ref`.
- If base has nothing for the job (JSON, HTTP, dates, a file format), look on Unison Share for a library and use it: see [libraries from Share](learn:skills/library).
- Don't install a library that `list lib` already shows, and don't create new projects: work in the current one (see [libraries](learn:ucm/libraries)).

## Detail

**An example.** The task is a function that applies another to every element, concurrently:

````
``` ucm
myproject/main> find.all : (a -> b) -> [a] -> [b]
myproject/main> find.all parMap
myproject/main> docs Threads.parMap
```
````

The third command shows what `parMap` does, and that it needs the `Threads` ability.

**A long reply is written to a file.** When the reply says where, read or grep that file (see [transcripts](learn:ucm/transcript)).

**When `find.all` finds nothing.** Try other words, a shorter word, or the type. `find.all` matches names that contain the word, so `find.all sort` also finds `sortBy`.

**Reading source is fine** unless the task says not to. `view <name>` shows how a function is written, which is the best documentation of what it does (see [find](learn:ucm/find)).
