Documentation in Unison is a value of type `Doc`, written in `{{ ... }}` and attached to a definition by naming it `<name>.doc`. `docs <name>` renders it, with its examples evaluated.

- To document `foo`, define `foo.doc = {{ ... }}`, immediately before `foo` in the file. The same works for a type or an ability: `Foo.doc`.
- Inside `{{ }}` you write plain text paragraphs. `` `x` `` (single backticks) is plain monospaced text. A `#` line is a heading, and `*text*` is emphasis.
- At any point in documentation you can use double braces to transclude a Unison expression that returns a `Doc`: `{{ other.doc }}` embeds another documentation value, and `{{ Doc.Word "text" }}` a computed one. Functions that build a `Doc` are how documentation does more than plain text. `docExample` is one.
- To show a call inline as code that is checked, and hyperlinked when rendered, transclude a `docExample`: `{{ docExample 2 (do (flags list -> List.pick flags list)) }}` shows as `List.pick flags list`. The lambda's parameters are the argument names, and the number says how many of them to leave out of the introduction (the two arguments here), so the shown code is just the body. It is typechecked, so a mistake in it is an error.
- A fenced code block (three backticks, then the code, then three backticks) is an example. It is evaluated when the documentation is rendered, and shows the result next to it, so don't write the result yourself.
- Refer to other definitions with links: `{List.map}` for a term and `{type Map}` for a type or ability. They become hyperlinks when rendered, which backticks don't. A link to a name that doesn't exist is a type error, so the links are checked. To show type parameters too, write `` {type Map} `k v` ``.
- A usual shape: an embedded `docExample` of the call, which introduces the argument names, a short description ending in "For example:", examples (normal use first, then corner cases), a line `*Also see:* {a}, {b}`, and optionally an `# Implementation notes` heading for the cost (like `O(log n)`) or anything surprising.
- Examples can't be evaluated when the function uses IO. Define the example as its own term, `foo.docs.examples.exampleName : '{IO, Exception} ()` (see [delayed computations](learn:lang/delayed)), and put `@source{foo.docs.examples.exampleName}` in the documentation. Then show how to run it, a `run foo.docs.examples.exampleName` in a code block tagged `raw` (which isn't evaluated), and what it returns in a `@typecheck` block (or a `raw` block, if it prints output instead of returning a value). A function that needs abilities such as `Threads` should say so, and how to supply them.
- To show an expression as hyperlinked code without evaluating it, put `@typecheck` just before the fenced code block. It is typechecked and shown, but not run. Use it for a result or a short expression, not to define functions: put a definition in a `foo.docs.examples.exampleName` term and use `@source`.
- If you write documentation in a `unison` block of a [transcript](learn:ucm/transcript), the block contains fences of its own, so open and close the `unison` block with four backticks instead of three.

## Detail

**A documented function.**

`````
```` unison
List.take.doc = {{

{{ docExample 2 (do (n list -> List.take n list)) }} returns a list of the first `n` elements of `list`. For example:

```
List.take 2 [1,2,3,4]
```

A {List.take} of `0` elements returns the empty list:

```
List.take 0 [1,2,3,4]
```

*Also see:* {List.drop}, {List.takeWhile}

# Implementation notes

The implementation takes `O(log n)`, using the finger tree structure
of the {type List} type.

}}
````
`````

**A function that uses IO,** with a worked example in its own term:

`````
```` unison
IO.console.printLine.docs.examples.askName = do
  printLine "What is your name? \n> "
  name = readLine()
  printLine ("Hello there, " ++ name)

IO.console.printLine.doc = {{

{{ docExample 1 (do (msg -> printLine msg)) }} prints `msg` to standard out, with a newline at the end. For example:

@source{IO.console.printLine.docs.examples.askName}

You can run this in UCM via `run IO.console.printLine.docs.examples.askName`, and it produces
the output:

```raw
What is your name?
> Alice
Hello there, Alice
```

*Also see:* {Handle.putText}, {Handle.stdOut}

}}
````
`````

**Showing an expression without evaluating it.** `@typecheck` shows a hyperlinked, typechecked expression, for a result you want to display:

`````
```` unison
List.filterMap.doc = {{

{{ docExample 2 (do (f list -> List.filterMap f list)) }} keeps the `Some` results of applying `f`. For example, with

@typecheck ```
[None, Some 1, Some 2, None, Some 99]
```

as the results, the list is `[1, 2, 99]`.

}}
````
`````

**Types and abilities.** Give an overview of the type and the usual functions for working with it, a short paragraph and then sections as needed: `List.doc`, `Random.doc` and `Bytes.doc` are good models. Not every type needs that much.

**Learn from existing documentation.** `docs List.filterMap` and `docs List.drop` show the rendered result, and `view List.filterMap.doc` shows the source (see [find](learn:ucm/find)). For a library's documentation see [libraries](learn:ucm/libraries).
