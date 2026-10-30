<!-- Not a solution: step 1 of validation/good.md, kept apart because it changes nothing in the codebase. One rule,
     `Log.line level code ==> Log.lineSwapped code level`, rewrites every use that has at least two arguments (full
     calls, `|>`, `<|`, and the partial application `Log.line "ERROR" 500` passed to `List.map`) and the doc example.
     It leaves alone `Log.warn` and `Log.numbered`, which pass only the first argument, and `Table.line`. `rewrite`
     writes the result to `rewrites/<time>-<guid>.u` (a random name, so good.md goes on with the text). -->

``` unison
Log.lineSwapped : Nat -> Text -> Text -> Text
Log.lineSwapped code level message =
  use Text ++
  "[" ++ level ++ " " ++ Nat.toText code ++ "] " ++ message

swap level code = @rewrite term Log.line level code ==> Log.lineSwapped code level
```

``` ucm
scratch/main> rewrite.find :swap
```

``` ucm
scratch/main> rewrite :swap
```
