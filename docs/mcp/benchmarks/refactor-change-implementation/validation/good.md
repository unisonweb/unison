<!-- The route the topics teach for a type-preserving change: send the new definition and its tests, `update`, `test`.
     Nothing that uses `slug` is touched; `update` rebuilds it. -->

```` unison
Text.slug : Text -> Text
Text.slug title =
  keep c = isAlphaNum c || c == ?-
  collapse : [Char] -> [Char]
  collapse = cases
    ?- +: (?- +: rest) -> collapse (?- +: rest)
    c +: rest -> c +: collapse rest
    [] -> []
  trim cs = List.dropWhile (c -> c == ?-) (List.reverse (List.dropWhile (c -> c == ?-) (List.reverse cs)))
  title
    |> Text.toLowercase
    |> Text.replaceAll " " "-"
    |> Text.toCharList
    |> List.filter keep
    |> collapse
    |> trim
    |> Text.fromCharList

test> Text.slug.tests.dashes = test.verify do
  ensureEqual (Text.slug "  Hello --  World! ") "hello-world"
  ensureEqual (Text.slug "a - b") "a-b"
  ensureEqual (Text.slug "--x--") "x"
````

``` ucm
scratch/main> update
scratch/main> test
```

``` unison
> Text.slug "  Hello --  World! "
> Text.slug "a - b"
> Text.slug "Hello World"
> Text.slug "--x--"
> Post.url "My First Post"
```

``` ucm
scratch/main> names Text.initials
scratch/main> names Text.tests.initials
scratch/main> view Post.url Post.urls Post.anchor Site.index
scratch/main> find
scratch/main> branches
scratch/main> todo
```
