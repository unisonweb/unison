``` unison
Page.title : Text
Page.title = "Home"

Page.render : Text -> Text
Page.render body = Page.title ++ "\n" ++ body

test> Page.render.tests.example = test.verify do
  ensureEqual (Page.render "hi") "Home\nhi"
```

``` ucm
scratch/main> add
scratch/main> branch alice
scratch/main> branch bob
```

Alice changes the title and how a page is rendered.

``` ucm
scratch/alice> ls Page
```

``` unison
Page.title : Text
Page.title = "Home page"

Page.render : Text -> Text
Page.render body = "== " ++ Page.title ++ " ==\n" ++ body

test> Page.render.tests.example = test.verify do
  ensureEqual (Page.render "hi") "== Home page ==\nhi"
```

``` ucm
scratch/alice> update
```

Bob deletes `Page.render`, changes the title, adds a debugging helper, and adds `Text.slug` with a test.

``` ucm
scratch/bob> delete.term Page.render.tests.example
scratch/bob> delete.term Page.render
```

``` unison
Page.title : Text
Page.title = "Welcome"

Debug.trace : Text -> Text
Debug.trace t = "TRACE " ++ t

Text.slug : Text -> Text
Text.slug t = Text.replaceAll " " "-" (Text.toLowercase t)

test> Text.slug.tests.example = test.verify do
  ensureEqual (Text.slug "Hello World") "hello-world"
  ensureEqual (Text.slug "Already-fine") "already-fine"
```

``` ucm
scratch/bob> update
```

Alice tries to merge Bob's branch. It cannot finish, and she is left on the temporary branch.

``` ucm :error
scratch/alice> merge /bob
```
