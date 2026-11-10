`Text.slug` turns a title into a URL slug, and four definitions use it, with tests. The existing tests don't exercise repeated or leading dashes, so the change the prompt asks for keeps them passing. `Text.initials` and its test don't use `slug`.

```` unison
Text.slug : Text -> Text
Text.slug title =
  keep c = isAlphaNum c || c == ?-
  title
    |> Text.toLowercase
    |> Text.replaceAll " " "-"
    |> Text.toCharList
    |> List.filter keep
    |> Text.fromCharList

Text.slug.doc = {{
{{ docExample 1 (do (title -> Text.slug title)) }} turns a title into a slug for a URL: lowercase, spaces as dashes,
only letters, digits and dashes kept. For example:

@eval{ Text.slug "Hello World" }
}}

Post.url : Text -> Text
Post.url title = "/posts/" ++ Text.slug title

Post.urls : [Text] -> [Text]
Post.urls = List.map Post.url

Post.anchor : Text -> Text
Post.anchor heading = "#" ++ Text.slug heading

Site.index : [Text] -> Text
Site.index titles = Text.join "\n" (List.map (title -> "- [" ++ title ++ "](" ++ Post.url title ++ ")") titles)

Text.initials : Text -> Text
Text.initials name = Text.fromCharList (List.filterMap (word -> List.head (Text.toCharList word)) (Text.split ?  name))

test> Text.slug.tests.basic = test.verify do
  ensureEqual (Text.slug "Hello World") "hello-world"
  ensureEqual (Text.slug "Unison 101!") "unison-101"

test> Post.tests.url = test.verify do
  ensureEqual (Post.url "My First Post") "/posts/my-first-post"
  ensureEqual (Post.urls ["A B", "C"]) ["/posts/a-b", "/posts/c"]
  ensureEqual (Post.anchor "Getting Started") "#getting-started"

test> Site.tests.index = test.verify do
  ensureEqual (Site.index ["Hi There"]) "- [Hi There](/posts/hi-there)"

test> Text.tests.initials = test.verify do
  ensureEqual (Text.initials "Ada Lovelace") "AL"
````

``` ucm
scratch/main> add
scratch/main> test
```
