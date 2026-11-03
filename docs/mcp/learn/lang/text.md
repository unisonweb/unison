Working with `Text`: simple functions for simple cases, and `Pattern`, the base library's parser for text with structure, such as pulling pieces out of a string or checking its shape.

- `++` joins text, `Text.size` is the length, `Text.take n t` and `Text.drop n t` split off a prefix, `Text.splitOnText ", " t` splits at every occurrence of a separator and gives a `[Text]`, and `Text.toCharList` and `Text.fromCharList` convert to and from `[Char]`. `Nat.fromText "12"` and `Int.fromText "-3"` give an `Optional` (`None` if the text isn't a number). A `Char` is written `?a`. `'a'` is something else: a [delayed](learn:lang/delayed) `a`.
- **To find parts of a text or check its shape, use a `Pattern`, not a recursive loop over its characters.** A hand-written character loop is long and gets corner cases wrong. A pattern says what the text looks like.
- A `Pattern Text` matches text and can *capture* parts of it. `Pattern.run pattern text` is `Some (captures, rest)` if the pattern matches at the start of the text, where `captures : [Text]` is what the `Pattern.capture`s matched, in order, and `rest` is the text after the match. It is `None` if the pattern doesn't match there. End a pattern with `Text.patterns.eof` to require that all of the text is matched. `Pattern.isMatch pattern text` is just a `Boolean`.
- The pieces, in `Text.patterns`: `literal "abc"`, `digit`, `int` (an optional minus sign and then digits), `letter`, `space`, `anyChar`, `eof`, `charIn [?a, ?b]`, `notCharIn [?x]`, `notChars "ab"`, and more (`find.all Text.patterns.`).
- The ways to combine them, in `Pattern`: `join [a, b, c]` one after another, `or a b` either, `many p` zero or more times, `some p` one or more, `optional p`, and `capture p`, which keeps the text `p` matched.
- To repeat a pattern an exact number of times, `join (List.fill 2 Text.patterns.digit)` (the way to say "exactly two digits"), since `many` and `some` take any number.
- **Name the pieces,** like the parts of a regular expression, and then `join` the names. A named `digit`, `twoDigits` and `message` is much easier to read and fix than one big expression.
- Captures are `Text`, so convert them: `Nat.fromText`, `Int.fromText`. To collect every match in a text, make a pattern that is `many (or <what you want, captured> anyChar)`: at each position it either matches (and captures) or skips one character.

## Detail

**Collecting all matches.** The quoted parts of a text:

````
``` unison
quoted : Text -> [Text]
quoted t =
  open = Text.patterns.literal "\""
  inside = Pattern.capture (Pattern.many (Text.patterns.notChars "\""))
  piece = Pattern.join [open, inside, open]
  match Pattern.run (Pattern.many (Pattern.or piece Text.patterns.anyChar)) t with
    Some (found, _) -> found
    None -> []

> quoted "say \"hi\" to \"you\" now"
```
````

This gives `["hi", "you"]`.

**Checking a shape and capturing its parts.** A date such as `2024-05-17`, with exactly four, two and two digits:

````
``` unison
date : Text -> Optional (Nat, Nat, Nat)
date t =
  digits n = Pattern.capture (Pattern.join (List.fill n Text.patterns.digit))
  dash = Text.patterns.literal "-"
  pattern = Pattern.join [digits 4, dash, digits 2, dash, digits 2, Text.patterns.eof]
  match Pattern.run pattern t with
    Some ([y, m, d], _) ->
      match (Nat.fromText y, Nat.fromText m, Nat.fromText d) with
        (Some year, Some month, Some day) -> Some (year, month, day)
        _ -> None
    _ -> None

> date "2024-05-17"
> date "2024-5-17"
```
````

These give `Some (2024, 5, 17)` and `None`. `eof` is why `2024-05-170` is also `None`.

**A pattern on several lines.** Put the pieces of a `join` in a list, one to a line, inside the brackets (see [layout](learn:lang/layout) for where lines may begin):

````
pattern =
  Pattern.join
    [ digits 4
    , dash
    , digits 2
    ]
````

**Prefix, not whole text.** `Pattern.isMatch` and `Pattern.run` match at the start only: `Pattern.isMatch Text.patterns.digit "7x"` is `true`. `Text.patterns.isFullMatch pattern text` needs all of the text to match, as `eof` does.

**More.** `docs Pattern` and `docs Text.patterns.int` have the rest, and `docs Text` lists the plain functions (see [find](learn:ucm/find)).
