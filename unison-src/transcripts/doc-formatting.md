This transcript explains a few minor details about doc parsing and pretty-printing, both from a user point of view and with some implementation notes.  (The ucm `add` commands and their output are hidden for brevity.)

Docs can be used as inline code comments.

```unison
foo : Nat -> Nat
foo n =
  [: do the thing :]
  n + 1
```

```ucm:hide
.> add
```
```ucm
.> view foo
```

Note that `@` and `:]` must be escaped within docs.

```unison
escaping = [: Docs look [: like \@this \:] :]
```

```ucm:hide
.> add
```
```ucm
.> view escaping
```

(Alas you can't have `\@` or `\:]` in your doc, as there's currently no way to 'unescape' them.)

```unison
-- Note that -- comments are preserved within doc literals.
commented = [:
  example:

    -- a comment
    f x = x + 1
:]
```

```ucm:hide
.> add
```
```ucm
.> view commented
```

### Indenting, and paragraph reflow

Handling of indenting in docs between the parser and pretty-printer is a bit fiddly.

```unison
-- The leading and trailing spaces are stripped from the stored Doc by the
-- lexer, and one leading and trailing space is inserted again on view/edit
-- by the pretty-printer.
doc1 = [:   hi   :]
```

```ucm:hide
.> add
```
```ucm
.> view doc1
```

```unison
-- Lines (apart from the first line) are unindented until at least one of
-- them hits the left margin (by a post-processing step in the parser).
-- You may not notice this because the pretty-printer indents them again on
-- view/edit.
doc2 = [: hello
            - foo
            - bar
          and the rest. :]
```

```ucm:hide
.> add
```
```ucm
.> view doc2
```

```unison
doc3 = [: When Unison identifies a paragraph, it removes any newlines from it before storing it, and then reflows the paragraph text to fit the display window on display/view/edit.

For these purposes, a paragraph is any sequence of non-empty lines that have zero indent (after the unindenting mentioned above.)

 - So this is not a paragraph, even
   though you might want it to be.

   And this text  | as a paragraph
   is not treated | either.

Note that because of the special treatment of the first line mentioned above, where its leading space is removed, it is always treated as a paragraph.
   :]
```

```ucm:hide
.> add
```
```ucm
.> view doc3
```

```unison
doc4 = [: Here's another example of some paragraphs.

          All these lines have zero indent.

            - Apart from this one. :]
```

```ucm:hide
.> add
```
```ucm
.> view doc4
```

```unison
-- The special treatment of the first line does mean that the following
-- is pretty-printed not so prettily.  To fix that we'd need to get the
-- lexer to help out with interpreting doc literal indentation (because
-- it knows what columns the `[:` was in.)
doc5 = [:   - foo
            - bar
          and the rest. :]
```

```ucm:hide
.> add
```
```ucm
.> view doc5
```
TODO fence
```
-- You can do the following to avoid that problem.
doc6 = [:
            - foo
            - bar
          and the rest.
       :]
```
TODO
```
.> add
```
TODO
```
.> view doc6
```

TODO
```
-- You can do the following to avoid that problem.
empty = [::]
```
```
.> add
```
```
.> view empty
```
