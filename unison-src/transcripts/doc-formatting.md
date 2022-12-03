This transcript explains a few minor details about doc parsing and pretty-printing, both from a user point of view and with some implementation notes.  The later stuff is meant more as unit testing than for human consumption.  (The ucm `add` commands and their output are hidden for brevity.)

Docs can be used as inline code comments.

```ucm:hide
.> builtins.merge
```

```unison
foo : Nat -> Nat
foo n =
  _ = [: do the thing :]
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
-- Lines (apart from the first line, i.e. the bit between the [: and the
-- first newline) are unindented until at least one of
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

```unison
-- You can do the following to avoid that problem.
doc6 = [:
            - foo
            - bar
          and the rest.
       :]
```

```ucm:hide
.> add
```
```ucm
.> view doc6
```

### More testing

```unison
-- Check empty doc works.
empty = [::]

expr = foo 1
```
```ucm:hide
.> add
```
```ucm
.> view empty
```

```unison
test1 = [:
The internal logic starts to get hairy when you use the \@ features, for example referencing a name like @List.take.  Internally, the text between each such usage is its own blob (blob ends here --> @List.take), so paragraph reflow has to be aware of multiple blobs to do paragraph reflow (or, more accurately, to do the normalization step where newlines with a paragraph are removed.)

Para to reflow: lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor ending in ref @List.take

@List.take starting para lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

Middle of para: lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor @List.take lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

  - non-para line (@List.take) with ref @List.take
  Another non-para line
  @List.take starting non-para line

  - non-para line with ref @List.take
before a para-line lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

  - non-para line followed by a para line starting with ref
@List.take lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor.

a para-line ending with ref lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor @List.take
  - non-para line

para line lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor lorem ipsum dolor
  @List.take followed by non-para line starting with ref.

@[signature] List.take

@[source] foo

@[evaluate] expr

@[include] doc1

-- note the leading space below
  @[signature] List.take

:]
```
```ucm:hide
.> add
```
```ucm
.> view test1
```

```unison
-- Regression test for #1363 - preservation of spaces after @ directives in first line when unindenting
reg1363 = [: `@List.take foo` bar
  baz :]
```
```ucm:hide
.> add
```
```ucm
.> view reg1363
```

```unison
-- Demonstrate doc display when whitespace follows a @[source] or @[evaluate]
-- whose output spans multiple lines.

test2 = [:
  Take a look at this:
  @[source] foo    ▶    bar
:]
```
```ucm:hide
.> add
```
View is fine.
```ucm
.> view test2
```
But note it's not obvious how display should best be handling this.  At the moment it just does the simplest thing:
```ucm
.> display test2
```
