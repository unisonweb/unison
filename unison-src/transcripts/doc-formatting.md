This transcript verifies a few minor details about doc parsing and pretty-printing.

Docs can be used as inline code comments.

```unison
foo : Nat -> Nat
foo n =
  [: do the
     thing :]
  n + 1
```

```ucm
.> add
.> view foo
```

Note that `@` and `:]` must be escaped within docs.

```unison
escaping = [: Docs look [: like \@this \:] :]
```

```ucm
.> add
.> view escaping
```

(Alas you can't have `\@` or `\:]` in your doc, as there's currently no way to 'unescape' them.)

### Indenting

Handling of indenting in docs between the parser and pretty-printer is a bit fiddly.

```unison

-- The leading and trailing spaces are stripped from the stored Doc, and one
-- leading and trailing space is inserted again on view/edit
-- by the pretty-printer.
doc1 = [:   hi   :]

-- The above treatment is only applied to the first line of a Doc.
-- For storing subsequent lines, they are unindented until at least one of
-- them hits the left margin.  You don't notice this because the
-- pretty-printer indents them again on view/edit.
doc2 = [: hello
            - foo
            - bar
          and the rest. :]

-- That does mean that the following is pretty-printed not so prettily.
-- To fix that we'd need to get the lexer to help out with interpreting
-- doc literal indentation (because it knows what columns the `[:` was in.)
doc3 = [:   - foo
            - bar
          and the rest. :]

-- You can do the following to avoid that problem.
-- TODO this hashes the same as 3
doc4 = [:
            - foo
            - bar
          and the rest.
       :]

-- Note that -- comments are preserved within doc literals.
commented = [:
  example:

  -- a comment
  f x = x + 1
:]
```

```ucm
.> add
.> view doc1 doc2 doc3 doc4 commented
```

If we edit `foo` starting from the pretty-printed version above, and add it again, we end up with the same thing.

TODO

### Reflowing paragraphs

TODO
