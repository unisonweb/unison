
This transcript shows some syntax for raw text literals.

```unison
lit1 = """
This is a raw text literal.
It can start with 3 or more ",
and is terminated by the same number of quotes.
Nothing is escaped. \n

The initial newline, if it exists, is ignored.
"""

> lit1

lit2 = """"
This is a raw text literal.
It can start with 3 or more ",
and is terminated by the same number of quotes.
Nothing is escaped. \n

This doesn't terminate the literal - """
""""

> lit2
> Some lit2
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      lit1 : Text
      lit2 : Text
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    10 | > lit1
           ⧩
           "This is a raw text literal.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThe initial newline, if it exists, is ignored.\n"
  
    21 | > lit2
           ⧩
           "This is a raw text literal.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThis doesn't terminate the literal - \"\"\"\n"
  
    22 | > Some lit2
           ⧩
           Some
             "This is a raw text literal.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThis doesn't terminate the literal - \"\"\"\n"

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    lit1 : Text
    lit2 : Text

.> view lit1 lit2

  lit1 : Text
  lit1 =
    "This is a raw text literal.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThe initial newline, if it exists, is ignored.\n"
  
  lit2 : Text
  lit2 =
    "This is a raw text literal.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThis doesn't terminate the literal - \"\"\"\n"

```
