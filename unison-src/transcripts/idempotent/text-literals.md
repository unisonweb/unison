``` ucm :hide
scratch/main> builtins.merge
```

This transcript shows some syntax for raw text literals.

``` unison
lit1 = """
This is a raw text literal.
It can start with 3 or more ",
and is terminated by the same number of quotes.
Nothing is escaped. \n

The initial newline, if it exists, is ignored.
The last line, if it's just whitespace up to the closing quotes,
is ignored.

Use an extra blank line if you'd like a trailing newline. Like so:

"""

> lit1
> Some lit1

lit2 = """"
    This is a raw text literal, indented.
    It can start with 3 or more ",
    and is terminated by the same number of quotes.
    Nothing is escaped. \n

    This doesn't terminate the literal - """
    """"

> lit2
> Some lit2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lit1 : Text
  + lit2 : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.

    15 | > lit1
           ⧩
           """
           This is a raw text literal.
           It can start with 3 or more ",
           and is terminated by the same number of quotes.
           Nothing is escaped. \n
           
           The initial newline, if it exists, is ignored.
           The last line, if it's just whitespace up to the closing quotes,
           is ignored.
           
           Use an extra blank line if you'd like a trailing newline. Like so:
           
           """

    16 | > Some lit1
           ⧩
           Some
             "This is a raw text literal.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThe initial newline, if it exists, is ignored.\nThe last line, if it's just whitespace up to the closing quotes,\nis ignored.\n\nUse an extra blank line if you'd like a trailing newline. Like so:\n"

    27 | > lit2
           ⧩
           """"
           This is a raw text literal, indented.
           It can start with 3 or more ",
           and is terminated by the same number of quotes.
           Nothing is escaped. \n
           
           This doesn't terminate the literal - """
           """"

    28 | > Some lit2
           ⧩
           Some
             "This is a raw text literal, indented.\nIt can start with 3 or more \",\nand is terminated by the same number of quotes.\nNothing is escaped. \\n\n\nThis doesn't terminate the literal - \"\"\""
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view lit1 lit2

  lit1 : Text
  lit1 =
    """
    This is a raw text literal.
    It can start with 3 or more ",
    and is terminated by the same number of quotes.
    Nothing is escaped. \n
    
    The initial newline, if it exists, is ignored.
    The last line, if it's just whitespace up to the closing quotes,
    is ignored.
    
    Use an extra blank line if you'd like a trailing newline. Like so:
    
    """

  lit2 : Text
  lit2 =
    """"
    This is a raw text literal, indented.
    It can start with 3 or more ",
    and is terminated by the same number of quotes.
    Nothing is escaped. \n
    
    This doesn't terminate the literal - """
    """"
```
