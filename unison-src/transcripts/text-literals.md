
```ucm:hide
.> builtins.merge
```

This transcript shows some syntax for raw text literals.

```unison
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

```ucm
.> add
.> view lit1 lit2
```