
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
.> add
.> view lit1 lit2
```