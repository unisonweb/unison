# Underscore separators in numeric literals

Unison supports underscores as visual separators in numeric literals.
Underscores can appear between digits in any numeric base but are
stripped before evaluation.

``` ucm :hide
> builtins.merge
```

## Valid literals

Decimal integers:

``` unison
> 1_000
> 1_000_000
> +1_000
> -1_000
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > 1_000
          ⧩
          1000

    2 | > 1_000_000
          ⧩
          1000000

    3 | > +1_000
          ⧩
          +1000

    4 | > -1_000
          ⧩
          -1000
```

Floating-point and scientific notation:

``` unison
> 1_000.5
> 1_000.000_001
> 1_000e1_0
> 1_000.5e1_0
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > 1_000.5
          ⧩
          1000.5

    2 | > 1_000.000_001
          ⧩
          1000.000001

    3 | > 1_000e1_0
          ⧩
          1.0e13

    4 | > 1_000.5e1_0
          ⧩
          1.0005e13
```

Hexadecimal, octal, and binary:

``` unison
> 0xFF_FF
> 0o77_77
> 0b1010_0101
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > 0xFF_FF
          ⧩
          65535

    2 | > 0o77_77
          ⧩
          4095

    3 | > 0b1010_0101
          ⧩
          165
```

## Invalid literals

Trailing underscores and consecutive underscores are rejected:

``` unison :error
x = 1_
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = 1_


  I was surprised to find a 
   here.
  I was expecting one of these instead:

  * decimal digit
  * end of input
```

``` unison :error
x = 1__2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = 1__2


  I was surprised to find a _ here.
  I was expecting one of these instead:

  * decimal digit
  * end of input
```

``` unison :error
x = 0xFF_
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = 0xFF_


  I was surprised to find a 
   here.
  I was expecting one of these instead:

  * hexadecimal digit
```
