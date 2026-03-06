# Underscore separators in bytes literals

Unison supports underscores as visual separators in bytes literals (`0xs...`).
Underscores can appear between hex digits and are stripped before evaluation.

``` ucm :hide
> builtins.merge
```

## Valid literals

Bytes with underscores:

``` unison
> 0xs01_ef
> 0xsAA_BB_CC
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > 0xs01_ef
          ⧩
          0xs01ef

    2 | > 0xsAA_BB_CC
          ⧩
          0xsaabbcc
```

Bytes without underscores still work:

``` unison
> 0xs01ef
> 0xs
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > 0xs01ef
          ⧩
          0xs01ef

    2 | > 0xs
          ⧩
          0xs
```

## Invalid literals

Trailing underscore:

``` unison :error
x = 0xs01_
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = 0xs01_


  I was surprised to find a 
   here.
  I was expecting one of these instead:

  * hexadecimal character
```

Consecutive underscores:

``` unison :error
x = 0xs01__ef
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = 0xs01__ef


  I was surprised to find a _ here.
  I was expecting one of these instead:

  * hexadecimal character
```

Underscore immediately after prefix:

``` unison :error
x = 0xs_01
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x = 0xs_01


  I was surprised to find a _ here.
  I was expecting one of these instead:

  * hexadecimal character
```
