Since this code block is expecting an error, we still hide it. It seems unusual to want to hide an error, but maybe it’s just too verbose or something. This follows the author’s intent.

``` unison :hide :error
x + x +
```

For comparison, here is what we get without the `:hide`.

``` unison :error
x + x +
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I got confused here:

      1 | x + x +


  I was surprised to find a x here.
  I was expecting one of these instead:

  * ability
  * class
  * namespace
  * newline or semicolon
  * type
  * use
```

Even though this code block has `:hide` on it, we should still see the error output, because it wasn’t expecting an error.

``` unison :hide
x + x +
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
I got confused here:

    1 | x + x +


I was surprised to find a x here.
I was expecting one of these instead:

* ability
* class
* namespace
* newline or semicolon
* type
* use
```
