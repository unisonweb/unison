Just a bunch of random parse errors to test the error formatting.

``` unison
x =
  foo.123
```

``` ucm
  Loading changes detected in scratch.u.

  I got confused here:
  
      2 |   foo.123
  
  
  I was surprised to find a 1 here.
  I was expecting one of these instead:
  
  * end of input
  * hash (ex: #af3sj3)
  * identifier (ex: abba1, snake_case, .foo.bar#xyz, .foo.++#xyz, or 🌻)

```

``` unison
namespace.blah = 1
```

``` ucm
  Loading changes detected in scratch.u.

  I got confused here:
  
      1 | namespace.blah = 1
  
  
  I was surprised to find a = here.
  I was expecting one of these instead:
  
  * ability
  * bang
  * binding
  * do
  * false
  * force
  * handle
  * if
  * lambda
  * let
  * newline or semicolon
  * quote
  * termLink
  * true
  * tuple
  * type
  * typeLink
  * use

```

``` unison
x = 1 ]
```

``` ucm
  Loading changes detected in scratch.u.

  I found a closing ']' here without a matching '['.
  
      1 | x = 1 ]
  

```

``` unison
x = a.#abc
```

``` ucm
  Loading changes detected in scratch.u.

  I got confused here:
  
      1 | x = a.#abc
  
  
  I was surprised to find a '.' here.
  I was expecting one of these instead:
  
  * and
  * bang
  * do
  * false
  * force
  * handle
  * if
  * infixApp
  * let
  * newline or semicolon
  * or
  * quote
  * termLink
  * true
  * tuple
  * typeLink

```

``` unison
x = "hi
```

``` ucm
  Loading changes detected in scratch.u.

  I got confused here:
  
      2 | 
  
  I was surprised to find an end of input here.
  I was expecting one of these instead:
  
  * "
  * \s
  * literal character

```

``` unison
y : a
```

``` ucm
  Loading changes detected in scratch.u.

  I got confused here:
  
      2 | 
  
  I was surprised to find an end of section here.
  I was expecting one of these instead:
  
  * ->
  * newline or semicolon

```
