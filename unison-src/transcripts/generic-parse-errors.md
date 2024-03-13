Just a bunch of random parse errors to test the error formatting.

```unison:error
x = 
  foo.123
```

```unison:error
namespace.blah = 1
```

```unison:error
x = 1 ]
```

```unison:error
x = a.#abc
```

```unison:error
x = "hi
```

```unison:error
y : a 
```
