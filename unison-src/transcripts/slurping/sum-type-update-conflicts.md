# Regression test for updates which conflict with an existing data constructor

https://github.com/unisonweb/unison/issues/2786

```ucm:hide
.builtins> builtins.mergeio
```

First we add a sum-type to the codebase.

```unison
structural type X = x
```

```ucm
.ns> add
```

Now we update the type, changing the name of the constructors, _but_, we simultaneously
add a new top-level term with the same name as the old constructor.

```unison
structural type X = y | z

X.x : Text
X.x = "some text that's not in the codebase"

dependsOnX = Text.size X.x
```

The 'update' will succeed up until it tries to resolve the reference to `X.x`
within `depondsOnX`, but `X.x` is actually not in the codebase!

```ucm:error
.ns> update
```
