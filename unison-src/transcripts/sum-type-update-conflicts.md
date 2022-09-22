# Regression test for updates which conflict with an existing data constructor

https://github.com/unisonweb/unison/issues/2786

```ucm:hide
.> builtins.mergeio
```

First we add a sum-type to the codebase.

```unison
structural type X = x
```

```ucm
.ns> add
.> cd .
```

Now we update the type, changing the name of the constructors, _but_, we simultaneously
add a new top-level term with the same name as the old constructor.

```unison
structural type X = y | z

X.x : Text
X.x = "some text that's not in the codebase"

dependsOnX = Text.size X.x
```

This update should succeed since the conflicted constructor
is removed in the same update that the new term is being added.

```ucm
.ns> update
```
