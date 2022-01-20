# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786

```ucm:hide
.builtins> builtins.mergeio
```

First we add an ability to the codebase.
Note that this will create the name `Channels.send` as an ability constructor.

```unison
unique ability Channels where
  send : a -> {Channels} ()
```

```ucm
.ns> add
```

Now we update the ability, changing the name of the constructor, _but_, we simultaneously
add a new top-level term with the same name as the constructor which is being
removed from Channels.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = ()

thing : '()
thing _ = send 1
```

These should fail with a term/ctor conflict since we exclude the ability from the update.

```ucm:error
.ns> update patch Channels.send
.ns> update patch thing
```

If however, `Channels.send` and `thing` _depend_ on `Channels`, updating them should succeed since it pulls in the ability as a dependency.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = sends [a]

thing : '()
thing _ = send 1
```

These updates should succeed since `Channels` is a dependency.

```ucm
.ns> update.preview patch Channels.send
.ns> update.preview patch thing
```

We should also be able to successfully update the whole thing.

```ucm
.ns> update
```

# Constructor-term conflict

```unison
X.x = 1
```

```ucm
.ns2> add
```

```unison:error
structural ability X where
  x : ()
```

This should fail with a ctor/term conflict.

```ucm:error
.ns2> add
```
