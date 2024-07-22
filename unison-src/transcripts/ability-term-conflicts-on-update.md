# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786

```ucm:hide
scratch/main> builtins.merge lib.builtins
```

First we add an ability to the codebase.
Note that this will create the name `Channels.send` as an ability constructor.

```unison
unique ability Channels where
  send : a -> {Channels} ()
```

```ucm
scratch/main> add
```

Now we update the ability, changing the name of the constructor, _but_, we simultaneously
add a new top-level term with the same name as the constructor which is being
removed from Channels.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = ()

thing : '{Channels} ()
thing _ = send 1
```

These should fail with a term/ctor conflict since we exclude the ability from the update.

```ucm:error
scratch/main> update.old patch Channels.send
scratch/main> update.old patch thing
```

If however, `Channels.send` and `thing` _depend_ on `Channels`, updating them should succeed since it pulls in the ability as a dependency.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = sends [a]

thing : '{Channels} ()
thing _ = send 1
```

These updates should succeed since `Channels` is a dependency.

```ucm
scratch/main> update.old.preview patch Channels.send
scratch/main> update.old.preview patch thing
```

We should also be able to successfully update the whole thing.

```ucm
scratch/main> update.old
```

# Constructor-term conflict

```ucm:hide
scratch/main2> builtins.merge lib.builtins
```


```unison
X.x = 1
```

```ucm
scratch/main2> add
```

```unison
structural ability X where
  x : ()
```

This should fail with a ctor/term conflict.

```ucm:error
scratch/main2> add
```
