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

Channels.send : a -> {Channels} ()
Channels.send a = sends [a]

thing : '{Channels} ()
thing _ = send 1
```

The 'update' will succeed up until it tries to resolve the reference to `send`
within `thing`; because the old send is deleted and the new `send` isn't ever added.

```ucm:error
.ns> update
```
