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

Update should succeed, and treat `Channels.send` as an 'add', 
since the `Channels.send` constructor is being removed in the same update that 
the new top-level term is added.

```ucm
.ns> update
```
