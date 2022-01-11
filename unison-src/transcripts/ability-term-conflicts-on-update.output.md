# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786

First we add an ability to the codebase.
Note that this will create the name `Channels.send` as an ability constructor.

```unison
unique ability Channels where
  send : a -> {Channels} ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique ability Channels

```
```ucm
  ☝️  The namespace .ns is empty.

.ns> add

  ⍟ I've added these definitions:
  
    unique ability Channels

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

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Channels.send : a ->{Channels} ()
      thing         : '{Channels} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique ability Channels

```
Update should succeed, and treat `Channels.send` as an 'add', 
since the `Channels.send` constructor is being removed in the same update that 
the new top-level term is added.

```ucm
.ns> update

  ⍟ I've added these definitions:
  
    Channels.send : a ->{Channels} ()
    thing         : '{Channels} ()
  
  ⍟ I've updated these names to your new definition:
  
    unique ability Channels

```
