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

Channels.send : a -> ()
Channels.send a = ()

thing : '()
thing _ = send 1
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Channels.send : a -> ()
      thing         : '()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique ability Channels

```
These should fail with a term/ctor conflict since we exclude the ability from the update.

```ucm
.ns> update patch Channels.send

  x These definitions failed:
  
    Reason
    term/ctor collision   Channels.send   : a -> ()
  
    Tip: Use `help filestatus` to learn more.

.ns> update patch thing

  x These definitions failed:
  
    Reason
    term/ctor collision   Channels.send   : a -> ()
    blocked               thing           : '()
  
    Tip: Use `help filestatus` to learn more.

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
These updates should succeed since `Channels` is a dependency.

```ucm
.ns> update.preview patch Channels.send

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Channels.send : a ->{Channels} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique ability Channels

.ns> update.preview patch thing

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
We should also be able to successfully update the whole thing.

```ucm
.ns> update

  ⍟ I've added these definitions:
  
    Channels.send : a ->{Channels} ()
    thing         : '{Channels} ()
  
  ⍟ I've updated these names to your new definition:
  
    unique ability Channels

```
