# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786

First we add an ability to the codebase.
Note that this will create the name `Channels.send` as an ability constructor.

```unison
unique ability Channels where
  send : a -> {Channels} ()
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      ability Channels

```
```ucm
.ns> add

  ⍟ I've added these definitions:
  
    ability Channels

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

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Channels.send : a -> ()
      thing         : '{Channels} ()
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ability Channels

```
We should be able to update everything at once.

```ucm
.ns> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
If `Channels.send` and `thing` _depend_ on `Channels`, updating them should succeed since it pulls in the ability as a dependency.

```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> ()
Channels.send a = sends [a]

thing : '{Channels} ()
thing _ = send 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⊡ Previously added definitions will be ignored: Channels
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      Channels.send : a ->{Channels} ()
      thing         : '{Channels} ()

```
We should be able to successfully update the whole thing.

```ucm
.ns> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
# Constructor-term conflict

```unison
X.x = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      X.x : Nat

```
```ucm
.ns2> add

  ⍟ I've added these definitions:
  
    X.x : Nat

```
```unison
structural ability X where
  x : ()
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    x These definitions would fail on `add` or `update`:
    
      Reason
      blocked structural ability X
      ctor/term collision   X.x   
    
      Tip: Use `help filestatus` to learn more.

```
This should fail with a ctor/term conflict.

```ucm
.ns2> add

  x These definitions failed:
  
    Reason
    blocked structural ability X
    ctor/term collision   X.x   
  
    Tip: Use `help filestatus` to learn more.

```
