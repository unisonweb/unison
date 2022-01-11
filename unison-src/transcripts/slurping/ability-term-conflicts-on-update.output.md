# Regression test for updates which conflict with an existing ability constructor

https://github.com/unisonweb/unison/issues/2786


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
```ucm
.ns> add

  ⊡ Ignored previously added definitions: Channels Channels.send

```
```unison
unique ability Channels where
  sends : [a] -> {Channels} ()

Channels.send : a -> {Channels} ()
Channels.send a = sends [a]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      unique ability Channels

```
```ucm
.ns> update

  ⍟ I've updated these names to your new definition:
  
    unique ability Channels

```
```unison
Channels.send : a -> {Channels} ()
Channels.send a = sends [a]
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Channels.send : a ->{Channels} ()

```
```ucm
.ns> add

  ⍟ I've added these definitions:
  
    Channels.send : a ->{Channels} ()

.ns> view Channels.send

  Channels.send : a ->{Channels} ()
  Channels.send a = sends [a]

```
