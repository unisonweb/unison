# Regression test for updates which conflict with an existing data constructor

https://github.com/unisonweb/unison/issues/2786

First we add a sum-type to the codebase.

```unison
structural type X = x
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type X
        (also named builtin.Unit)

```
```ucm
.ns> add

  ⍟ I've added these definitions:
  
    structural type X
      (also named builtin.Unit)

```
Now we update the type, changing the name of the constructors, _but_, we simultaneously
add a new top-level term with the same name as the old constructor.

```unison
structural type X = y | z

X.x : Text
X.x = "some text that's not in the codebase"

dependsOnX = Text.size X.x
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      X.x        : Text
      dependsOnX : Nat
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type X
        (The old definition is also named builtin.Unit.)

```
This update should succeed since the conflicted constructor
is removed in the same update that the new term is being added.

```ucm
.ns> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.

```
