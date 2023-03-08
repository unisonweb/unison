Define a simple record type:

```unison
unique type User = {
  id : Nat,
  name : Text
}
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type User
      User.id          : User -> Nat
      User.id.modify   : (Nat ->{g} Nat) -> User ->{g} User
      User.id.set      : Nat -> User -> User
      User.name        : User -> Text
      User.name.modify : (Text ->{g} Text) -> User ->{g} User
      User.name.set    : Text -> User -> User

```
It should now pretty-print as a record:

```ucm
.> view User

  unique type User = { id : Nat, name : Text }

```
But the `modify` function on record fields is changed with a pretty-printer roundtrip. This shouldn't prompt me to update:

```ucm
.> edit User.id.modify

  ☝️
  
  I added these definitions to the top of
  /Users/cpenner/dev/unison/scratch.u
  
    User.id.modify : (Nat ->{g} Nat) -> User ->{g} User
    User.id.modify f = cases User id name -> User (f id) name
  
  You can edit them there, then do `update` to replace the
  definitions currently in this namespace.

.> load scratch.u

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      User.id.modify : (Nat ->{g} Nat) -> User ->{g} User

```
If I go ahead and do the update, then my type is no longer treated as a record:

```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    User.id.modify : (Nat ->{g} Nat) -> User ->{g} User

.> view User

  unique type User = User Nat Text

```
