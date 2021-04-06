```ucm
.> alias.type ##Int Int

  Done.

```
```unison
type Optional a = None | Some a
type Boptional = Bconstructional (Optional ##Int)
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Boptional
      type Optional a

```
```ucm
  ☝️  The namespace .mytypes is empty.

.mytypes> add

  ⍟ I've added these definitions:
  
    type Boptional
    type Optional a

```
```unison
```

```ucm

  I loaded scratch.u and didn't find anything.

```
```ucm
.> names Optional

  Type
  Hash:  #5isltsdct9
  Names: mytypes.Optional

.> names Boptional

  Type
  Hash:  #5q7ug1s3tb
  Names: mytypes.Boptional

.> find

  1. builtin type Int
  2. type mytypes.Boptional
  3. mytypes.Boptional.Bconstructional : Optional Int
                                         -> Boptional
  4. type mytypes.Optional a
  5. mytypes.Optional.None : Optional a
  6. mytypes.Optional.Some : a -> Optional a
  

```
