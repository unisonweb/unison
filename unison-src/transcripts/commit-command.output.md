Add some definitions to the codebase for us to later update.

```unison
type MyRecord =
  { nat : Nat
  , text : Text
  , bool : Boolean
  }

lib.dep.dependency = 1
termOne = lib.dep.dependency + 2
termTwo = lib.dep.dependency + 3

addToRecordField : MyRecord -> Nat
addToRecordField rec = nat rec + 10

> addToRecordField (MyRecord 9 "hi" true)
```

Should be able to easily change and remove record fields and definitions in a single commit.

```unison
-- Rename and re-type the `nat` field to `getNat`
-- Remove the `bool` field
type MyRecord =
  { getNat : () -> Nat
  , text : Text
  }


-- Update termOne,
termOne = dependency + 20
-- termTwo is deleted simply by omitting it from the scratch file.

addToRecordField : MyRecord -> Nat
addToRecordField rec = !(getNat rec) + 10

> addToRecordField (MyRecord '9 "hi")
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      MyRecord.getNat        : MyRecord -> 'Nat
      MyRecord.getNat.modify : ('Nat ->{g} 'Nat)
                               -> MyRecord
                               ->{g} MyRecord
      MyRecord.getNat.set    : 'Nat -> MyRecord -> MyRecord
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type MyRecord
      MyRecord.text        : MyRecord -> Text
      MyRecord.text.modify : (Text ->{g} Text)
                             -> MyRecord
                             ->{g} MyRecord
      MyRecord.text.set    : Text -> MyRecord -> MyRecord
      addToRecordField     : MyRecord -> Nat
      termOne              : Nat
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    16 | > addToRecordField (MyRecord '9 "hi")
           ⧩
           19

```
```ucm
.> experimental.commit.preview

  Loading changes detected in scratch.u.

  Updates:
  
    1.  type MyRecord
        ↓
    2.  type MyRecord
    
    3.  addToRecordField : MyRecord -> Nat
        ↓
    4.  addToRecordField : MyRecord -> Nat
    
    5.  MyRecord.MyRecord : Nat -> Text -> Boolean -> MyRecord
        ↓
    6.  MyRecord.MyRecord : 'Nat -> Text -> MyRecord
    
    7.  MyRecord.text : MyRecord -> Text
        ↓
    8.  MyRecord.text : MyRecord -> Text
    
    9.  MyRecord.text.modify : (Text ->{g} Text)
        -> MyRecord
        ->{g} MyRecord
        ↓
    10. MyRecord.text.modify : (Text ->{g} Text)
        -> MyRecord
        ->{g} MyRecord
    
    11. MyRecord.text.set : Text -> MyRecord -> MyRecord
        ↓
    12. MyRecord.text.set : Text -> MyRecord -> MyRecord
    
    13. termOne : Nat
        ↓
    14. termOne : Nat
  
  Added definitions:
  
    15. MyRecord.getNat        : MyRecord -> 'Nat
    16. MyRecord.getNat.modify : ('Nat ->{g} 'Nat)
                               -> MyRecord
                               ->{g} MyRecord
    17. MyRecord.getNat.set    : 'Nat -> MyRecord -> MyRecord
  
  Removed definitions:
  
    18. MyRecord.bool        : MyRecord -> Boolean
    19. MyRecord.bool.modify : (Boolean ->{g} Boolean)
                             -> MyRecord
                             ->{g} MyRecord
    20. MyRecord.nat.modify  : (Nat ->{g} Nat)
                             -> MyRecord
                             ->{g} MyRecord
    21. MyRecord.nat         : MyRecord -> Nat
    22. MyRecord.bool.set    : Boolean -> MyRecord -> MyRecord
    23. MyRecord.nat.set     : Nat -> MyRecord -> MyRecord
    24. termTwo              : Nat

.> experimental.commit

  Loading changes detected in scratch.u.

  Updates:
  
    1.  type MyRecord
        ↓
    2.  type MyRecord
    
    3.  addToRecordField : MyRecord -> Nat
        ↓
    4.  addToRecordField : MyRecord -> Nat
    
    5.  MyRecord.MyRecord : Nat -> Text -> Boolean -> MyRecord
        ↓
    6.  MyRecord.MyRecord : 'Nat -> Text -> MyRecord
    
    7.  MyRecord.text : MyRecord -> Text
        ↓
    8.  MyRecord.text : MyRecord -> Text
    
    9.  MyRecord.text.modify : (Text ->{g} Text)
        -> MyRecord
        ->{g} MyRecord
        ↓
    10. MyRecord.text.modify : (Text ->{g} Text)
        -> MyRecord
        ->{g} MyRecord
    
    11. MyRecord.text.set : Text -> MyRecord -> MyRecord
        ↓
    12. MyRecord.text.set : Text -> MyRecord -> MyRecord
    
    13. termOne : Nat
        ↓
    14. termOne : Nat
  
  Added definitions:
  
    15. MyRecord.getNat        : MyRecord -> 'Nat
    16. MyRecord.getNat.modify : ('Nat ->{g} 'Nat)
                               -> MyRecord
                               ->{g} MyRecord
    17. MyRecord.getNat.set    : 'Nat -> MyRecord -> MyRecord
  
  Removed definitions:
  
    18. MyRecord.bool        : MyRecord -> Boolean
    19. MyRecord.bool.modify : (Boolean ->{g} Boolean)
                             -> MyRecord
                             ->{g} MyRecord
    20. MyRecord.nat.modify  : (Nat ->{g} Nat)
                             -> MyRecord
                             ->{g} MyRecord
    21. MyRecord.nat         : MyRecord -> Nat
    22. MyRecord.bool.set    : Boolean -> MyRecord -> MyRecord
    23. MyRecord.nat.set     : Nat -> MyRecord -> MyRecord
    24. termTwo              : Nat

.> find

  1.  addToRecordField : MyRecord -> Nat
  2.  type MyRecord
  3.  MyRecord.getNat : MyRecord -> 'Nat
  4.  MyRecord.getNat.modify : ('Nat ->{g} 'Nat)
                               -> MyRecord
                               ->{g} MyRecord
  5.  MyRecord.getNat.set : 'Nat -> MyRecord -> MyRecord
  6.  MyRecord.MyRecord : 'Nat -> Text -> MyRecord
  7.  MyRecord.text : MyRecord -> Text
  8.  MyRecord.text.modify : (Text ->{g} Text)
                             -> MyRecord
                             ->{g} MyRecord
  9.  MyRecord.text.set : Text -> MyRecord -> MyRecord
  10. termOne : Nat
  

.> view MyRecord

  type MyRecord = { getNat : 'Nat, text : Text }

.> ls MyRecord

  1. MyRecord ('Nat -> Text -> MyRecord)
  2. getNat   (MyRecord -> 'Nat)
  3. getNat/  (2 terms)
  4. text     (MyRecord -> Text)
  5. text/    (2 terms)

.> view addToRecordField

  addToRecordField : MyRecord -> Nat
  addToRecordField rec =
    use Nat +
    getNat rec () + 10

.> view termOne

  termOne : Nat
  termOne =
    use Nat +
    dependency + 20

```
This term should be deleted.

```ucm
.> view termTwo

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    termTwo

```
