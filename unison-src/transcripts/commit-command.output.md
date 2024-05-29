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

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type MyRecord
      MyRecord.bool        : MyRecord -> Boolean
      MyRecord.bool.modify : (Boolean ->{g} Boolean)
                             -> MyRecord
                             ->{g} MyRecord
      MyRecord.bool.set    : Boolean -> MyRecord -> MyRecord
      MyRecord.nat         : MyRecord -> Nat
      MyRecord.nat.modify  : (Nat ->{g} Nat)
                             -> MyRecord
                             ->{g} MyRecord
      MyRecord.nat.set     : Nat -> MyRecord -> MyRecord
      MyRecord.text        : MyRecord -> Text
      MyRecord.text.modify : (Text ->{g} Text)
                             -> MyRecord
                             ->{g} MyRecord
      MyRecord.text.set    : Text -> MyRecord -> MyRecord
      addToRecordField     : MyRecord -> Nat
      lib.dep.dependency   : Nat
      termOne              : Nat
      termTwo              : Nat
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    14 | > addToRecordField (MyRecord 9 "hi" true)
           ⧩
           19

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    type MyRecord
    MyRecord.bool        : MyRecord -> Boolean
    MyRecord.bool.modify : (Boolean ->{g} Boolean)
                           -> MyRecord
                           ->{g} MyRecord
    MyRecord.bool.set    : Boolean -> MyRecord -> MyRecord
    MyRecord.nat         : MyRecord -> Nat
    MyRecord.nat.modify  : (Nat ->{g} Nat)
                           -> MyRecord
                           ->{g} MyRecord
    MyRecord.nat.set     : Nat -> MyRecord -> MyRecord
    MyRecord.text        : MyRecord -> Text
    MyRecord.text.modify : (Text ->{g} Text)
                           -> MyRecord
                           ->{g} MyRecord
    MyRecord.text.set    : Text -> MyRecord -> MyRecord
    addToRecordField     : MyRecord -> Nat
    lib.dep.dependency   : Nat
    termOne              : Nat
    termTwo              : Nat

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
.> experimental.commit

  x These definitions would fail on `add` or `update`:
  
    Reason
    needs update type MyRecord
    needs update   addToRecordField         : MyRecord -> ##Nat
    needs update   termOne                  : ##Nat
    blocked        MyRecord.getNat          : MyRecord -> '##Nat
    blocked        MyRecord.getNat.modify   : ('##Nat
                                              ->{g} '##Nat)
                                              -> MyRecord
                                              ->{g} MyRecord
    blocked        MyRecord.getNat.set      : '##Nat
                                              -> MyRecord
                                              -> MyRecord
    blocked        MyRecord.text            : MyRecord -> ##Text
    blocked        MyRecord.text.modify     : (##Text
                                              ->{g} ##Text)
                                              -> MyRecord
                                              ->{g} MyRecord
    blocked        MyRecord.text.set        : ##Text
                                              -> MyRecord
                                              -> MyRecord
  
    Tip: Use `help filestatus` to learn more.

```

```ucm
.> experimental.commit.> find.> view MyRecord.> ls MyRecord.> view addToRecordField.> view termOne
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  x These definitions would fail on `add` or `update`:
  
    Reason
    needs update type MyRecord
    needs update   addToRecordField         : MyRecord -> ##Nat
    needs update   termOne                  : ##Nat
    blocked        MyRecord.getNat          : MyRecord -> '##Nat
    blocked        MyRecord.getNat.modify   : ('##Nat
                                              ->{g} '##Nat)
                                              -> MyRecord
                                              ->{g} MyRecord
    blocked        MyRecord.getNat.set      : '##Nat
                                              -> MyRecord
                                              -> MyRecord
    blocked        MyRecord.text            : MyRecord -> ##Text
    blocked        MyRecord.text.modify     : (##Text
                                              ->{g} ##Text)
                                              -> MyRecord
                                              ->{g} MyRecord
    blocked        MyRecord.text.set        : ##Text
                                              -> MyRecord
                                              -> MyRecord
  
    Tip: Use `help filestatus` to learn more.

