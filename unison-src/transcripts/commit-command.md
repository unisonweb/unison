```ucm:hide
.> builtins.merge
```

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
.> add
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
.> experimental.commit.preview
.> experimental.commit
.> find
.> view MyRecord
.> ls MyRecord
.> view addToRecordField
.> view termOne
```

This term should be deleted.

```ucm:error
.> view termTwo
```
