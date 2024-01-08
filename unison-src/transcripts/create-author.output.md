Demonstrating `create.author`:

```unison
def1 = 1
def2 = 2
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    def1 : Nat
    def2 : Nat

.> create.author alicecoder "Alice McGee"

  Added definitions:
  
    1. metadata.authors.alicecoder          : Author
    2. metadata.copyrightHolders.alicecoder : CopyrightHolder
    3. metadata.authors.alicecoder.guid     : GUID
  
  Tip: Add License values for alicecoder under metadata.

.> view 2

  metadata.copyrightHolders.alicecoder : CopyrightHolder
  metadata.copyrightHolders.alicecoder =
    CopyrightHolder alicecoder.guid "Alice McGee"

```
