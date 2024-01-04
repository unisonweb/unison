Demonstrating `create.author`:

```unison
def1 = 1
def2 = 2
```

```ucm
  ☝️  The namespace .foo is empty.

.foo> add

  ⍟ I've added these definitions:
  
    def1 : Nat
    def2 : Nat

.foo> create.author alicecoder "Alice McGee"

  Added definitions:
  
    1. metadata.authors.alicecoder          : Author
    2. metadata.copyrightHolders.alicecoder : CopyrightHolder
    3. metadata.authors.alicecoder.guid     : GUID
  
  Tip: Add License values for alicecoder under metadata.

.foo> view 2

  metadata.copyrightHolders.alicecoder : CopyrightHolder
  metadata.copyrightHolders.alicecoder =
    CopyrightHolder guid "Alice McGee"

```
