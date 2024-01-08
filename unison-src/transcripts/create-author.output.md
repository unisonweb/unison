Demonstrating `create.author`:

```ucm
  ☝️  The namespace .foo is empty.

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
