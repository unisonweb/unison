Demonstrating `create.author`:

```ucm
  ☝️  The namespace .foo is empty.

.foo> create.author alicecoder "Alice McGee"

  Added definitions:
  
    1. metadata.authors.alicecoder          : #345f3nptqq
    2. metadata.copyrightHolders.alicecoder : #pgornst1pq
    3. metadata.authors.alicecoder.guid     : #hqectlr3gt
  
  Tip: Add License values for alicecoder under metadata.

.foo> view 2

  .foo.metadata.copyrightHolders.alicecoder : CopyrightHolder
  .foo.metadata.copyrightHolders.alicecoder =
    CopyrightHolder alicecoder.guid "Alice McGee"

```
