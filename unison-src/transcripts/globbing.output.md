# Globbing 

## Overview

This allows quickly selecting terms, types, and namespaces for any "bulk" commands.

* Currently supports up to one wildcard PER SEGMENT; Each segment can have its own wildcard if you really want and it'll still be performant. E.g. `.base.?.to?`
* Can have a prefix, suffix or infix wildcard! E.g. `to?` or `?List` or `to?With!`
* I went with `?` instead of `*` for the wildcard symbol since `?` isn't currently a valid symbol name. This may cause some confusion since it differs from bash globbing though; so if anyone has thoughts/concerns about how to better handle this I'd love to hear them.
* Commands can select which targets they want globs to expand to; e.g. `cd` should only glob for namespace, `view` should only glob to terms & types.

## Demo

Add some definitions which we can match over:
```unison
convertToThing = 1
convertFromThing = 2

-- Nested definitions
nested.toList = 3
nested.toMap = 4
othernest.toList = 5
othernest.toMap = 6
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      convertFromThing : ##Nat
      convertToThing   : ##Nat
      nested.toList    : ##Nat
      nested.toMap     : ##Nat
      othernest.toList : ##Nat
      othernest.toMap  : ##Nat

```
Globbing as a prefix, infix, or suffix wildcard.

```ucm
.> view convert?

  convertFromThing : ##Nat
  convertFromThing = 2
  
  convertToThing : ##Nat
  convertToThing = 1

.> view convert?Thing

  convertFromThing : ##Nat
  convertFromThing = 2
  
  convertToThing : ##Nat
  convertToThing = 1

.> view ?Thing

  convertFromThing : ##Nat
  convertFromThing = 2
  
  convertToThing : ##Nat
  convertToThing = 1

```
Globbing can occur in any name segment.

```ucm
.> view ?.toList

  nested.toList : ##Nat
  nested.toList = 3
  
  othernest.toList : ##Nat
  othernest.toList = 5

.> view nested.to?

  nested.toList : ##Nat
  nested.toList = 3
  
  nested.toMap : ##Nat
  nested.toMap = 4

```
You may have up to one glob per name segment.

```ucm
.> view ?.to?

  nested.toList : ##Nat
  nested.toList = 3
  
  nested.toMap : ##Nat
  nested.toMap = 4
  
  othernest.toList : ##Nat
  othernest.toList = 5
  
  othernest.toMap : ##Nat
  othernest.toMap = 6

```
Globbing only expands to the appropriate argument type.

E.g. `view` should not see glob expansions for namespaces.
This should expand to the empty argument and silently succeed.

```ucm
.> view other?

```
