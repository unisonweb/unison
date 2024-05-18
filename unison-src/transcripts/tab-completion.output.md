# Tab Completion

Test that tab completion works as expected.

## Tab Complete Command Names

```ucm
.> debug.tab-complete vi

   view
   view.global
   view.patch

.> debug.tab-complete delete.

   delete.branch
   delete.namespace
   delete.namespace.force
   delete.patch
   delete.project
   delete.term
   delete.term-replacement
   delete.term.verbose
   delete.type
   delete.type-replacement
   delete.type.verbose
   delete.verbose

```
## Tab complete terms & types

```unison
subnamespace.someName = 1
subnamespace.someOtherName = 2
subnamespace2.thing = 3
othernamespace.someName = 4

unique type subnamespace.AType = A | B
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type subnamespace.AType
      othernamespace.someName    : ##Nat
      subnamespace.someName      : ##Nat
      subnamespace.someOtherName : ##Nat
      subnamespace2.thing        : ##Nat

```
```ucm
-- Should tab complete namespaces since they may contain terms/types
.> debug.tab-complete view sub

   subnamespace.
   subnamespace2.

-- Should not complete things from child namespaces of the current query if there are other completions at this level
.> debug.tab-complete view subnamespace

   subnamespace.
   subnamespace2.

-- Should complete things from child namespaces of the current query if it's dot-suffixed
.> debug.tab-complete view subnamespace.

  * subnamespace.AType
    subnamespace.AType.
  * subnamespace.someName
  * subnamespace.someOtherName

-- Should complete things from child namespaces of the current query if there are no more completions at this level.
.> debug.tab-complete view subnamespace2

    subnamespace2.
  * subnamespace2.thing

-- Should prefix-filter by query suffix
.> debug.tab-complete view subnamespace.some

  * subnamespace.someName
  * subnamespace.someOtherName

.> debug.tab-complete view subnamespace.someOther

  * subnamespace.someOtherName

-- Should tab complete absolute names
.othernamespace> debug.tab-complete view .subnamespace.some

  * .subnamespace.someName
  * .subnamespace.someOtherName

```
## Tab complete namespaces

```ucm
-- Should tab complete namespaces
.> debug.tab-complete find-in sub

   subnamespace
   subnamespace2

.> debug.tab-complete find-in subnamespace

   subnamespace
   subnamespace2

.> debug.tab-complete find-in subnamespace.

   subnamespace.AType

.> debug.tab-complete io.test sub

   subnamespace.
   subnamespace2.

.> debug.tab-complete io.test subnamespace

   subnamespace.
   subnamespace2.

.> debug.tab-complete io.test subnamespace.

    subnamespace.AType.
  * subnamespace.someName
  * subnamespace.someOtherName

```
Tab Complete Delete Subcommands

```unison
unique type Foo = A | B
add : a -> a
add b = b
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo
      add : a -> a

```
```ucm
.> update.old

  ⍟ I've added these definitions:
  
    type Foo
    add : a -> a

.> debug.tab-complete delete.type Foo

  * Foo
    Foo.

.> debug.tab-complete delete.term add

  * add

```
## Tab complete projects and branches

```ucm
.> project.create-empty myproject

  🎉 I've created the project myproject.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

myproject/main> branch mybranch

  Done. I've created the mybranch branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /mybranch`.

myproject/main> debug.tab-complete branch.delete /mybr

   /mybranch

myproject/main> debug.tab-complete project.rename my

   myproject

```
Commands which complete namespaces OR branches should list both

```unison
mybranchsubnamespace.term = 1
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      mybranchsubnamespace.term : ##Nat

```
```ucm
myproject/main> add

  ⍟ I've added these definitions:
  
    mybranchsubnamespace.term : ##Nat

myproject/main> debug.tab-complete merge mybr

   /mybranch

```
