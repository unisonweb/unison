<!-- https://github.com/unisonweb/unison/issues/4997 -->

# Delete namespace dependents check

This is a regression test, previously `delete.namespace` allowed a delete as long as the deletions had a name _anywhere_ in your codebase, it should only check the current project branch.

```unison
sub.dependency = 123

dependent = dependency + 99
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      dependent      : Nat
      sub.dependency : Nat

```
```ucm
myproject/main> add

  ⍟ I've added these definitions:
  
    dependent      : Nat
    sub.dependency : Nat

myproject/main> branch /new

  Done. I've created the new branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /new`.

myproject/new> delete.namespace sub

  Done.

myproject/new> view dependent

  dependent : Nat
  dependent =
    use Nat +
    #mllb0u5378 + 99

```
