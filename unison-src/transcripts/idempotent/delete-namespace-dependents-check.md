<!-- https://github.com/unisonweb/unison/issues/4997 -->

# Delete namespace dependents check

This is a regression test, previously `delete.namespace` allowed a delete as long as the deletions had a name _anywhere_ in your codebase, it should only check the current project branch.

``` ucm :hide
myproject/main> builtins.merge
```

``` unison
sub.dependency = 123

dependent = dependency + 99
```

``` ucm :error
myproject/main> add
myproject/main> branch /new
myproject/new> delete.namespace sub
myproject/new> view dependent
```
