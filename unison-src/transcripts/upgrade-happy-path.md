``` ucm :hide
proj/main> builtins.merge lib.builtin
```

``` unison
lib.old.foo = 17
lib.new.foo = 18
thingy = lib.old.foo + 10
```


``` ucm
proj/main> add
```

Test tab completion and fzf options of upgrade command.

``` ucm
proj/main> debug.tab-complete upgrade ol
proj/main> debug.fuzzy-options upgrade _
proj/main> debug.fuzzy-options upgrade old _
```

``` ucm
proj/main> upgrade old new
proj/main> ls lib
proj/main> view thingy
```
