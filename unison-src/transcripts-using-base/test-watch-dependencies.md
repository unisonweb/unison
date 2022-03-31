# Ensure test watch dependencies are properly considered.

https://github.com/unisonweb/unison/issues/2195

```ucm:hide
.> builtins.merge
```

We add a simple definition.

```unison:hide
x = 999
```

```ucm:hide
.> add
```

Now, we update that definition and define a test-watch which depends on it.

```unison
x = 1000
test> mytest = checks [x + 1 == 1001]
```

We expect this 'add' to fail because the test is blocked by the update to `x`.

```ucm:error
.> add
```

---

```unison
y = 42
test> useY = checks [y + 1 == 43]
```

This should correctly identify `y` as a dependency and add that too.

```ucm
.> add useY
```
