# Example 1

We have a namespace type named `Namespace.Foo` and a file type named `File.Foo`. A reference to the type `Foo` is
ambiguous. A reference to `Namespace.Foo` or `File.Foo` work fine.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.

```
``` unison
type Namespace.Foo = Bar
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Namespace.Foo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Namespace.Foo

```
``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm

  Loading changes detected in scratch.u.

  
    ❓
    
    I couldn't resolve any of these symbols:
    
        2 | type UsesFoo = UsesFoo Foo
    
    
    Symbol   Suggestions
             
    Foo      File.Foo
             Namespace.Foo
  

```
``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Namespace.Foo File.Foo
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type File.Foo
      type UsesFoo

```
``` ucm
scratch/main> project.delete scratch

```
# Example 2

We have a namespace type named `Foo` and a file type named `File.Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the namespace type (because it is an exact match).

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.

```
``` unison
type Foo = Bar
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Foo

```
``` unison
type File.Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type File.Foo
      type UsesFoo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type File.Foo
    type UsesFoo

scratch/main> view UsesFoo

  type UsesFoo = UsesFoo Foo

```
``` ucm
scratch/main> project.delete scratch

```
# Example 3

We have a namespace type named `Namespace.Foo` and a file type named `Foo`. A reference to the type `Foo` is not ambiguous:
it refers to the file type (because it is an exact match).

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.

```
``` unison
type Namespace.Foo = Bar
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Namespace.Foo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Namespace.Foo

```
``` unison
type Foo = Baz
type UsesFoo = UsesFoo Foo
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo
      type UsesFoo

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    type Foo
    type UsesFoo

scratch/main> view UsesFoo

  type UsesFoo = UsesFoo Foo

```
``` ucm
scratch/main> project.delete scratch

```
# Example 4

We have a namespace term `Woot.state : Nat` and a file term `Something.state : Text -> Something`. A reference to the
term `state : Text` resolves to `Something.state`, which shadows `Woot.state`. (This behavior will change).

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.

```
``` unison
Woot.state : Nat
Woot.state = 42
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Woot.state : Nat

```
``` ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    Woot.state : Nat

```
``` unison
type Something = { state : Text }

ex = do
  s = Something "hello"
  state s ++ " world!"
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Something
      Something.state        : Something -> Text
      Something.state.modify : (Text ->{g} Text)
                               -> Something
                               ->{g} Something
      Something.state.set    : Text -> Something -> Something
      ex                     : 'Text

```
``` ucm
scratch/main> project.delete scratch

```
