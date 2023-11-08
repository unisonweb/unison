```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo = Bar Nat
structural type Baz = Qux Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Baz
      structural type Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Baz
    structural type Foo

```
```unison
structural type Foo = Bar Nat Nat
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Foo

```
```ucm
.> update

  I propagated the update and am now saving the results.

  Done.

.> view Foo

  structural type Foo = Bar Nat Nat

.> view Baz

  structural type Baz = Qux Foo

.> find.verbose

  1. -- #mcq2jd12o5j0u9svgj9eudg5qsc64mcqmqt3kag0bv6nvtkb01hsq5fifrspe4nbq8ujsng4708li1he2vs2mfrs75lrnrlhhvgeueg
     structural type Baz
     
  2. -- #mcq2jd12o5j0u9svgj9eudg5qsc64mcqmqt3kag0bv6nvtkb01hsq5fifrspe4nbq8ujsng4708li1he2vs2mfrs75lrnrlhhvgeueg#0
     Baz.Qux : Foo -> Baz
     
  3. -- #ui1efdev724dr1jkcofsj3spf1psmpt16ltq1bb3aprjh0casu15fliov6mb9jebi8122j8638anu4nmvuvk20i2locqfgqkmrho66g
     structural type Foo
     
  4. -- #ui1efdev724dr1jkcofsj3spf1psmpt16ltq1bb3aprjh0casu15fliov6mb9jebi8122j8638anu4nmvuvk20i2locqfgqkmrho66g#0
     Foo.Bar : Nat -> Nat -> Foo
     
  

```
