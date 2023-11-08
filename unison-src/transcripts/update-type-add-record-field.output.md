```ucm
.> builtins.merge

  Done.

.> move.namespace builtin lib.builtin

  Done.

```
```unison
structural type Foo = { bar : Nat }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural type Foo
    Foo.bar        : Foo -> Nat
    Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
    Foo.bar.set    : Nat -> Foo -> Foo

```
```unison
structural type Foo = { bar : Nat, baz : Int }
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      Foo.baz        : Foo -> Int
      Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
      Foo.baz.set    : Int -> Foo -> Foo
    
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      structural type Foo
      Foo.bar        : Foo -> Nat
      Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
      Foo.bar.set    : Nat -> Foo -> Foo

```
```ucm
.> update

  I propagated the update and am now saving the results.

  Done.

.> view Foo

  structural type Foo = { bar : Nat, baz : Int }

.> find.verbose

  1. -- #c8lqlau828dsiv3ht34ouvj99af6ni5ea8d0lcdgan8deo81bo5jp0e62o68m6h6sb48ankinjv69khjpv75efkuuopucgdeltrvpc8
     structural type Foo
     
  2. -- #plugs26tg47t470v7hm3kh0cqprom3mk2hjs1j3t4q1epphi9b4273fv77cabars6ka7kjibh8is807q3mcbg27phok3n7ug2vfkh7o
     Foo.bar : Foo -> Nat
     
  3. -- #ur93ievaahimqm5in7c01atktmls5ps7gg8c5ertmesh5joit8335g2no04p60dl9qsc021r2q1vg2l0f4310bps84fnhfp9gpb6be0
     Foo.bar.modify : (Nat ->{g} Nat) -> Foo ->{g} Foo
     
  4. -- #f64s8rv4ib3pqd3cjatm4u0cogtdlkrfo6biefb66r4q24ou72anglu0rv9l0gj7ifj82t8k82bkn2bdjlmc450qjsgl5a0mi9fqjf0
     Foo.bar.set : Nat -> Foo -> Foo
     
  5. -- #m1qs3bkkmhdbn36vtk9psegu7208n2rdgrt2c348ckb3jat7125fds8bjecbucebks0s0s36b3tbksoos98nmrfioqimkt48v6bh11o
     Foo.baz : Foo -> Int
     
  6. -- #p6avdaqjq2u15nb0bv36bjpp3sbespn5gt9ot89l28a99jn1majnval3n5g45ij5un4l8137peqpcohr4hafpi743edbbhc6ugredsg
     Foo.baz.modify : (Int ->{g} Int) -> Foo ->{g} Foo
     
  7. -- #68iqrrap68hjep8rjpgkg2o15dpsph0r7h6m43gfrbipn5m2de3la7qr0kk25l4r7b5otg6eihmtnmufg8klpapl6tvgin2i17m529g
     Foo.baz.set : Int -> Foo -> Foo
     
  8. -- #c8lqlau828dsiv3ht34ouvj99af6ni5ea8d0lcdgan8deo81bo5jp0e62o68m6h6sb48ankinjv69khjpv75efkuuopucgdeltrvpc8#0
     Foo.Foo : Nat -> Int -> Foo
     
  

```
