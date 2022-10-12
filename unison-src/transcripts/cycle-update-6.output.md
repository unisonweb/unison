Not yet working: properly updating implicit terms with conflicted names.

```ucm
.> builtins.merge

  Done.

```
```unison
ping : 'Nat
ping _ = !pong + 1

pong : 'Nat
pong _ = !ping + 2

inner.pong : 'Nat
inner.pong _ = !ping + 3
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      inner.pong : 'Nat
      ping       : 'Nat
      pong       : 'Nat

```
N.B. The `find.verbose pong` is just to print the hash, for easy copying.

```ucm
.> add

  ⍟ I've added these definitions:
  
    inner.pong : 'Nat
    ping       : 'Nat
    pong       : 'Nat

.> find.verbose pong

  1. -- #lu6v9j6kvdigbcicuea5fd2e51o05rhgjp62gcgu13h7h59p7nockft2s20fflr4n6l59q7sf6l0fs8f8cnf0a4876dnvperel1vpa0
     inner.pong : 'Nat
     
  2. -- #4t465jk908dsue9fgdfi06fihppsme16cvaua29hjm1585de1mvt11dftqrab5chhla3reilsj4c0e7vlkkcct56khgaa5saeu4du48
     pong : 'Nat
     
  

.> merge inner

  Here's what's changed in the current namespace after the
  merge:
  
  New name conflicts:
  
    1. pong#4t465jk908 : 'Nat
       ↓
    2. ┌ pong#4t465jk908 : 'Nat
    3. └ pong#lu6v9j6kvd : 'Nat
  
  Name changes:
  
    Original         Changes
    4. inner.pong    5. pong#lu6v9j6kvd (added)
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```unison
ping : 'Nat
ping _ = ! #4t465jk908dsue9fgdfi06fihppsme16cvaua29hjm1585de1mvt11dftqrab5chhla3reilsj4c0e7vlkkcct56khgaa5saeu4du48 + 4
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      ping : 'Nat

```
```ucm
.> update

  ⍟ I've updated these names to your new definition:
  
    ping : 'Nat

.> view ping pong

  ping : 'Nat
  ping _ =
    use Nat +
    !pong#4t465jk908 + 4
  
  pong#4t465jk908 : 'Nat
  pong#4t465jk908 _ =
    use Nat +
    !#4t465jk908.1 + 2
  
  pong#hrsm7vhrcr : 'Nat
  pong#hrsm7vhrcr _ =
    use Nat +
    !ping + 3

```
Here we see that we didn't properly update `pong` to point to the new `ping because it was conflicted.
