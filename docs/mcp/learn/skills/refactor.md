How to change code that exists. First decide which kind of change it is: a change that keeps every type the same can't break anything that uses the code, and a change to a type or a signature can. Each kind has a procedure, and the procedures use UCM to find the uses and [`rewrite`](learn:ucm/rewrite) to change them, so that you edit by hand only what is left.

- **To rename something, use [`move`](learn:ucm/rename-and-delete)** (`move foo bar`) and nothing else: it takes the term, the type and everything under the name (docs, tests, constructors) together, and uses don't change, since code refers to definitions by hash. A constructor named after its type (`Sku.Sku`) keeps its last segment, so after `move Sku Item` it is `Item.Sku`: `move Item.Sku Item.Item`. Never rename by editing the name in a file, except for a definition that isn't in the branch yet.
- **To change an implementation without changing its type,** put the definitions in a file of your own, `edit.to work.u foo bar` (give a path you can edit: the file is written where the server runs unless the path says otherwise), edit them there, and `update work.u`, which typechecks the file and saves it; if it reports errors, fix them and run it again. It applies cleanly, and nothing else changes. After a command that numbers its results, such as `find` or `dependents`, `edit.to work.u 1-` sends all of them to the file, and `edit.to work.u 1 5 16 20-25` some of them. `edit.to` adds to the top of a file that exists, leaving out what the file already has.
- **To change a type or a signature when the fix for each use is mechanical** (an added argument that gets a default, arguments in another order or combined into one, a constructor's fields changed), define the new version beside the old one under a suffix, `foo_v2`, have `rewrite` change the uses, then strip the suffix. The detail below has the steps. This is also the way to change an ability's requests.
- **When the uses need thought,** either do the direct thing (change the definition, `update`, and fix the file UCM hands back on a [temporary branch](learn:ucm/temporary-branch)), which is fine when `dependents foo` lists a few definitions, or walk the dependents outward in a work file (the detail below), using rewrites for the mechanical parts. Everything stays typechecking, in a file you can always go back to, until one `update` at the end.
- **To add a constructor to a type, or an operation to an ability,** find what matches on it: `rewrite.find` with a `case` rule that names an existing constructor, `matches = @rewrite case Shape.Circle ==> Shape.Circle` (see [rewrite](learn:ucm/rewrite)), lists every definition with a pattern on `Circle`, which is where a case for the new constructor goes; for an ability, name an operation, `case Journal.entry ==> Journal.entry`, and the list is the handlers. `edit.to work.u 1-` brings those into a file with the type (`edit.to work.u Shape`); add the constructor and the cases, `load`, `update`. **A function that doesn't match on the type needs no change** and doesn't belong in the file: `update` carries the new type through to it. If something in the file calls a definition of the branch that uses the old type (a test that calls another function on the ability, say), `load` adds that definition to the file, above the fold, under a comment saying why, and says so in the reply; it is typechecked with the file, yours to edit if the new definition doesn't fit it, and `update` saves it rebuilt. A function whose match ends in a wildcard isn't in the `rewrite.find` list and still typechecks (`dependents Shape` lists it), so decide whether its wildcard is right for the new case. Changing the type first and running `update` also works: the file UCM hands back has every definition that stopped typechecking, but not the ones with wildcards.
- **A rule** is a `@rewrite` block: `term a b ==> c` rewrites an expression, `case P ==> Q` a pattern, `signature T ==> U` a type in a signature, several to a block, each on its own line. `rewrite work.u:rules.r1,rules.r2 work.u` applies the rules of `work.u` to the definitions of `work.u` and writes the result to a new file, named in the reply. A rule can refer to anything the file defines. [Rewrite](learn:ucm/rewrite) has the full description. A rewrite can't define anything and can't add a case to a match or a handler: those you edit by hand.

## Detail

**Adding an argument.** `foo : Nat -> Nat -> Nat` is getting a third argument, and existing uses should pass `0`.

1. `edit.to work.u foo`. In the file, rename it to `foo_v2`, add the argument, and `load work.u` to check that it typechecks.
2. `dependents foo` lists what uses it, and `edit.to work.u 1-` adds those definitions to the file. (`load work.u` first, if the file has changed since the last `load`: `edit.to` leaves out definitions that the last loaded file already has, so that nothing is added twice.)
3. Add the rules to the file, in a namespace of their own:

   ````
   rules.r1 x y = @rewrite term foo x y ==> foo_v2 x y 0
   rules.r2 x = @rewrite term foo x ==> (y -> foo_v2 x y 0)
   rules.r3 = @rewrite term foo ==> (x y -> foo_v2 x y 0)
   ````

   The second and third catch partial applications: `List.map (foo 1) xs` and `foo` passed on its own. Write the rules rather than editing the uses in the file by hand, even when there are only a few: the partial applications and the pipelines (`xs |> foo`, `foo << g`) are where hand edits go wrong, and the rules handle all of them the same way.
4. `rewrite work.u:rules.r1,rules.r2,rules.r3 work.u`. The file is rewritten in place (the version before is saved under `rewrites/`, and the reply names it) and the reply says which definitions changed. The rewritten code refers to `foo_v2`, which the file also defines. Read the file, and look at anything you would rather change by hand, such as a caller that should take the new argument itself instead of passing `0`.
5. Strip `_v2` from the file (a `sed`), delete the `rules.` definitions from it, and `update work.u` (which loads the file and updates in one; a plain `update` saves what the last `load` typechecked). Every use of `foo` is in the file, so it applies cleanly. Give `foo` a [doc](learn:lang/documentation) for its new argument, and a test.

Changing the order of arguments, or combining two of them into a pair or a record, works the same way: a `_v2` version, rules for the full and the partial applications, strip, update.

**Changing a type.** The same recipe, with `signature` and `case` rules as well. Suppose `type Blah = C1 Nat Text | C2` should have `C1`'s fields the other way around. Give the constructors the suffix too, `type Blah_v2 = C1_v2 Text Nat | C2_v2`: with both types in the file, a pattern `C1 n t` would be ambiguous (term positions resolve by type, patterns don't). The rules, in one block:

````
rules.b1 n t = @rewrite
  signature Blah ==> Blah_v2
  case Blah.C1 n t ==> C1_v2 t n
  case Blah.C2 ==> C2_v2
  term Blah.C1 n t ==> C1_v2 t n
  term Blah.C1 ==> (n t -> C1_v2 t n)
  term Blah.C2 ==> C2_v2
````

The `signature` rule changes the signatures that mention `Blah`, the `case` rules the patterns, and the `term` rules the constructions, including the partially applied constructor. Then `rewrite work.u:rules.b1 work.u`, strip `_v2` (which turns `C1_v2` back into `C1`), delete the rules, `update work.u`. For an ability, the requests are the constructors.

**Walking the dependents.** For a change that rewrites don't fully express, keep a list of definitions whose type you changed, and bring their dependents into the file one round at a time:

1. `edit.to work.u foo`, make the new version `foo_v2`, and put `foo_v2` on the list.
2. Take a definition off the list, `dependents foo` for its old name, and `edit.to work.u 1-`. These are the immediate dependents only, which is the point: a dependent whose type stays the same is where the change stops. `load work.u` and fix the file until it typechecks against the new versions. Any definition whose type you had to change gets the suffix too, `bar_v2`, and goes on the list.
3. Repeat until the list is empty. At any round, when the edits are mechanical, write a rule in the file and `rewrite work.u:rules.r1 work.u` instead of editing by hand; the file is rewritten in place, so the next round carries on in it.
4. Strip `_v2`, delete the rules, `update work.u`.

Compared with the direct route, this is worth it when rewrites can do much of the work, or when there are many dependents: the direct route has one long stretch where nothing typechecks.

**When the results change.** A change that alters what a function returns (a handler that now records `WARN: ...`, say) makes the existing tests of it fail at `test` after the `update`. That is the tests doing their job, not a mistake in the refactoring: read each failure, and change the expectation where the new result is the intended one. Whatever you add gets a [doc](learn:lang/documentation) and a test, as any new definition does.

**Where the rules can come from, and what to rewrite:** see [rewrite](learn:ucm/rewrite). A rule that refers to a definition only the work file has is for rewriting that file: in a rewrite of the branch, the result would refer to something the branch doesn't have.
