How to change code that exists. First decide which kind of change it is: a change that keeps every type the same can't break anything that uses the code, and a change to a type or a signature can. Each kind has a procedure, and the procedures use UCM to find and rewrite the uses, so that you edit by hand only what is left.

- **To rename something, use [`move`](learn:ucm/rename-and-delete)** (`move foo bar`) and nothing else. Uses don't change, since code refers to definitions by hash. Never rename by editing the name in a file, except for a definition that isn't in the branch yet.
- **To change an implementation without changing its type,** put the definitions in a file of your own, `edit.to work.u foo bar`, edit them there, `load work.u` until it typechecks, and [`update`](learn:ucm/update). It applies cleanly, and nothing else changes. After a command that numbers its results, such as `find` or `dependents`, `edit.to work.u 1-` sends all of them to the file, and `edit.to work.u 1 5 16 20-25` some of them. `edit.to` adds to the top of a file that exists.
- **To change a type or a signature when the fix for each use is mechanical** (an added argument that gets a default, arguments in another order or combined into one, a constructor's fields changed), define the new version beside the old one under a suffix, `foo_v2`, have `rewrite` change the uses, then strip the suffix. The detail below has the steps. This is also the way to change an ability's requests.
- **When the uses need thought,** either do the direct thing (change the definition, `update`, and fix the file UCM hands back on a [temporary branch](learn:ucm/temporary-branch)), which is fine when `dependents foo` lists a few definitions, or walk the dependents outward in a work file (the detail below), using rewrites for the mechanical parts. Everything stays typechecking, in a file you can always go back to, until one `update` at the end.
- **A rule** is a `@rewrite` block. `term a b ==> c` rewrites an expression (`a` and `b` stand for any expression), `case P ==> Q` rewrites a pattern in a `match` or `cases`, and `signature T ==> U` rewrites a type in a signature. A block can have several, each on its own line. `rewrite work.u:rules.r1,rules.r2 work.u` applies the rules of `work.u` to the definitions of `work.u` and writes the result to a new file, named in the reply, `rewrites/work-<time>-<guid>.u`. A rule can refer to anything the file defines. `rewrite.find :r1` lists which definitions of the branch a rule's left side matches.
- **A rewrite can't define anything,** so a helper the right side uses has to be in the file or the branch, and it can't add a case to a handler or a constructor to a match, so those you edit by hand.

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

   The second and third catch partial applications: `List.map (foo 1) xs` and `foo` passed on its own.
4. `rewrite work.u:rules.r1,rules.r2,rules.r3 work.u`. The reply names the new file and says which definitions changed. The rewritten code refers to `foo_v2`, which the new file also defines; the rules are at the bottom, below a fold line that `update` ignores. Read the file, and look at anything you would rather change by hand, such as a caller that should take the new argument itself instead of passing `0`.
5. Strip `_v2` from the new file (a `sed`), `load` it, and `update`. Every use of `foo` is in the file, so it applies cleanly.

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

The `signature` rule changes the signatures that mention `Blah`, the `case` rules the patterns, and the `term` rules the constructions, including the partially applied constructor. Then `rewrite work.u:rules.b1 work.u`, strip `_v2` (which turns `C1_v2` back into `C1`), `load`, `update`. For an ability, the requests are the constructors.

**Walking the dependents.** For a change that rewrites don't fully express, keep a list of definitions whose type you changed, and bring their dependents into the file one round at a time:

1. `edit.to work.u foo`, make the new version `foo_v2`, and put `foo_v2` on the list.
2. Take a definition off the list, `dependents foo` for its old name, and `edit.to work.u 1-`. These are the immediate dependents only, which is the point: a dependent whose type stays the same is where the change stops. `load work.u` and fix the file until it typechecks against the new versions. Any definition whose type you had to change gets the suffix too, `bar_v2`, and goes on the list.
3. Repeat until the list is empty. At any round, when the edits are mechanical, write a rule in the file and `rewrite work.u:rules.r1 work.u` instead of editing by hand, and carry on in the file it writes. Its rules are below the fold, so take them from `work.u` the next time: `rewrite work.u:rules.r2 rewrites/work-<time>-<guid>.u`.
4. Strip `_v2`, `load`, `update`.

Compared with the direct route, this is worth it when rewrites can do much of the work, or when there are many dependents: the direct route has one long stretch where nothing typechecks.

**Where the rules can come from.** `work.u:r1,r2` takes `r1` and `r2` from `work.u`, a file with one definition can be given alone, and `:r1,r2` takes them from the latest typechecked file, or else from the branch. The file is typechecked when the command runs, so its rules can use anything in it. **What to rewrite:** with no second argument, `rewrite` looks at the whole branch (not `lib`) and writes only the definitions that changed; `rewrite :r1 path.to.module` limits it to a namespace, and a `.u` file as the second argument rewrites the definitions in that file, writing all of them. A rule that refers to a definition only the file has is for rewriting that file: in a rewrite of the branch, the result would refer to something the branch doesn't have.
