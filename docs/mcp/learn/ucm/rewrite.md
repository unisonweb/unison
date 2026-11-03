Finding code by its shape and changing it with `rewrite.find` and `rewrite`: every call of a function in a given form, every match on a constructor, every handler of an ability, rewritten in one command instead of by hand.

- **A rule** is a definition whose value is a `@rewrite` block. `rules.r x y = @rewrite term foo x y ==> bar y x` says that any expression of the form `foo x y`, for any expressions `x` and `y` (the rule's parameters stand for anything), becomes `bar y x`. The kinds of rule, each on its own line in the block: `term a ==> b` for an expression, `case P ==> Q` for a pattern in a `match` or `cases` (including the `{ Ability.op args -> resume }` patterns of a handler), and `signature T ==> U` for a type in a signature.
- **To find where a constructor is matched or an ability's operation is handled,** write a `case` rule that names it without its arguments and `rewrite.find` it: `matchesOnWord = @rewrite case Token.Word ==> Token.Word` in a `unison` block, then `rewrite.find :matchesOnWord` lists every definition with a pattern on `Word`, with any arguments, wildcards included; `handlersOfCount = @rewrite case Metrics.count ==> Metrics.count` lists the handlers of `Metrics.count`. The right side is ignored by `rewrite.find`. To list every use of a definition, `dependents foo` is the command (see [find](learn:ucm/find)); `rewrite.find` is for a shape: a particular form of application, `term foo x 0 ==> ()` for the calls whose second argument is `0`, the patterns on a constructor, the handlers of an operation.
- **A function whose match ends in a wildcard** has no pattern on the constructor you are adding, so `rewrite.find` doesn't list it, and it still typechecks when the constructor is added. Read those (`dependents Token` lists everything that uses the type) and decide whether the wildcard answers correctly for the new case.
- **`rewrite <rules> <file>` rewrites a file in place.** `work.u:rules.r1,rules.r2` takes the rules from the file `work.u`, which is typechecked for the occasion, so its rules can refer to anything the file defines; `rewrite work.u:rules.r1,rules.r2 work.u` then rewrites `work.u` itself, and saves the version before as `rewrites/<time>-work-<guid>.u` beside it (the reply names it). The file keeps its name through a refactoring: `load work.u` to typecheck the result, and [`update`](learn:ucm/update) when you are done. The rules stay in the file as ordinary definitions, so keep them in a namespace of their own, `rules.r1`, and delete them from the file before the final `update` (or `delete.namespace rules` after it). Nothing changes in the branch until you `update`.
- **With no target,** `rewrite :r1` rewrites the branch (not `lib`), taking `r1` from the latest typechecked file or else the branch, and writes only the definitions that changed to a new file, `rewrites/<time>-<guid>.u`; a namespace target, `rewrite :r1 app.model`, limits it. This form is for a rewrite that needs nothing new, such as replacing one existing function with another everywhere. A refactoring that introduces a definition goes through a file, as above, so that the new definition, the uses and the rules are together.
- **A rewrite can't define anything** and can't add a case to a `match` or a handler. Edit those by hand, in the file `rewrite` wrote.
- **A definition of the codebase that the file uses, and that uses what the file redefines,** is added to the file by `load`, above the fold, under a comment saying why (the reply says `Added to the file, from the codebase: ...`), and saved rebuilt by `update`, so a work file needs only the definitions you change.
- **Whenever you have a file of definitions to fix**, including the file of an action that [can't finish](learn:ucm/temporary-branch), consider a rewrite of it before editing by hand: `rewrite work.u:rules.r1 update-main-90f1b725.u`. The procedures for the common refactorings are in [refactoring](learn:skills/refactor).

## Detail

**Rules, in full.** Parameters are bound by the rule's definition: in `rules.swap x y = @rewrite term f x y ==> f y x`, `x` and `y` match any expression, and a name that isn't a parameter, like `f`, must be a definition (of the file or the branch), matched by what it refers to, not by its spelling: a call of `f` under another name is found too. A `term` rule matches anywhere in an expression, including inside lambdas and `match` branches. A `case` rule matches a pattern, so `case Token.Word w ==> Token.Word2 w 0` rewrites the patterns and `term Token.Word w ==> Token.Word2 w 0` the constructions. A `signature` rule, `signature Token ==> Token2`, rewrites the type wherever it appears in a signature, including in a type with parameters such as `[Token]` or `Token -> Nat`. Several rules in one block apply together, and `rewrite` takes several blocks: `rewrite work.u:rules.a,rules.b`.

**Partial applications.** `term foo x y ==> bar x y 0` doesn't match `List.map (foo 1) xs` or a bare `foo` passed to another function. Add rules for those: `term foo x ==> (y -> bar x y 0)` and `term foo ==> (x y -> bar x y 0)`.

**An example.** `Price.format : Nat -> Text` should take a currency symbol first, and every present use should pass `"$"`:

````
``` unison
Price.formatWith : Text -> Nat -> Text
Price.formatWith symbol amount = ...

addSymbol = @rewrite term Price.format ==> Price.formatWith "$"
```

``` ucm
> rewrite.find :addSymbol
> rewrite :addSymbol
```
````

`rewrite.find` numbers its list, so `edit.to work.u 1-3` brings those definitions into a file if you would rather edit them by hand. This `rewrite`, with no target, writes the rewritten definitions to `rewrites/<time>-<guid>.u`; they refer to `Price.formatWith`, which only the block defined, so it would have to be added to that file too. The [refactoring](learn:skills/refactor) topic has the better version of this, with a work file, where the new definition, the uses and the rules live together and the file is rewritten in place.

**Reading the result.** The rewritten file starts with a comment naming the rules and the definitions that changed. It is a real file (see [the scratch file](learn:ucm/scratch-file)): read it with your file tools, and `load` it to typecheck it. Don't retype its contents from memory. A rewrite can be repeated on the same file with other rules, since the rules are still in it.
