Defining named values, with optional type signatures, and the literals and tuples you can write.

- A term is a name and a value. The type signature `name : Type` goes on the line above the definition `name = value`, and can be left out because Unison infers it. Write one for top-level definitions anyway.
- By convention, term names start with a lowercase letter (camelCase). Type names start with an uppercase letter. Names are organized in namespaces separated by dots, as in `Nat.toText`. A [branch](learn:ucm/branches) is a versioned namespace of Unison definitions.
- Values never change. Defining the same name twice in the same scope is an error.
- Comments are `-- until the end of the line` and `{- a block -}`. Or use `note "a comment"` (on its own line) if you'd like the comment to be persisted to the codebase.
- Literals: `"text"` (`"""` for several lines), `?a` is a `Char`, `5` is a `Nat` (zero or more), `+4` and `-9` are `Int`s (note the `+`), `3.14` is a `Float`, `true` and `false` are `Boolean`s, and `0xs0102` is `Bytes`.
- A tuple groups values of different types: `("Alice", 5) : (Text, Nat)`. Take one apart inside a block, not at the top level: `(name, count) = tuple`.

## Detail

**Arithmetic uses the type's name:** `Nat.+`, `Int.-`, `Text.++`. Written infix, `1 Nat.+ 2`. `use Nat +` at the top of a block lets you write just `+` in it (see [operators](learn:lang/operators)). `++` joins `Text`, lists and `Bytes`.

**Annotating an expression.** To give an expression a type inline, put the whole thing in parentheses: `x = (Map.empty : Map Text Nat)` or `([] : [Nat])`. Without the parentheses, the type swallows what comes after it, and you get a parse error. Usually a signature line above the definition is clearer.

**Documentation.** A doc comment `{{ ... }}` before a definition attaches documentation to it, which `docs <name>` shows (see [documentation](learn:lang/documentation)).

**Names.** Dotted names such as `Optional.map` are just names in the namespace `Optional`. `List.map` can be written by that name, or as `map` after `use List map`.
