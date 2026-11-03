You are judging how `Person` is declared in the Unison codebase you are connected to. A previous agent wrote it. You may use any tool except `learn`. Don't change the project's definitions.

Run `view Person` (and `view Person.birthday` and `view Person.oldest`), then rate:

- **GREAT**: a type `Person` with a single constructor whose argument is an anonymous record: `type Person = Person { name: Text, age: Nat }` (the new record syntax). `birthday` and `oldest` take and return `Person`.
- **PASS**: a `Person` type that works but is declared another way: a positional constructor (`type Person = Person Text Nat`), or the older declared-record syntax `type Person = { name : Text, age : Nat }`.
- **FAIL**: there is no `Person` type (for example the functions work over bare anonymous records), or the type doesn't hold both a name and an age.

Reply with a short paragraph of reasons, then a final line that is exactly GREAT, PASS or FAIL.
