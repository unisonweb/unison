You are judging `Person.birthday` and `Person.oldest` in the Unison codebase you are connected to. A previous agent wrote them, with tests. You may use any tool except `learn`. Don't change the project's definitions; use scratch code only for your own checks.

A person is a record with a `name : Text` and an `age : Nat`. Records are written `{ name: "Ann", age: 30 }`, and `r@age` reads a field.

Run each of the following, and note the result of each:

1. `view Person.birthday` and `view Person.oldest`: `Person.birthday` takes a person and returns a person; `Person.oldest` takes a list of persons and returns an `Optional` person.
2. `Person.birthday { name: "Ann", age: 30 }` is `{ name: "Ann", age: 31 }`: the age goes up by one and the name is unchanged.
3. `Person.oldest [{ name: "Ann", age: 30 }, { name: "Bob", age: 41 }, { name: "Cy", age: 25 }]` is `Some { name: "Bob", age: 41 }`.
4. `Person.oldest []` is `None`.
5. Informational only (it does not affect pass or fail): note whether `Person.birthday` and `Person.oldest` are typed over records with an `age` field of any other fields (`{ age: Nat | r }`), and whether the agent declared a `type Person = { ... }` (the older declared-record syntax) instead of using records directly.

They pass only if checks 1 to 4 pass.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
