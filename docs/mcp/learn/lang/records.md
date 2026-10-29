Two ways to have named fields: anonymous records, written `{ name: "Bob", age: 99 }`, and record types you declare.

- A record value is `{ name: "Bob", age: 99 }`, and its type is `{ name: Text, age: Nat }`. Fields are ordered by name, not by how you wrote them.
- `r@age` reads a field, and chains: `r@a@b`. `(@age)` is the getter as a function.
- `r@age.set 100` returns a copy with the field changed. `r@{age: @age + 1, name: "Rob"}` changes several: inside the braces `@age` is the field's value before the change.
- Patterns take fields by name, in any order, and may leave some out: `{ name: n } -> n`. `{ x: x, y: y } = r` destructures in a block.
- A function that takes `{ x: Nat }` accepts a record with more fields. Name the leftover fields with `{ x: Nat | r }` to return them too.
- A declared record type: `type Volunteer = { preferredName : Text, age : Nat }`. It makes the constructor `Volunteer "Ann" 40`, and the functions `Volunteer.age`, `Volunteer.age.set 500 v` and `Volunteer.age.modify f v`.
- A constructor can take a record: `type Point = Point { x: Nat, y: Nat }`, built as `Point { x: 1, y: 2 }`.

## Detail

**Rows.** `{ x: Nat | r }` means "a record with the field `x`, and other fields called `r`". A function that returns its argument with the same extra fields needs it: `pass : { x: Nat | r } -> { x: Nat | r }`. Without `r`, `{ x: Nat } -> { x: Nat }` returns exactly `{ x: Nat }`, and a record with extra fields is an error there. A parameter alone, `{ x: Nat } -> Nat`, accepts extra fields.

**Constructor fields are exact.** A record passed to a constructor must have exactly the declared fields: `Point { x: 1 }` and `Point { x: 1, y: 2, z: 3 }` are errors.

**Nested records.** `r@a@b@{x: @x + 1}` changes a field of a nested record.

**Spacing.** `r@x`, `r @ x` and `r@ x` are the same getter. `r @x`, with a space before and none after, is not.

**Comparing.** Two records can be compared only if they have the same fields.
