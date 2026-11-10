Two ways to have named fields: anonymous records, written `{ name: "Bob", age: 99 }`, and record types you declare.

- A record value is `{ name: "Bob", age: 99 }`, with `:`, not `=`, and its type is `{ name: Text, age: Nat }`. Fields are ordered by name, not by how you wrote them.
- `r@age` reads a field, and chains: `r@a@b`. `(@age)` is the getter as a function.
- `r@age.set 100` returns a copy with the field changed. `r@{age: @age + 1, name: "Rob"}` changes several: inside the braces `@age` is the field's value before the change.
- Patterns take fields by name, in any order, and may leave some out: `{ name: n } -> n`. `{ x: x, y: y } = r` destructures in a block.
- A function that takes `{ x: Nat }` accepts a record with more fields. Name the leftover fields with `{ x: Nat | r }` to return them too.
- **To declare a record type, give a type a constructor that takes a record:** `type Pet = Pet { name: Text, weight: Nat }`. It is built as `Pet { name: "Tom", weight: 4 }`. Take it apart by matching on the constructor: `cases Pet r -> r@weight`, or `cases Pet { weight: w } -> w`. Change a field with the block syntax on the record inside: `Pet.feed = cases Pet r -> Pet r@{weight: @weight + 1}`.
- The older form `type Volunteer = { preferredName : Text, age : Nat }` still works but is deprecated: it generates constructor and accessor functions (`Volunteer "Ann" 40`, `Volunteer.age`). Don't use it in new code. The form above generates no accessors: there is no `Pet.weight`, so a function of the field is `cases Pet r -> r@weight`.
## Detail

**Rows.** `{ x: Nat | r }` means "a record with the field `x`, and other fields called `r`". A function that returns its argument with the same extra fields needs it: `pass : { x: Nat | r } -> { x: Nat | r }`. Without `r`, `{ x: Nat } -> { x: Nat }` returns exactly `{ x: Nat }`, and a record with extra fields is an error there. A parameter alone, `{ x: Nat } -> Nat`, accepts extra fields.

**Constructor fields are exact.** A record passed to a constructor must have exactly the declared fields: `Point { x: 1 }` and `Point { x: 1, y: 2, z: 3 }` are errors.

**Nested records.** `r@a@b@{x: @x + 1}` changes a field of a nested record.

**Spacing.** `r@x`, `r @ x` and `r@ x` are the same getter. `r @x`, with a space before and none after, is not.

**Comparing.** Two records can be compared only if they have the same fields.
