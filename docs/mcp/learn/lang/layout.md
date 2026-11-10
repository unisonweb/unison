Where a line may begin, and why code that looks fine fails to parse. Unison has no semicolons or braces around blocks: indentation says where a block starts and ends.

- A **block** starts after one of these: `=`, `do`, `then`, `else`, `with`, `where`, `cases`, `let`, `->` in a case of a `match` or `cases`, `handle ... with`. Its column is the column of the first word after the opener, which can be on the same line or on the next.
- Every line of the block that starts at that column is a new item, a new statement. A line indented further continues the previous item. A line indented less ends the block.
- So after `x = foo` on one line, a continuation line must be indented more than `foo`'s column, not just more than `x`. Put the body on the next line, indented, and it is simple: `x =` then an indented block.
- Brackets, parentheses and braces open a block too, but a line inside them that is indented more than the surrounding block is fine, including one that starts with `,` or `]`.
- The last line of a block is its value. A block with only definitions and no final expression is an error.
- A case of a `match` has its body after `->`. Several statements go on the following lines, indented, all at the same column. There is no `let` needed to name a value in the middle of a block: write `name = expression` on a line of its own.
- `use Nat +` on a line of its own (at the start of a block) lets you write `+` for `Nat.+` in that block (see [operators](learn:lang/operators)).

## Detail

**A body on the next line.**

````
total : [Nat] -> Nat
total xs =
  use Nat +
  go acc = cases
    [] -> acc
    h +: t -> go (acc + h) t
  go 0 xs
````

`use Nat +`, `go ...` and `go 0 xs` are three items of the block started by `total xs =`, at the column of `use`. The `cases` block of `go` starts at `[]`, indented further, and its two cases are lines at the same column.

**Several statements in a case.**

````
describe = cases
  Some n ->
    doubled = n * 2
    "got " ++ Nat.toText doubled
  None -> "nothing"
````

**Dedent ends a block.** If `None` were indented as far as `doubled`, it would be read as a third statement of the `Some` case.

**An argument on the next line.** The first word after `=` sets the block's column, and a continuation line must start to the right of it. This fails, because `[` is to the left of `List.size`:

````
count = List.size
  [ 1
  , 2
  ]
````

(`I was surprised to find a [ here. I was expecting one of these instead: newline or semicolon`.) Two ways to fix it: start the body on the next line, as below, or indent `[` past `List.size`.

````
count =
  List.size
    [ 1
    , 2
    ]
````

Inside brackets the lines can be anywhere to the right of the block's column, and a line starting with `,` or `]` is fine.

**Common failures.** A continuation line that isn't to the right of the first word after `=`, a statement in a case body that doesn't line up with the others, and a last line that is an assignment instead of a value.
