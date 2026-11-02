Declaring your own types with `type`.

- `type Color = RGB Nat Nat Nat | Named Text` declares a type with two constructors. A constructor is a function: `RGB 1 2 3 : Color`. A constructor with no fields just tags a value: `type Genre = Fiction | Poetry`.
- By convention, constructors are in the type's namespace: `Genre.Poetry`.
- A type can take parameters, which follow its name: `type Box a = Box a`.
- Types are **unique** by default: a type is identified by its name, so two unique types with the same shape are different types. `structural type Weekday = Mon | Tues` is identified by its shape instead, and any two with the same shape are the same type. Use `structural` only when the type means nothing beyond its shape.
- A type with a single constructor with named fields is a [record](learn:lang/records).
- A type whose constructors each give their own result type, `type Expr a where ...`, is a [GADT](learn:lang/gadts).
- Take values apart with [patterns](learn:lang/cases).
- To add a constructor to a type that exists, or change one's fields, see [refactoring](learn:skills/refactor): `rewrite.find` lists every function that matches on the type, and `rewrite` changes the patterns and constructions.
