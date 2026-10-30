``` unison
Person.birthday : { age: Nat | r } -> { age: Nat | r }
Person.birthday p = p@age.set (p@age + 1)

Person.oldest : [{ age: Nat | r }] -> Optional { age: Nat | r }
Person.oldest = List.maximumOn (a b -> Universal.ordering b a) (@age)

test> Person.birthday.tests.example = test.verify do
  ensureEqual (Person.birthday { name: "Ann", age: 30 }) { name: "Ann", age: 31 }
  ensureEqual (Person.oldest []) None
```
``` ucm
scratch/main> add
scratch/main> test
```
