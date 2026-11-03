``` unison
type Person = Person { name: Text, age: Nat }

Person.birthday : Person -> Person
Person.birthday = cases Person r -> Person r@{age: @age + 1}

Person.oldest : [Person] -> Optional Person
Person.oldest = List.maximumOn Universal.ordering (cases Person { age: a } -> a)

test> Person.birthday.tests.example = test.verify do
  ensureEqual (Person.birthday (Person { name: "Ann", age: 30 })) (Person { name: "Ann", age: 31 })
  ensureEqual (Person.oldest [Person { name: "Ann", age: 30 }, Person { name: "Bob", age: 41 }, Person { name: "Cy", age: 25 }]) (Some (Person { name: "Bob", age: 41 }))
  ensureEqual (Person.oldest []) None
```
``` ucm
scratch/main> add
scratch/main> test
```
