``` unison
type Person = { name : Text, age : Nat }

Person.birthday : Person -> Person
Person.birthday = Person.age.modify Nat.increment

Person.oldest : [Person] -> Optional Person
Person.oldest = List.maximumOn (a b -> Universal.ordering b a) Person.age

test> Person.birthday.tests.example = test.verify do
  ensureEqual (Person.birthday (Person "Ann" 30)) (Person "Ann" 31)
  ensureEqual (Person.oldest []) None
```
``` ucm
scratch/main> add
scratch/main> test
```
