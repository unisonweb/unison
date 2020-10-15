Question: should the dependents / dependency index be a Relation Reference Reference (like now) a Relation Referent Reference?

Example, if you have `type Foo = Blah | Blah2 Nat`,



Advantages:

* If patches can replace constructors (not just types or terms), then having the index keyed by `Referent` lets you efficiently target the definitions that use those constructors.
* Also lets you find things that depend on `Blah2` (rather than depending on `Foo`).
