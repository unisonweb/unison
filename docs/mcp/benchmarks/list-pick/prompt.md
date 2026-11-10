In the current Unison project, I need `List.pick`, which takes a list of Booleans and a list of values, and returns the values at the positions where the Boolean is true: the elements themselves, not their indices. For example, `List.pick [true, false, true] ["a", "b", "c"]` is `["a", "c"]`, and `List.pick [false, true, true] [10, 20, 30]` is `[20, 30]`.

Its type is `[Boolean] -> [a] -> [a]`. It should work on lists of any length. Make sure it is documented and tested, and added to the codebase.
