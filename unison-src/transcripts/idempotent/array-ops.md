``` ucm :hide
> builtins.mergeio
```

This transcript tests the bulk array builtins against their original
specs. The examples in the spec are usually very nice cases, so this is
not the most thorough test of their behavior. It is, rather, more of a
smoke test to make sure they aren't completely broken.

``` unison
run! : '{Exception} a -> a
run! th = handle !th with cases
  { a } -> a
  { raise (Failure _ msg _) -> _ } -> bug msg

psort : ImmutableArray a -> ImmutableArray a
psort a = run! do pick (sortIx a) a

> at1s (fromList [(1, "a"), (2, "b"), (3, "c")])
> at1s (fromList [("x", true), ("y", false)])

> at2s (fromList [(1, 2, 3), (4, 5, 6)])
> at2s (fromList [("a", "b", "c"), ("d", "e", "f")])

> run! do
    chop (fromNatList [0, 3, 5])
         (fromNatList [2, 2, 3])
         (fromList [1,2,3,4,5,6,7,8])

> run! do
    chop (fromNatList [0, 4])
         (fromNatList [4, 2])
         (fromList ["a", "b", "c", "d", "e", "f"])

> fromListAt1 [(1, "a"), (2, "b"), (3, "c")]
> fromListAt1 [(true, 10), (false, 20)]

> fromListAt2 [(1, 2, 3), (4, 5, 6)]
> fromListAt2 [("x", "y", "z"), ("a", "b", "c")]

> ImmutableArray.fromList [1, 2, 3, 4]
> ImmutableArray.fromList ["a", "b", "c"]

> intersectIx (fromList [1, 2, 3]) (fromList [2, 3, 4])
> intersectIx (fromList ["a", "b"]) (fromList ["b", "c"])

> murmurHashesUntyped (fromList [1, 2, 3])
> fromNatList (List.map murmurHashUntyped [1, 2, 3])

> outerJoinIx (fromList [1, 2, 3]) (fromList [2, 3, 4])
> outerJoinIx (fromList ["a", "b"]) (fromList ["b", "c"])

> run! do ImmutableArray.pick (fromNatList [0, 2, 1]) (fromList ["a", "b", "c"])
> run! do ImmutableArray.pick (fromNatList [3, 0, 3]) (fromList [10, 20, 30, 40])

> run! do ImmutableArray.pick1 (fromNatList [1,0,3,2]) (fromList ["a", "b", "c"])
> run! do ImmutableArray.pick1 (fromNatList [2,1,0,2]) (fromList [10, 20, 30])

> run! do ImmutableArray.pick1Or "X" (fromNatList [1,0,2,0]) (fromList ["a", "b", "c"])
> run! do ImmutableArray.pick1Or 99 (fromNatList [0,3,0,1]) (fromList [10, 20, 30])

> runsIx (fromList [1,1,1,2,2,3,1,1])
> runsIx (fromList ["a","a","b","b","b","c"])

> sortIx (fromList [3,1,2])
> sortIx (fromList ["c", "a", "b")

> psort (fromList [3,1,2])

> psort (fromList ["c","a","b"])

> toLists (fromList [fromList [1,2], fromList [3,4,5], fromList [6]])
> toLists (fromList [fromList ["a"], fromList ["b", "c"])

> ImmutableArray.toList (fromList [1,2,3,4])
> ImmutableArray.toList (fromList ["x", "y", "z"])

> zipWithAppend (fromList [[1,2], [3]]) (fromList [[4], [5,6]])
> zipWithAppend (fromList [["a"], ["b"]]) (fromList [["c", "d"], ["e"]])

> fromNatList [0,1,2,3]
> fromNatList [100, 200, 300]

> modR (fromNatList [10, 11, 12, 13]) 3
> modR (fromNatList [5, 10, 15, 20]) 7

> multiplyR (fromNatList [1, 2, 3, 4]) 5
> multiplyR (fromNatList [7, 8, 9]) 3

> divideR (fromNatList [10, 15, 20, 25]) 5
> divideR (fromNatList [21, 24, 27]) 3

> occurrences (fromNatList [0, 2, 1, 2, 0])
> occurrences (fromNatList [3, 0, 3, 3])

> run! do pick (fromNatList [0, 2, 1]) (fromNatList [10, 20, 30])
> run! do pick1 (fromNatList [1,0,2,1]) (fromNatList [10,20,30])
> run! do pick1Or 99 (fromNatList [1,0,2,0]) (fromNatList [10,20,30])

> size (fromNatList [1,2,3,4,5])
> size (fromNatList [])

> toList (fromNatList [1,2,3])
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + psort : ImmutableArray a -> ImmutableArray a
  + run!  : '{Exception} a -> a

  Run `update` to apply these changes to your codebase.

    9 | > at1s (fromList [(1, "a"), (2, "b"), (3, "c")])
          ⧩
          ImmutableArray.fromList [1, 2, 3]

    10 | > at1s (fromList [("x", true), ("y", false)])
           ⧩
           ImmutableArray.fromList ["x", "y"]

    12 | > at2s (fromList [(1, 2, 3), (4, 5, 6)])
           ⧩
           ImmutableArray.fromList [2, 5]

    13 | > at2s (fromList [("a", "b", "c"), ("d", "e", "f")])
           ⧩
           ImmutableArray.fromList ["b", "e"]

    15 | > run! do
           ⧩
           ImmutableArray.fromList
             [ ImmutableArray.fromList [1, 2]
             , ImmutableArray.fromList [4, 5]
             , ImmutableArray.fromList [6, 7, 8]
             ]

    20 | > run! do
           ⧩
           ImmutableArray.fromList
             [ ImmutableArray.fromList ["a", "b", "c", "d"]
             , ImmutableArray.fromList ["e", "f"]
             ]

    25 | > fromListAt1 [(1, "a"), (2, "b"), (3, "c")]
           ⧩
           ImmutableArray.fromList [1, 2, 3]

    26 | > fromListAt1 [(true, 10), (false, 20)]
           ⧩
           ImmutableArray.fromList [true, false]

    28 | > fromListAt2 [(1, 2, 3), (4, 5, 6)]
           ⧩
           ImmutableArray.fromList [2, 5]

    29 | > fromListAt2 [("x", "y", "z"), ("a", "b", "c")]
           ⧩
           ImmutableArray.fromList ["y", "b"]

    31 | > ImmutableArray.fromList [1, 2, 3, 4]
           ⧩
           ImmutableArray.fromList [1, 2, 3, 4]

    32 | > ImmutableArray.fromList ["a", "b", "c"]
           ⧩
           ImmutableArray.fromList ["a", "b", "c"]

    34 | > intersectIx (fromList [1, 2, 3]) (fromList [2, 3, 4])
           ⧩
           (fromNatList [1, 2], fromNatList [0, 1])

    35 | > intersectIx (fromList ["a", "b"]) (fromList ["b", "c"])
           ⧩
           (fromNatList [1], fromNatList [0])

    37 | > murmurHashesUntyped (fromList [1, 2, 3])
           ⧩
           fromNatList
             [ 12377779861971637790
             , 2071518504945439519
             , 1857633277936041700
             ]

    38 | > fromNatList (List.map murmurHashUntyped [1, 2, 3])
           ⧩
           fromNatList
             [ 12377779861971637790
             , 2071518504945439519
             , 1857633277936041700
             ]

    40 | > outerJoinIx (fromList [1, 2, 3]) (fromList [2, 3, 4])
           ⧩
           (fromNatList [1, 2, 3, 0], fromNatList [0, 1, 2, 3])

    41 | > outerJoinIx (fromList ["a", "b"]) (fromList ["b", "c"])
           ⧩
           (fromNatList [1, 2, 0], fromNatList [0, 1, 2])

    43 | > run! do ImmutableArray.pick (fromNatList [0, 2, 1]) (fromList ["a", "b", "c"])
           ⧩
           ImmutableArray.fromList ["a", "c", "b"]

    44 | > run! do ImmutableArray.pick (fromNatList [3, 0, 3]) (fromList [10, 20, 30, 40])
           ⧩
           ImmutableArray.fromList [40, 10, 40]

    46 | > run! do ImmutableArray.pick1 (fromNatList [1,0,3,2]) (fromList ["a", "b", "c"])
           ⧩
           ImmutableArray.fromList ["a", "c", "b"]

    47 | > run! do ImmutableArray.pick1 (fromNatList [2,1,0,2]) (fromList [10, 20, 30])
           ⧩
           ImmutableArray.fromList [20, 10, 20]

    49 | > run! do ImmutableArray.pick1Or "X" (fromNatList [1,0,2,0]) (fromList ["a", "b", "c"])
           ⧩
           ImmutableArray.fromList ["a", "X", "b", "X"]

    50 | > run! do ImmutableArray.pick1Or 99 (fromNatList [0,3,0,1]) (fromList [10, 20, 30])
           ⧩
           ImmutableArray.fromList [99, 30, 99, 10]

    52 | > runsIx (fromList [1,1,1,2,2,3,1,1])
           ⧩
           (fromNatList [0, 3, 5, 6], fromNatList [3, 2, 1, 2])

    53 | > runsIx (fromList ["a","a","b","b","b","c"])
           ⧩
           (fromNatList [0, 2, 5], fromNatList [2, 3, 1])

    55 | > sortIx (fromList [3,1,2])
           ⧩
           fromNatList [1, 2, 0]

    56 | > sortIx (fromList ["c", "a", "b")
           ⧩
           fromNatList [1, 2, 0]

    58 | > psort (fromList [3,1,2])
           ⧩
           ImmutableArray.fromList [1, 2, 3]

    60 | > psort (fromList ["c","a","b"])
           ⧩
           ImmutableArray.fromList ["a", "b", "c"]

    62 | > toLists (fromList [fromList [1,2], fromList [3,4,5], fromList [6]])
           ⧩
           ImmutableArray.fromList [[1, 2], [3, 4, 5], [6]]

    63 | > toLists (fromList [fromList ["a"], fromList ["b", "c"])
           ⧩
           ImmutableArray.fromList [["a"], ["b", "c"]]

    65 | > ImmutableArray.toList (fromList [1,2,3,4])
           ⧩
           [1, 2, 3, 4]

    66 | > ImmutableArray.toList (fromList ["x", "y", "z"])
           ⧩
           ["x", "y", "z"]

    68 | > zipWithAppend (fromList [[1,2], [3]]) (fromList [[4], [5,6]])
           ⧩
           ImmutableArray.fromList [[1, 2, 4], [3, 5, 6]]

    69 | > zipWithAppend (fromList [["a"], ["b"]]) (fromList [["c", "d"], ["e"]])
           ⧩
           ImmutableArray.fromList [["a", "c", "d"], ["b", "e"]]

    71 | > fromNatList [0,1,2,3]
           ⧩
           fromNatList [0, 1, 2, 3]

    72 | > fromNatList [100, 200, 300]
           ⧩
           fromNatList [100, 200, 300]

    74 | > modR (fromNatList [10, 11, 12, 13]) 3
           ⧩
           fromNatList [1, 2, 0, 1]

    75 | > modR (fromNatList [5, 10, 15, 20]) 7
           ⧩
           fromNatList [5, 3, 1, 6]

    77 | > multiplyR (fromNatList [1, 2, 3, 4]) 5
           ⧩
           fromNatList [5, 10, 15, 20]

    78 | > multiplyR (fromNatList [7, 8, 9]) 3
           ⧩
           fromNatList [21, 24, 27]

    80 | > divideR (fromNatList [10, 15, 20, 25]) 5
           ⧩
           fromNatList [2, 3, 4, 5]

    81 | > divideR (fromNatList [21, 24, 27]) 3
           ⧩
           fromNatList [7, 8, 9]

    83 | > occurrences (fromNatList [0, 2, 1, 2, 0])
           ⧩
           ImmutableArray.fromList
             [ fromNatList [0, 4]
             , fromNatList [2]
             , fromNatList [1, 3]
             ]

    84 | > occurrences (fromNatList [3, 0, 3, 3])
           ⧩
           ImmutableArray.fromList
             [ fromNatList [1]
             , fromNatList []
             , fromNatList []
             , fromNatList [0, 2, 3]
             ]

    86 | > run! do pick (fromNatList [0, 2, 1]) (fromNatList [10, 20, 30])
           ⧩
           fromNatList [10, 30, 20]

    87 | > run! do pick1 (fromNatList [1,0,2,1]) (fromNatList [10,20,30])
           ⧩
           fromNatList [10, 20, 10]

    88 | > run! do pick1Or 99 (fromNatList [1,0,2,0]) (fromNatList [10,20,30])
           ⧩
           fromNatList [10, 99, 20, 99]

    90 | > size (fromNatList [1,2,3,4,5])
           ⧩
           5

    91 | > size (fromNatList [])
           ⧩
           0

    93 | > toList (fromNatList [1,2,3])
           ⧩
           [1, 2, 3]
```
