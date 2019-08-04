Documentation of the Unison codebase repo format. DRAFT, still evolving. We'll freeze this file and remove the DRAFT from the name once we do a release.

```
.unison/v1/
  paths/_head/<namespace-hash>
  paths/<namespace-hash>.ub

  terms/<term-hash>/compiled.ub
  terms/<term-hash>/type.ub

  types/<type-hash>/compiled.ub -- the type declaration

  watches/test/<source-hash>.ub  -- contents have the result of evaluating source-hash
  watches/cache/<source-hash>.ub -- ditto, for other kinds of watches

  dependents/<hash>/<dependent-hash> 
  dependents/_builtin/<builtin-name>/<dependent-hash>

  patches/<hash>.up -- patches, linked from the branches
  
  type-index/<type-hash>/<term-hash>
  type-mentions-index/<type-hash>/<term-hash>
```

`dependents` is updated in the following way:

* When a definition `d` is added, we compute its hash, `h`, and the dependencies of `d`. For each `dep` in the dependencies of `d`, we add `h` to the `dependents` directory, under `dependents/dep/h`. `h` is just an empty file, a link to a definition.
* Note: the dependencies of a term `d` include the dependencies of `d`'s _type_.
* Note: git merge of `dependents` does the right thing.

### A sample repository structure:

```
.unison
└── v1
    ├── dependents
    │   ├── #00tac3luv05e0tqqsef1t5lt7ih8vppek8tqgembt9fsrjapc0rti50n24l76mvq6fsafl8l8a0laoaa91q29bjd08p2dq0e7r69prg
    │   │   └── #et7vgc96hcqhnobco8n36eoitulc0n8ag6n103b6f9hg5bb34seua0gceqvkrg70rlk1cron9uq43scft9pcabpdj1ckotvar0qddt0
    │   ├── #vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0
    │   │   ├── #6rsg4b2n7i5onkm0buh4g6ldd1hqeolsv47mnli9ls90vt0hsqtv3hpe690ae5dqvp52q850e7av1l8tq5elnd2jqjqq4477mgleod0
    │   │   └── #vbh5ul81ck4h1egosbe29s9ehlh46lchlv6lqclnhgvtuu55mk64dnjhh40dh7uvn6ue1b7rsqor4dkk366kan57jm92moqjf6odnlg
    │   └── _builtin
    │       ├── Boolean
    │       │   ├── #15gaimdqqg8rqmj9r5l2l6fon6uktr6abkk7slvufde3um0e8v8o94bkouqtf60ev9rpp94ukfs0kivk0faa5jr7vpbdhrvo6162jf0
    │       │   └── #u8tecclu1h4km9ek31i9u7jiat7epkcqfbqo2e30vedqiok64qoglbo9lr3qsofkvhrlnj9gha3esa96l5oerat32cb9l4f5tp47fng
    │       └── Bytes
    │           ├── #2hvi11d9q25a3disvhuuq6sdm1puekf4dncihb08dkdjtcmncjj1ovgf87fmnr5t21b9dhld2jnvm60r45cvhid339iqhuvgatfk1m0
    │           ├── #fgaevis4bljt5a8f8nv4dckj5ckau43r5dbda6s0oucsa5j8fn3ie5apjouc00pksqpja5vbud0d0joavnu7mcbja1mr56jumfu8d5g
    │           └── #ka3k1bcfmh1lj6b3a351tp9nnjdj714i6m9b82k730t22j4kgqt095mrkc9ksu0sm4f4qk4bscphkehlj8qodneberm468pf5ims14g
    ├── patches
    │   └── 2m0bc94768sbrv3v89kvsdgq9p0kmd7u07dv245akd1i0mobu574kn3t4msc2g1cffjh6ekfar5shvj2ln05r2qcrr61hfmhc9o6kn0.up
    ├── paths
    │   ├── 0c3ad476251ft7dnbdiq870bj4es0vtlev2u5i4jts022mgjjnugod2sdcqli53503i21vrtim35j6pqncm2g9mglghv8aod070e5cg.ub
    │   ├── 9vngjb4h6ag6q0345fmsv67aeik2fut3q9klm6c6s1dkqstuui1ckij5i7eum1mllk01o84dm0obtk79ulbsesef2s5so5mmbjsdbh8.ub
    │   ├── jm2ltsg8hh2b3c3re7aru6e71oepkqlc3skr2v7bqm4h1qgl3srucnmjcl1nb8c9ltdv56dpsgpdur1jhpfs6n5h43kig5bs4vs50co.ub
    │   └── _head
    │       └── jm2ltsg8hh2b3c3re7aru6e71oepkqlc3skr2v7bqm4h1qgl3srucnmjcl1nb8c9ltdv56dpsgpdur1jhpfs6n5h43kig5bs4vs50co
    ├── terms
    │   ├── #00tac3luv05e0tqqsef1t5lt7ih8vppek8tqgembt9fsrjapc0rti50n24l76mvq6fsafl8l8a0laoaa91q29bjd08p2dq0e7r69prg
    │   │   ├── compiled.ub
    │   │   └── type.ub
    │   └── #017bvhctrkag6e5gi0c811oathfjr950kbl7crurbf8fjtf9s5t021nb7rlrq7rf5hquakel1duke7jh4kc10t3sbikrlaj6fl5314g
    │       ├── compiled.ub
    │       └── type.ub
    ├── type-index
    │   ├── #02la9kiu7k4ta5ihc888vvji1g6jpu2l551ns2sihsinrdtv5toldobqvjnh73r1645s3pnp7bq34vmicfpt2irg6653cgjmfu372f8
    │   │   └── #7di5ureqgi60ue42886240kbovfhko0fg85rp2thpkl8af699upsl0os1btk27te1cjdmuerad5oi9bdd04me6mjh2m25djbj236fbo#d0
    │   ├── #ep1qo0ujpuu501tqra77gq1vjmgqjfijig63bjcd4jrf22nuca3hmcu0b49m0bnmd1aurai7rm6e9tvorutd5geg9kehnaufe0dr5hg
    │   │   ├── #ep1qo0ujpuu501tqra77gq1vjmgqjfijig63bjcd4jrf22nuca3hmcu0b49m0bnmd1aurai7rm6e9tvorutd5geg9kehnaufe0dr5hg#d0
    │   │   ├── #ep1qo0ujpuu501tqra77gq1vjmgqjfijig63bjcd4jrf22nuca3hmcu0b49m0bnmd1aurai7rm6e9tvorutd5geg9kehnaufe0dr5hg#d1
    │   │   └── #ep1qo0ujpuu501tqra77gq1vjmgqjfijig63bjcd4jrf22nuca3hmcu0b49m0bnmd1aurai7rm6e9tvorutd5geg9kehnaufe0dr5hg#d2
    │   └── _builtin
    │       ├── Int
    │       │   ├── #d75vubeoep5o8ph72v0v9qdm36n17up0d7bsbdckjapcs7k9g1kv5mnbpp3444u8fmvo2h3benmk7o3sd09g1lkrrvk4q93vv8u2n3g
    │       │   └── #p9og3s2h41natoslfjoi1do0omp82s4jiethebfd4j5p99ltbdmcua2egbiehs9tq9k65744cvugibiqdkgip21t7se4e8faktnl3k0
    │       └── Nat
    │           ├── #447fer8m13n7stndnfrkdqehdb6tome5csg0jempo8bqvah37qgdupkvv34eb22qr71ouvjj3pfokpn0ekq9krv5h21e1jiligg5880
    │           └── #s9h25aadei68iscfiu60eldfhe9uvh0pk3knd9m965gqlejvc5jlcqs9gfcgpgvfv85n2pefvee4ca2n7mepcoqamou73g7ilscf450
    ├── type-mentions-index
    │   ├── #7oj73vs24tjp0tu00jl3hp14jgershfll5vovtbte4ft05fdcg1rpki3hpjacqojlge2ltd9hh7dhi9sqggvd4t7j5amr212lvmmrv0
    │   │   └── #21fc70s2sun00ledska1gbkr8a6e0uf9ul1on8iogi5tai71tftsagqbkpug2q0k8re5tme45l8gkkdlhnceaffjmg7t8pps6tb6ckg
    │   ├── #9giu4jv4864091hr03v5ji9ffh3i1qdg26v5n442qkjrek9gl2lt7asqphtq3gc43h5m9a0ehiar428rgeovvu1habrc8mnqapul1lo
    │   │   ├── #9giu4jv4864091hr03v5ji9ffh3i1qdg26v5n442qkjrek9gl2lt7asqphtq3gc43h5m9a0ehiar428rgeovvu1habrc8mnqapul1lo#d0
    │   │   └── #sb97alikfa50nqkcd8l5qtoi28i0v4l50cmf342bsclq3hjjktgr4h61cg7l9ethpm1eeg82ba5r7b839rect0dqpc1u1o43b0bi2d0
    │   └── _builtin
    │       ├── Boolean
    │       │   ├── #nsjfm3466etncobbk421e78v22nanactun28pg5qvntq02fdv1ot0src0dtm7dger6s4q1lccihgr08vh6pfq7cr3ii9dp2r8936qi0
    │       │   └── #u8tecclu1h4km9ek31i9u7jiat7epkcqfbqo2e30vedqiok64qoglbo9lr3qsofkvhrlnj9gha3esa96l5oerat32cb9l4f5tp47fng
    │       └── Nat
    │           └── #v6q3nv802g3lfosq1lhn1976udu03kvu4i97ml4upbtu7d8dd6f3u7rhejgpjpgnbjlgrhfb483suoarv5qu3dmh1u7gk3imoaqcdmg
    ├── types
    │   ├── #3n6ctj9fne0fjmf2mhem6bschovul0hic2uoumblsp47rfrrhn6beoi613r373bbt7nfgtod93u5i5fjnvutqh3gspi9lc4ral1nkgo
    │   │   └── compiled.ub
    │   └── #vmc06s4f236sps61vqv35g7ridnae03uetth98aocort1825stbv7m6ncfca2j0gcane47c8db2rjtd2o6kch2lr7v2gst895pcs0m0
    │       └── compiled.ub
    └── watches
        ├── _cache
        └── test
```
