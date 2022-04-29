# projects api

```unison
rachel.filesystem.x = 42
ross.http.y = 43
joey.json.z = 44
joey.yaml.zz = 45
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      joey.json.z         : ##Nat
      joey.yaml.zz        : ##Nat
      rachel.filesystem.x : ##Nat
      ross.http.y         : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    joey.json.z         : ##Nat
    joey.yaml.zz        : ##Nat
    rachel.filesystem.x : ##Nat
    ross.http.y         : ##Nat

```
```api
GET /api/projects
[
    {
        "owner": "joey",
        "name": "json",
        "hash": "#vjmnhfbas8pejgpgsh26255ebaolepuc56juiifft4b9bg8u43nmmhe2skfncrfvin3std4grbfa7io846nskq3j5b3819rvaddnbn0"
    },
    {
        "owner": "joey",
        "name": "yaml",
        "hash": "#plgokdvco3iu26r56u20faojs7pv0r0114pkd5aumt7ucd567t307bcuv92ejtkcvvmp0tg4e2g5d3btqbggn54pifbvql2kd9hlg48"
    },
    {
        "owner": "rachel",
        "name": "filesystem",
        "hash": "#sbh98idno2b9ide5ue7bcj01ftu7u9msm57g3jn7q9efsbo0bdtnaei5i8sq4p3gb6p8alkqrp8gttp4ptvq9f45c8stkf39l9pvb2g"
    },
    {
        "owner": "ross",
        "name": "http",
        "hash": "#1l4rfnjpsut79lc0kcv7aa4m6elk1lj7nse69ptaipb4gvlfa7kcnqrte56opeeb5ahrr6tvms2052e9fjjjuh97glkll6hp3lam788"
    }
]
GET /api/projects?owner=joey
[
    {
        "owner": "joey",
        "name": "json",
        "hash": "#vjmnhfbas8pejgpgsh26255ebaolepuc56juiifft4b9bg8u43nmmhe2skfncrfvin3std4grbfa7io846nskq3j5b3819rvaddnbn0"
    },
    {
        "owner": "joey",
        "name": "yaml",
        "hash": "#plgokdvco3iu26r56u20faojs7pv0r0114pkd5aumt7ucd567t307bcuv92ejtkcvvmp0tg4e2g5d3btqbggn54pifbvql2kd9hlg48"
    }
]
```