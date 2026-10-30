<!-- expected: outcome FAIL -->
The worker fixed the type and `Config.parse` and stopped, believing the update was done: UCM left it on `update-main` and nothing was saved into `main`.

``` unison
unique type Config = { host : Text, retries : Nat, port : Nat }

Config.parse : Text -> Nat -> Config
Config.parse host port = Config host 3 port
```

``` ucm :error
scratch/main> update
```
