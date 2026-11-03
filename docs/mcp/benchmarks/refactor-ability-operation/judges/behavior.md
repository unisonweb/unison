You are judging the behavior of the handlers of the ability `Journal` in the Unison codebase you are connected to. A previous agent added the operation `warn : Text ->{Journal} ()`, so that `warn message` does what `entry ("WARN: " ++ message)` does in every handler. You may use any tool except `learn`; this check runs code that prints, so it needs the `transcript-io` tool. Don't change the project's definitions; use a `unison` block of watch expressions (`> expr`) and scratch definitions for your own checks and don't `update` them. Work in `scratch/main`.

Evaluate each of these and compare with the expected value:

1. `Journal.collect do Journal.entry "a"; Journal.warn "b"; Journal.entry "c"; 1` is `(["a", "WARN: b", "c"], 1)`.
2. `Sink.collect do Journal.toSink do Journal.entry "a"; Journal.warn "b"; 3` is `(["a", "WARN: b"], 3)` (the handler forwards what it receives to the `Sink` ability, and `Sink.collect` gathers it).
3. `Journal.ignore do Journal.warn "b"; Journal.entry "c"; 4` is `4`: nothing is recorded and the program still finishes.
4. The printing handler. In a `unison` block define

       jprint : '{IO, Exception} ()
       jprint = do
         Journal.printing do
           Journal.entry "a"
           Journal.warn "b"
           Journal.entry "c"

   then in a `ucm` block `scratch/main> run jprint`. The standard output is exactly the three lines `a`, `WARN: b`, `c`, in that order.
5. `Journal.collect do Inventory.audit [("apple", 3), ("pear", 0), ("fig", 2)]` is `(["apple: 3", "WARN: pear is out of stock", "fig: 2"], 5)`, and `Journal.collect do Inventory.audit []` is `([], 0)`.
6. `Journal.collect do Inventory.receive "fig" 2` is `(["received 2 fig"], ())`: the other user of `entry` did not change.
7. Several warnings in a row keep their order: `Journal.collect do Journal.warn "x"; Journal.warn "y"; Journal.entry "z"; ()` is `(["WARN: x", "WARN: y", "z"], ())`.

It passes only if every check passes.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly PASS or FAIL.
