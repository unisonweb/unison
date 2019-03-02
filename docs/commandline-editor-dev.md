The Unison CLI code is made up of a few components:

`CommandLine.Main` sets up threads to watch the filesystem and parse `stdin` to produce `Event`s and `Input`s respectively.

`Action.hs` contains a `loop` which receives `Event`s and `Input`s and executes `Command`s.  This loop can't use `IO` or access the `Codebase`.

`Command`s are defined in Editor.hs and interpreted by `Editor.commandLine`.  `Editor.commandLine` *does* use `IO` and access the `Codebase`.`

One of the `Commands` that can be executed is `Notify`, which presents an `Output` to the user.  `Output`s are defined in Editor.hs as well, and our presentation implementation is in `OutputMessages.notifyUser`.

`Event`s, `Input`s, `Output`s, and `Command`s are all defined in Editor.hs.

`Input` parsers are defined in InputPatterns.hs.
