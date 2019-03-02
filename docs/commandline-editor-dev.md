The Unison CLI code is made up of a few components:

`CommandLine.Main` sets up threads to watch the filesystem and parse `stdin` to produce `Editor.Event`s and `Editor.Input`s respectively.

`Editor.Input` parsers are defined in InputPattern.hs and InputPatterns.hs.

`Action.loop` receives `Editor.Event`s and `Editor.Input`s and executes `Editor.Command`s.  This loop can't use `IO` or access the `Codebase`.

`Editor.Command`s are defined in Editor.hs and interpreted by `Editor.commandLine`.  `Editor.commandLine` *does* use `IO` and access the `Codebase`.`

One of the `Editor.Commands` that can be executed is `Notify`, which presents an `Editor.Output` to the user.  Our presentation implementation is in `OutputMessages.notifyUser`.
