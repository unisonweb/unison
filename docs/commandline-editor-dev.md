The Unison CLI code is made up of a few components:

`CommandLine.Main` sets up threads to watch the filesystem and parse `stdin` to produce `Editor.Event`s and `Editor.Input`s respectively.

`Editor.Input` parsers are defined in InputPattern.hs and InputPatterns.hs.

`Action.loop` receives `Editor.Event`s and `Editor.Input`s and executes `Editor.Command`s.  This loop can't use `IO` or access the `Codebase` -- any access to these things must come from what `Editor.Command` provides.

`Editor.Command`s are defined in Editor.hs and interpreted by `Editor.commandLine`.  `Editor.commandLine` *does* use `IO` and access the `Codebase`.`

One of the `Editor.Commands` that can be executed is `Notify`, which presents an `Editor.Output` to the user.  Our current implementation is in `OutputMessages.notifyUser`.

CLI is in charge of remembering what branch the user is working on.  It currently keeps an in-memory copy of the Branch, but I'm not sure that buys us anything besides headache, given that every operation needs to hit the disk anyway.  I guess it could be used to detect an external change.

### FAQ

__Q:__ What's with the `branchChange` callback in `Editor.commandLine`?
__A:__ The command line needs to know the current branch in order to give tab completion. So whenever the branch changes, `commandLine` should invoke that callback.


