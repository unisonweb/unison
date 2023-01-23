This directory contains libraries necessary for building and running
unison programs via Chez Scheme. At the moment, they need to be
manually installed in the expected location. The default location is
the `unisonlanguage` directory in the XDG data directory.
Specifically, the `unison` subdirectory should be (recursively) copied
to:

    $XDG_DATA_DIRECTORY/unisonlanguage/scheme-libs

On unix systems, the XDG directory is ~/.local/share by default, ~
being the home directory. On Windows, this is instead the %APPDATA%
directory. See:

    https://hackage.haskell.org/package/directory/docs/System-Directory.html#t:XdgDirectory

for more information.

UCM can also be told to look in another directory by setting the
`SchemeLibs.Static` item in the unison config file. If this path is
denoted by `$CUSTOM`, then the compiler commands will look in:

    $CUSTOM/scheme-libs/

for the `unison/` directory containing the library files.

The compiler commands also expect Chez Scheme to be installed
separately, and for `scheme` to be callable on the user's path. For
information on how to install, see:

    https://github.com/cisco/ChezScheme/blob/main/BUILDING

For more information on Chez Scheme in general, see:

    https://cisco.github.io/ChezScheme/csug9.5/csug.html

There are two ways to run code via scheme. The first is to use
`run.native`, which compiles and immediately runs a given unison
definition. The second is `compile.native`, which produces a Chez
scheme `.boot` file with a specified name. The boot file can then be
used with the scheme binary to run the precompiled program, like so:

    scheme -b ./my-program.boot

It is also possible to install the boot file in a particular
directory, and make a renamed copy of the scheme binary, which will
automatically execute the boot file with the corresponding name on
start up. For more information on how to accomplish that, see:

    https://cisco.github.io/ChezScheme/csug9.5/use.html#./use:h8

---

A tip for working on files in this directory:

Assuming your are doing so by creating a unison test case, it can be
faster to have scheme code for that test case generated once, and then
just work on filling out the library functionality here. To do this,
you can run the ucm command:

    run.native <example>

This will cause a corresponding scheme file to be created in:

    $XDG_CACHE_DIRECTORY/unisonlanguage

This can be copied back to the unison directory, and then run directly
with something like:

    scheme --libdirs chez-libs:~/.cache/unisonlanguage/scheme-libs --script foo.scm

Then you can test directly against any changes to chez-libs, instead
of having to copy them to a different location, restart ucm, etc.

