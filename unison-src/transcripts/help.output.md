### Tests the output of help related commands

```ucm
.> help-topics

  🌻
  
  Here's a list of topics I can tell you more about: 
  
    testcache
    filestatus
    namespaces
    messages.disallowedAbsolute
  
  Example: use `help filestatus` to learn more about that topic.

.> help-topic testcache

  🎈
  
  Unison caches the results of test> watch expressions. Since
  these expressions are pure and always yield the same result
  when evaluated, there's no need to run them more than once!
  
  A test is rerun only if it has changed, or if one of the
  definitions it depends on has changed.

.> help-topic filestatus

  📓
  
  Here's a list of possible status messages you might see for
  definitions in a .u file.
  
  needs update         A definition with the same name as an
                       existing definition. Doing `update`
                       instead of `add` will turn this failure
                       into a successful update.
                       
  conflicted           A definition with the same name as an
                       existing definition. Resolving the
                       conflict and then trying an `update`
                       again will turn this into a successful
                       update.
                       
  term/ctor collision  A definition with the same name as an
                       existing constructor for some data type.
                       Rename your definition or the data type
                       before trying again to `add` or `update`.
                       
  ctor/term collision  A type defined in the file has a
                       constructor that's named the same as an
                       existing term. Rename that term or your
                       constructor before trying again to `add`
                       or `update`.
                       
  blocked              This definition was blocked because it
                       dependended on a definition with a failed
                       status.
                       
  extra dependency     This definition was added because it was
                       a dependency of a definition explicitly
                       selected.

.> help-topic namespaces

  🧐
  
  There are two kinds of namespaces, absolute, such as (.foo.bar
  or .base.math.+) and relative, such as (math.sqrt or
  util.List.++).
  
  Relative names are converted to absolute names by prepending
  the current namespace. For example, if your Unison prompt
  reads:
  
    .foo.bar>
  
  and your .u file looks like:
  
    x = 41
  
  then doing an add will create the definition with the absolute
  name .foo.bar.x = 41
  
  and you can refer to x by its absolute name .foo.bar.x
  elsewhere in your code. For instance:
  
    answerToLifeTheUniverseAndEverything = .foo.bar.x + 1

.> help-topic messages.disallowedAbsolute

  🤖
  
  Although I can understand absolute (ex: .foo.bar) or relative
  (ex: util.math.sqrt) references to existing definitions
  (help namespaces to learn more), I can't yet handle giving new
  definitions with absolute names in a .u file.
  
  As a workaround, you can give definitions with a relative name
  temporarily (like `exports.blah.foo`) and then use `move.*` or
  `merge` commands to move stuff around afterwards.

.> help testcache

  🎈
  
  Unison caches the results of test> watch expressions. Since
  these expressions are pure and always yield the same result
  when evaluated, there's no need to run them more than once!
  
  A test is rerun only if it has changed, or if one of the
  definitions it depends on has changed.

.> help filestatus

  📓
  
  Here's a list of possible status messages you might see for
  definitions in a .u file.
  
  needs update         A definition with the same name as an
                       existing definition. Doing `update`
                       instead of `add` will turn this failure
                       into a successful update.
                       
  conflicted           A definition with the same name as an
                       existing definition. Resolving the
                       conflict and then trying an `update`
                       again will turn this into a successful
                       update.
                       
  term/ctor collision  A definition with the same name as an
                       existing constructor for some data type.
                       Rename your definition or the data type
                       before trying again to `add` or `update`.
                       
  ctor/term collision  A type defined in the file has a
                       constructor that's named the same as an
                       existing term. Rename that term or your
                       constructor before trying again to `add`
                       or `update`.
                       
  blocked              This definition was blocked because it
                       dependended on a definition with a failed
                       status.
                       
  extra dependency     This definition was added because it was
                       a dependency of a definition explicitly
                       selected.

.> help namespaces

  🧐
  
  There are two kinds of namespaces, absolute, such as (.foo.bar
  or .base.math.+) and relative, such as (math.sqrt or
  util.List.++).
  
  Relative names are converted to absolute names by prepending
  the current namespace. For example, if your Unison prompt
  reads:
  
    .foo.bar>
  
  and your .u file looks like:
  
    x = 41
  
  then doing an add will create the definition with the absolute
  name .foo.bar.x = 41
  
  and you can refer to x by its absolute name .foo.bar.x
  elsewhere in your code. For instance:
  
    answerToLifeTheUniverseAndEverything = .foo.bar.x + 1

.> help messages.disallowedAbsolute

  🤖
  
  Although I can understand absolute (ex: .foo.bar) or relative
  (ex: util.math.sqrt) references to existing definitions
  (help namespaces to learn more), I can't yet handle giving new
  definitions with absolute names in a .u file.
  
  As a workaround, you can give definitions with a relative name
  temporarily (like `exports.blah.foo`) and then use `move.*` or
  `merge` commands to move stuff around afterwards.

.> help

  

.> help push

  

```
### Tests output 'help your-command' due to broken invocation of a command

These are treated as failures (for example: a user invokes wrong arguments, less or more)

```ucm
.> load

  😶
  
  There's nothing for me to add right now.
  
  Hint: I'm currently watching for definitions in .u files under
        the ~/unisonweb/unison/transcript-2e60832c90c01787
        directory. Make sure you've updated something there
        before using the `add` or `update` commands, or use
        `load` to load a file explicitly.

.> load 1 2 3

  

.> display

  

.> display 1 2 3

  

.> display.to

  

.> display.to 1 2 3

  

.> docs

  

.> docs 1 2

  

.> list

  nothing to show

.> list 1 2

  

.> alias.many

  

.> cd

  

.> cd 1 2 3

  

.> back 1

  

.> delete.namespace 1 2 3

  

.> delete.patch 1 2

  

.> copy.patch 1 2 3

  

.> rename.patch 1 2 3

  

.> rename.namespace 1 2 3

  

.> history 1 2 3

  

.> fork

  

.> fork 1

  

.> fork 1 2 3

  

.> reset-root

  

.> reset-root 1 2

  

.> pull

  ❗️
  
  I don't know where to pull from! Add a line like `GitUrl. the
  current namespace = <some-git-url>' to .unisonConfig. Type
  `help pull ` for more information.

.> pull 1 2 3

  

.> pull-request.create

  

.> pull-request.create 1 2 3

  

.> pull-request.load

  

.> pull-request.load 1

  

.> pull-request.load 1 2 3 4

  

.> merge

  

.> merge 1 2 3

  

.> diff.namespace

  

.> diff.namespace 1

  

.> diff.namespace 1 2 3

  

.> merge.preview

  

.> merge.preview 1 2 3

  

.> replace.type

  

.> replace.type 1 

  

.> replace.type 1 2

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    1
    2

  1 is not a kind of name I understand.

.> replace.term

  

.> replace.term 1

  

.> replace.term 1 2

  ⚠️
  
  The following names were not found in the codebase. Check your spelling.
    1
    2

  1 is not a kind of name I understand.

.> link

  

.> links

  

.> unlink

  

.> names

  

.> names 1 2

  

.> dependents

  

.> dependents 1 2

  

.> dependencies

  

.> dependencies 1 2

  

.> run

  

.> run 1 2

  

.> create.author

  

```
