### Tests the output of help related commands

```ucm
.> help-topics
.> help-topic testcache
.> help-topic filestatus
.> help-topic namespaces
.> help-topic messages.disallowedAbsolute
.> help testcache
.> help filestatus
.> help namespaces
.> help messages.disallowedAbsolute
.> help
.> help push
```

### Tests output 'help your-command' due to broken invocation of a command

These are treated as failures (for example: a user invokes wrong arguments, less or more)

```ucm:error
.> load 1 2 3
.> display
.> display 1 2 3
.> display.to
.> display.to 1 2 3
.> docs
.> docs 1 2
.> list
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
.> replace.term
.> replace.term 1
.> replace.term 1 2
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

load cannot be tested as its output changes per invocation of the trancript
