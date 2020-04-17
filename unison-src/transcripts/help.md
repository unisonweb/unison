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
```
```ucm:error
.> display
```
```ucm:error
.> display 1 2 3
```
```ucm:error
.> display.to
```
```ucm:error
.> display.to 1 2 3
```
```ucm:error
.> docs
```
```ucm:error
.> docs 1 2
```
```ucm:error
.> list
```
```ucm:error
.> list 1 2
```
```ucm:error
.> alias.many
```
```ucm:error
.> cd
```
```ucm:error
.> cd 1 2 3
```
```ucm:error
.> back 1
```
```ucm:error
.> delete.namespace 1 2 3
```
```ucm:error
.> delete.patch 1 2
```
```ucm:error
.> copy.patch 1 2 3
```
```ucm:error
.> rename.patch 1 2 3
```
```ucm:error
.> rename.namespace 1 2 3
```
```ucm:error
.> history 1 2 3
```
```ucm:error
.> fork
```
```ucm:error
.> fork 1
```
```ucm:error
.> fork 1 2 3
```
```ucm:error
.> reset-root
```
```ucm:error
.> reset-root 1 2
```
```ucm:error
.> pull
```
```ucm:error
.> pull 1 2 3
```
```ucm:error
.> pull-request.create
```
```ucm:error
.> pull-request.create 1 2 3
```
```ucm:error
.> pull-request.load
```
```ucm:error
.> pull-request.load 1
```
```ucm:error
.> pull-request.load 1 2 3 4
```
```ucm:error
.> merge
```
```ucm:error
.> merge 1 2 3
```
```ucm:error
.> diff.namespace
```
```ucm:error
.> diff.namespace 1
```
```ucm:error
.> diff.namespace 1 2 3
```
```ucm:error
.> merge.preview
```
```ucm:error
.> merge.preview 1 2 3
```
```ucm:error
.> replace.type
```
```ucm:error
.> replace.type 1 
```
```ucm:error
.> replace.type 1 2
```
```ucm:error
.> replace.term
```
```ucm:error
.> replace.term 1
```
```ucm:error
.> replace.term 1 2
```
```ucm:error
.> link
```
```ucm:error
.> links
```
```ucm:error
.> unlink
```
```ucm:error
.> names
```
```ucm:error
.> names 1 2
```
```ucm:error
.> dependents
```
```ucm:error
.> dependents 1 2
```
```ucm:error
.> dependencies
```
```ucm:error
.> dependencies 1 2
```
```ucm:error
.> run
```
```ucm:error
.> run 1 2
```
```ucm:error
.> create.author
```
