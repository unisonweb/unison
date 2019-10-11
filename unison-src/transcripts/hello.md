
# Hello!

This markdown file is also a Unison transcript file. Transcript files are an easy way to create self-documenting Unison programs, libraries, and tutorials.

The format is just a regular markdown file with some fenced code blocks that are typechecked and elaborated by `ucm`. For example, you can call this transcript via:

```
$ ucm hello.md
```

> Alternately `ucm sandbox hello.md` runs the transcript on a freshly generated temporary codebase. Do `ucm help` to learn more about usage.

Fenced code blocks of type `unison` and `ucm` are treated specially:

* `ucm` blocks are executed, and the output is interleaved into the output markdown file after each command, replacing the original `ucm` block.
* `unison` blocks are typechecked, and a `ucm` block with the output of typechecking and execution of the file is inserted immediately afterwards.

## Let's try it out!!

In the `unison` fenced block, you can give an (optional) file name (defaults to `scratch.u`), like so:

```unison myfile.u
x = 42
```

Let's go ahead and add that to the codebase, then make sure it's there:

```ucm
.> add
.> view x
```

If `view` returned no results, the transcript would fail at this point.

## Hiding output

You may not always want to view the output of typechecking and evaluaion every time, in which case, you can add `:hide` to the block. For instance:

```unison:hide
y = 99
```

This works for `ucm` blocks as well.

```ucm:hide
.> rename.term x answerToUltimateQuestionOfLife
```

## Expecting failures

Sometimes, you have a block which you are _expecting_ to fail, perhaps because you're illustrating how something would be a type error. Adding `:error` to the block will check for this. For instance, this program has a type error:

```unison:error
hmm : .builtin.Nat
hmm = "Not, in fact, a number"
```
