``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison :hide
README = {{
  This is a scratch project for testing tools in MCP.
}}

myTerm = 99

type MyType = MyConstructor

test> myPassingTest = [Ok "passing"]
test> myFailingTest = [Fail "failing"]

main : '{IO, Exception} (Optional Text)
main = do
  match getArgs.impl() with
    Left _err -> None
    Right args ->
      match List.at 0 args with
        None -> None
        Some txt ->
          _ = putBytes.impl (io2.IO.stdHandle StdOut) (Text.toUtf8 txt)
          Some txt
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

## list-project-branches

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "list-project-branches",
      "arguments": {
        "projectName": "scratch"
      }
    }
  }

RESPONSE:
  null

```

## typecheck-code

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "typecheck-code",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }, "code": {"sourceCode": "> x = 1 + 2"}
      }
    }
  }

RESPONSE:
  null

```

## docs

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "docs",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }, "name": "README"
      }
    }
  }

RESPONSE:
  null

```

## run

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "run",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "mainFunctionName": "main",
        "args": ["hello"]
      }
    }
  }

RESPONSE:
  null

```

## list-project-definitions

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "list-project-definitions",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }
      }
    }
  }

RESPONSE:
  null

```

## list-project-libraries

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "list-project-libraries",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }
      }
    }
  }

RESPONSE:
  null

```

## list-project-branches

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "list-project-branches",
      "arguments": {
        "projectName": "scratch"
      }
    }
  }

RESPONSE:
  null

```

## view-definitions

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "view-definitions",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }, "names": ["myTerm", "MyType"]
      }
    }
  }

RESPONSE:
  null

```

## search-definitions-by-name

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "search-definitions-by-name",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }, "query": "my"
      }
    }
  }

RESPONSE:
  null

```

## search-by-type

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "search-by-type",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }, "query": "Nat"
      }
    }
  }

RESPONSE:
  null

```

## get-current-project-context

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "get-current-project-context",
      "arguments": { }
    }
  }

RESPONSE:
  null

```

## tests

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "run-tests",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        }
      }
    }
  }

RESPONSE:
  null

```

## update-definitions

``` ucm
scratch/foo> builtins.merge lib.builtins

  Done.
```

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "update-definitions",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "foo"
        }, "code": {"text": "myTerm = 100"}
      }
    }
  }

RESPONSE:
  null

```

## rename-definition

``` ucm
scratch/foo> builtins.merge lib.builtins

  Done.
```

``` unison :hide
termToRename = 42
```

``` ucm
scratch/rename-test> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now rename it (only changes the final segment):

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "rename-definition",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "rename-test"
        },
        "oldName": "termToRename",
        "newNameSegment": "renamedTerm"
      }
    }
  }

RESPONSE:
  null

```

## move-definition

First, set up a branch with a term to move:

``` ucm
scratch/move-test> builtins.merge lib.builtins

  Done.
```

``` unison :hide
original.termToMove = 99
```

``` ucm
scratch/move-test> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now move it to a different namespace:

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "move-definition",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "move-test"
        },
        "oldName": "original.termToMove",
        "newName": "destination.movedTerm"
      }
    }
  }

RESPONSE:
  null

```

## move-to

First, set up a branch with terms to move:

``` ucm
scratch/move-to-test> builtins.merge lib.builtins

  Done.
```

``` unison :hide
source.termA = 1
source.termB = 2
```

``` ucm
scratch/move-to-test> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now move multiple terms into a destination namespace (preserving final segments):

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "move-to",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "move-to-test"
        },
        "sources": ["source.termA", "source.termB"],
        "destination": "dest"
      }
    }
  }

RESPONSE:
  null

```

## delete-definitions

First, set up a branch with a term to delete:

``` ucm
scratch/delete-test> builtins.merge lib.builtins

  Done.
```

``` unison :hide
termToDelete = 42
```

``` ucm
scratch/delete-test> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now delete it:

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "delete-definitions",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "delete-test"
        },
        "names": ["termToDelete"],
        "force": false
      }
    }
  }

RESPONSE:
  null

```

## delete-namespace

First, set up a branch with a namespace containing definitions:

``` ucm
scratch/delete-ns-test> builtins.merge lib.builtins

  Done.
```

``` unison :hide
MyNamespace.foo = 1
MyNamespace.bar = 2
```

``` ucm
scratch/delete-ns-test> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now delete the namespace:

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "delete-namespace",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "delete-ns-test"
        },
        "namespaceName": "MyNamespace",
        "force": false
      }
    }
  }

RESPONSE:
  null

```

## reflog

First, set up a branch with some changes to create reflog entries:

``` ucm
scratch/reflog-test> builtins.merge lib.builtins

  Done.
```

``` unison :hide
reflogTestTerm = 1
```

``` ucm
scratch/reflog-test> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now get the reflog:

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "reflog",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "reflog-test"
        },
        "scope": "branch",
        "limit": 5,
        "includeTimestamps": false
      }
    }
  }

RESPONSE:
  null

```

## history

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "history",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "reflog-test"
        },
        "limit": 5
      }
    }
  }

RESPONSE:
  null

```

## create-branch

### Create branch from current context

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "create-branch",
      "arguments": {
        "projectName": "scratch",
        "newBranchName": "from-current",
        "sourceType": "current"
      }
    }
  }

RESPONSE:
  null

```

### Create empty branch

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "create-branch",
      "arguments": {
        "projectName": "scratch",
        "newBranchName": "empty-branch",
        "sourceType": "empty"
      }
    }
  }

RESPONSE:
  null

```

### Create branch from existing branch

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "create-branch-x",
      "arguments": {
        "projectName": "scratch",
        "newBranchName": "from-main",
        "sourceType": "branch",
        "sourceBranchName": "main"
      }
    }
  }

RESPONSE:
  null

```
