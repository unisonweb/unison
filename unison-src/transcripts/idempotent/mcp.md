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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"     Branch   Remote branch\\n1.   main     \"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 2.12 ms (cpu), 2.18 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Loading changes detected in <mcp-virtual-source>.\",\"No changes found.\",\"  1 | > x = 1 + 2\\n        â§©\\n        3\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"evaluating watches: 7.13 ms (cpu), 7.48 ms (system)\\nUnisonFileChanged: 54.7 ms (cpu), 55.5 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"This is a scratch project for testing tools in MCP.\\n\\n\\n\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 30.8 ms (cpu), 32.2 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Some \\\"hello\\\"\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"helloInputPattern: 27.7 ms (cpu), 28.8 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"content\":[{\"text\":\"{\\\"errorMessages\\\":[],\\\"outputMessages\\\":[\\\"1. main : '{IO, Exception} Optional Text\\\\n2. myFailingTest : [Result]\\\\n3. myPassingTest : [Result]\\\\n4. myTerm : Nat\\\\n5. type MyType\\\\n6. MyType.MyConstructor : MyType\\\\n7. README : Doc2\\\\n\\\"],\\\"sourceCodeUpdates\\\":[],\\\"stderr\\\":\\\"\\\",\\\"stdout\\\":\\\"InputPattern: 5.92 ms (cpu), 7.14 ms (system)\\\\n\\\"}\",\"type\":\"text\"}],\"isError\":false}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"1. builtins. (944 terms, 136 types)\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 6.26 ms (cpu), 6.52 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"     Branch   Remote branch\\n1.   main     \"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 1.65 ms (cpu), 1.71 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"type MyType = MyConstructor\\n\\nmyTerm : Nat\\nmyTerm = 99\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 9.47 ms (cpu), 9.82 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"1. myFailingTest : [Result]\\n2. myPassingTest : [Result]\\n3. myTerm : Nat\\n4. type MyType\\n5. MyType.MyConstructor : MyType\\n\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 5.43 ms (cpu), 5.66 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"1. myTerm : Nat\\n\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 10.2 ms (cpu), 10.6 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"branchName\":\"main\",\"projectName\":\"scratch\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Cached test results (`help testcache` to learn more)\\n\\n  1. myPassingTest   â passing\\n\\n  2. myFailingTest   â failing\\n\\nð« 1 test(s) failing, â 1 test(s) passing\\n\\nTip: Use view 1 to view the source of a test.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 6.45 ms (cpu), 6.75 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Loading changes detected in <mcp-virtual-source>.\",\"+ myTerm : Nat\\n\\nRun `update` to apply these changes to your codebase.\",\"Done.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"UnisonFileChanged: 30.4 ms (cpu), 30.8 ms (system)\\nupdateProjectBranchRoot: 4.31 ms (cpu), 4.78 ms (system)\\nInputPattern: 10.7 ms (cpu), 11.4 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Renamed:\\n\\n  termToRename -> renamedTerm\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"updateProjectBranchRoot: 4.06 ms (cpu), 4.28 ms (system)\\nInputPattern: 16.5 ms (cpu), 17.2 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Done.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"updateProjectBranchRoot: 4.18 ms (cpu), 4.50 ms (system)\\nInputPattern: 17.3 ms (cpu), 18.2 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Moved:\\n\\n  source.termA -> dest.termA\\n  source.termB -> dest.termB\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"updateProjectBranchRoot: 4.83 ms (cpu), 5.04 ms (system)\\nInputPattern: 30.6 ms (cpu), 31.8 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"I deleted these terms:\\n\\n  1. termToDelete\\n\\nTip: You can use `undo` or use a hash from `reflog` to undo this change.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"updateProjectBranchRoot: 3.78 ms (cpu), 4.04 ms (system)\\nInputPattern: 10.6 ms (cpu), 11.2 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Done.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"updateProjectBranchRoot: 3.71 ms (cpu), 4.05 ms (system)\\nInputPattern: 13.8 ms (cpu), 14.6 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"entries\":[{\"branch\":\"reflog-test\",\"fromHash\":\"#bkqqb4f3c3\",\"project\":\"scratch\",\"reason\":\"update\",\"toHash\":\"#qim3pjrq57\"},{\"branch\":\"reflog-test\",\"fromHash\":\"#sg60bvjo91\",\"project\":\"scratch\",\"reason\":\"builtins.merge scratch/reflog-test:lib.builtins\",\"toHash\":\"#bkqqb4f3c3\"},{\"branch\":\"reflog-test\",\"fromHash\":null,\"project\":\"scratch\",\"reason\":\"Branch Created\",\"toHash\":\"#sg60bvjo91\"}],\"hasMore\":false}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Note: The most recent namespace hash is immediately below this message.\\n\\nâ 1. #qim3pjrq57\\n\\n  + Adds / updates:\\n  \\n    reflogTestTerm\\n\\nâ¡ 2. #bkqqb4f3c3 (start of history)\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 5.14 ms (cpu), 5.51 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Done. I've created the from-current branch based off of reflog-test.\\n\\nTip: To merge your work back into the reflog-test branch, first `switch /reflog-test` then `merge /from-current`.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 6.06 ms (cpu), 6.45 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Done. I've created an empty branch scratch/empty-branch.\\n\\nTip: Use `merge /somebranch` to initialize this branch.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"InputPattern: 5.08 ms (cpu), 5.80 ms (system)\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

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
  {
      "id": 1,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "Tool 'create-branch-x' not found.",
                  "type": "text"
              }
          ],
          "isError": true
      }
  }

```
