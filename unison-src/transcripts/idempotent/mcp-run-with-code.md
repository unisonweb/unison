## run with code parameter

This tests the `run` tool's optional `code` parameter, which typechecks source
before running a definition from it — without updating the codebase.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison :hide
printLn : Text ->{IO} ()
printLn msg =
  _ = putBytes.impl (io2.IO.stdHandle StdOut) (Text.toUtf8 (msg ++ "\n"))
  ()

existingMain : '{IO, Exception} ()
existingMain = do printLn "from codebase"
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

### Run without code — definition not in codebase (should fail)

`newMain` doesn't exist in the codebase, so running it without `code` should error.

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
        "mainFunctionName": "newMain",
        "args": []
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
                  "text": "{\"errorMessages\":[\"ð¶\\n\\nI looked for a function `newMain` in the most recently typechecked file and codebase but couldn't find one. It has to have the type:\\n\\n  newMain : '{IO, Exception} result\"],\"outputMessages\":[\"ð¶\\n\\nI looked for a function `newMain` in the most recently typechecked file and codebase but couldn't find one. It has to have the type:\\n\\n  newMain : '{IO, Exception} result\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

### Run with code (inline sourceCode) — same definition succeeds

Now provide `newMain` via the `code` parameter. It should typecheck and run.

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "run",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "mainFunctionName": "newMain",
        "args": [],
        "code": {"sourceCode": "newMain : '{IO, Exception} ()\nnewMain = do\n  _ = putBytes.impl (io2.IO.stdHandle StdOut) (Text.toUtf8 \"hello from code param\\n\")\n  ()"}
      }
    }
  }

RESPONSE:
  {
      "id": 2,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Loading changes detected in <mcp-virtual-source>.\",\"+ newMain : '{IO, Exception} ()\\n\\nRun `update` to apply these changes to your codebase.\",\"()\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"hello from code param\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

### Run with code (filePath) — loads from a .u file

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "run",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "mainFunctionName": "fileMain",
        "args": [],
        "code": {"filePath": "unison-src/transcripts/idempotent/mcp-run-with-code.u"}
      }
    }
  }

RESPONSE:
  {
      "id": 3,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Loading changes detected in unison-src/transcripts/idempotent/mcp-run-with-code.u.\",\"+ fileMain : '{IO, Exception} ()\\n\\nRun `update` to apply these changes to your codebase.\",\"()\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"hello from file\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

### Multi-term code — run one of several definitions

Provide multiple definitions in `code` and run just one of them.

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 4,
    "method": "tools/call",
    "params": {
      "name": "run",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "mainFunctionName": "greet",
        "args": [],
        "code": {"sourceCode": "helper : Text\nhelper = \"world\"\n\ngreet : '{IO, Exception} ()\ngreet = do\n  _ = putBytes.impl (io2.IO.stdHandle StdOut) (Text.toUtf8 (\"hello \" ++ helper ++ \"\\n\"))\n  ()"}
      }
    }
  }

RESPONSE:
  {
      "id": 4,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Loading changes detected in <mcp-virtual-source>.\",\"+ greet  : '{IO, Exception} ()\\n+ helper : Text\\n\\nRun `update` to apply these changes to your codebase.\",\"()\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"hello world\\n\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

### Confirm codebase is unchanged

The definitions provided via `code` should NOT have been added to the codebase.

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 5,
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
      "id": 5,
      "jsonrpc": "2.0",
      "result": {
          "content": [
              {
                  "text": "{\"content\":[{\"text\":\"{\\\"errorMessages\\\":[],\\\"outputMessages\\\":[\\\"1. existingMain : '{IO, Exception} ()\\\\n2. printLn : Text ->{IO} ()\\\\n\\\"],\\\"sourceCodeUpdates\\\":[],\\\"stderr\\\":\\\"\\\",\\\"stdout\\\":\\\"\\\"}\",\"type\":\"text\"}],\"isError\":false}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```
