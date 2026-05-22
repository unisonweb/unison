## compile and lib-upgrade MCP tools

Tests for the `compile` and `lib-upgrade` MCP tools.

``` ucm
scratch/main> builtins.mergeio lib.builtins

  Done.
```

``` unison :hide
myMain : '{IO, Exception} ()
myMain = do
  _ = putBytes.impl (io2.IO.stdHandle StdOut) (Text.toUtf8 "compiled!\n")
  ()
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

### compile — success

Compile `myMain` to a standalone `.uc` file.

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "compile",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "mainFunctionName": "myMain",
        "outputPath": "/tmp/mcp-compile-test-out"
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
                  "text": "{\"errorMessages\":[],\"outputMessages\":[],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

### compile — missing definition

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "compile",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "mainFunctionName": "doesNotExist",
        "outputPath": "/tmp/mcp-compile-test-missing"
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
                  "text": "{\"errorMessages\":[\"ð¶\\n\\nI looked for a function `doesNotExist` in the most recently typechecked file and codebase but couldn't find one. It has to have the type:\\n\\n  doesNotExist : '{IO, Exception} result\"],\"outputMessages\":[\"ð¶\\n\\nI looked for a function `doesNotExist` in the most recently typechecked file and codebase but couldn't find one. It has to have the type:\\n\\n  doesNotExist : '{IO, Exception} result\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

### lib-upgrade — old lib not found

``` api
POST /mcp
BODY:
  {
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "lib-upgrade",
      "arguments": {
        "projectContext": {
          "projectName": "scratch",
          "branchName": "main"
        },
        "oldLibName": "no_such_lib_1_0_0",
        "newLibName": "no_such_lib_2_0_0"
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
                  "text": "{\"errorMessages\":[\"â ï¸\\n\\nThe namespace .lib.no_such_lib_1_0_0 doesn't exist.\"],\"outputMessages\":[\"â ï¸\\n\\nThe namespace .lib.no_such_lib_1_0_0 doesn't exist.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```
