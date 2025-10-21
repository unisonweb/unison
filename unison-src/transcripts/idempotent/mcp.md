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
                  "text": "{\"outputMessages\":[\"This is a scratch project for testing tools in MCP.\\n\\n\\n\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"Some \\\"hello\\\"\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"hello\"}",
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
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```

## update-definitions

MCP can't edit branches unless they are marked as editable.

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
          "branchName": "uneditable"
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
                  "text": "The provided project-branch is not editable.\nPlease ask the user to allow edits to this project in their MCP configuration by adding `--mcp-editable-branches=scratch/uneditable` to the invocation of the Unison mcp within their agent's mcp configuration.",
                  "type": "text"
              }
          ],
          "isError": true
      }
  }

```

Transcripts allow mcp-editing on `agent-*` branches.

``` ucm
scratch/agent-foo> builtins.merge lib.builtins

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
          "branchName": "agent-foo"
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
                  "text": "{\"errorMessages\":[],\"outputMessages\":[\"Loading changes detected in <mcp-virtual-source>.\",\"+ myTerm : Nat\\n\\nRun `update` to apply these changes to your codebase.\",\"Done.\"],\"sourceCodeUpdates\":[]}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```
