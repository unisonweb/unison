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
                  "text": "{\"outputMessages\":[\"     Branch   Remote branch\\n1.   main     \"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"Loading changes detected in scratch.u.\",\"No changes found.\",\"  1 | > x = 1 + 2\\n        â§©\\n        3\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"content\":[{\"text\":\"{\\\"outputMessages\\\":[\\\"1. main : '{IO, Exception} Optional Text\\\\n2. myFailingTest : [Result]\\\\n3. myPassingTest : [Result]\\\\n4. myTerm : Nat\\\\n5. type MyType\\\\n6. MyType.MyConstructor : MyType\\\\n7. README : Doc2\\\\n\\\"],\\\"sourceCodeUpdates\\\":[],\\\"stderr\\\":\\\"\\\",\\\"stdout\\\":\\\"\\\"}\",\"type\":\"text\"}],\"isError\":false}",
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
                  "text": "{\"outputMessages\":[\"1. builtins. (840 terms, 122 types)\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"     Branch   Remote branch\\n1.   main     \"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"type MyType = MyConstructor\\n\\nmyTerm : Nat\\nmyTerm = 99\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"1. myFailingTest : [Result]\\n2. myPassingTest : [Result]\\n3. myTerm : Nat\\n4. type MyType\\n5. MyType.MyConstructor : MyType\\n\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"1. myTerm : Nat\\n\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "{\"outputMessages\":[\"Cached test results (`help testcache` to learn more)\\n\\n  1. myPassingTest   â passing\\n\\n  2. myFailingTest   â failing\\n\\nð« 1 test(s) failing, â 1 test(s) passing\\n\\nTip: Use view 1 to view the source of a test.\"],\"sourceCodeUpdates\":[],\"stderr\":\"\",\"stdout\":\"\"}",
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
                  "text": "Tool 'update-definitions' not found.",
                  "type": "text"
              }
          ],
          "isError": true
      }
  }

```
