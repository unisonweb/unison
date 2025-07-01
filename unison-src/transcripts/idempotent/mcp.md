``` ucm
scratch/main> builtins.merge

  Done.
```

``` api
-- Namespace segment prefix search
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
                  "text": "{\"outputMessages\":[\"     Branch   Remote branch\\n1.   main     \"],\"sourceCodeUpdates\":[]}",
                  "type": "text"
              }
          ],
          "isError": false
      }
  }

```
