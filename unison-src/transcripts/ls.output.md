```api
POST /api/commands/run { "command": "ls", "arguments": [], "namespace": "." }

  nothing to show

```


```



🛑

The transcript failed due to an error in the stanza above. The error is:

Error decoding response from Request {
  host                 = "127.0.0.1"
  port                 = 64218
  secure               = False
  requestHeaders       = [("Content-Type","application/json")]
  path                 = "/zKzF4lzKozbL5iee/api/commands/run"
  queryString          = ""
  method               = "POST"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
: Error in $: Failed reading: not a valid json value at 'Somethingwentwrong'
Something went wrong
