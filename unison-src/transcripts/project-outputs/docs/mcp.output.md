# MCP Setup

UCM comes packaged with a built-in MCP server for use with AI agents.
It includes tools for allowing an AI agent to inspect and search your code, write and typecheck new code, search Share
for projects and definitions, and more\!

## Setup

### MCP as an independent process (Recommended)

This approach allows agents to connect to UCM's MCP server directly via stdin/stdout.

Note that this causes an additional UCM to run as an entirely independent process for each agent you're using.

To configure the MCP for use with Claude (and any tools which read Claude's json config), edit your Claude Desktop config JSON file, which is found:

  - On Mac: `~/Library/Application Support/Claude/claude_desktop_config.json`
  - On Windows: `%APPDATA%\Claude\claude_desktop_config.json`

Configure a `unison` key in your `mcpServers` object as below. Replace `<path-to-ucm>` with the path to your `ucm` executable.
E.g. on Mac this is likely `/opt/homebrew/bin/ucm`, you can run `which ucm` to find your UCM executable path.

``` json
{
  "mcpServers": {
    "unison": {
      "command": "<path-to-ucm>",
      "args": ["mcp"]
    }
}
```

E.g. my complete file on Mac looks like this:

``` 
{
  "mcpServers": {
    "unison": {
      "command": "/opt/homebrew/bin/ucm",
      "args": ["mcp"]
    }
  }
}
```

After saving the file, restart the Claude Desktop app. You should then see a new "unison" option in the MCP server list.

### Connecting to a running UCM executable (not recommended)

If instead you wish to connect an agent to a running UCM executable you can use an HTTP MCP connection.
This is less consistent than the stdio approach, since some agents have rather poor handling of error states if you close your running UCM, or start
up an agent without UCM already running.

The following defaults should work if you haven't tweaked things, but ensure you use the correct port and token if you've changed them:

``` 
{
  "mcpServers": {
    "unison": {
      "type": "streamable-http",
      "url": "http://localhost:5858/codebase/mcp",
      "note": "Replace 5858 and 'codebase' with your UCM_PORT and UCM_TOKEN respectively if you've changed the defaults."
    }
  }
}
```

After saving the file, restart the Claude Desktop app. You should now see a new "unison" option in the MCP server list.


## Usage

By default, Claude will now automatically use the Unison MCP server when it deems it appropriate, however
if you're planning to ask Claude to write some Unison code it's recommended you use one of the Unison MCP prompts
to kick off your interaction. You can find them by clicking the "plus" icon next to the prompt input box, and then
choosing `Add from unison` and selecting the appropriate prompt.
