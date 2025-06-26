# MCP Setup

UCM comes packaged with a built-in MCP server for use with AI agents.
It includes tools for allowing an AI agent to inspect and search your code, write and typecheck new code, search Share
for projects and definitions, and more!

## Setup

### Connecting to a running UCM executable (recommended)

To configure the MCP for use with Claude, edit your Claude Desktop config JSON file, which is found:

* On Mac: `~/Library/Application Support/Claude/claude_desktop_config.json`
* On Windows: `%APPDATA%\Claude\claude_desktop_config.json`

Add the server config as a key in the `mcpServers` mapping there.

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

### MCP as an independent process

Alternatively you can connect to the MCP using the UCM executable directly via stdin/stdout.

Add the following as a key in the `mcpServers` mapping there. Replace `<path-to-ucm>` with the path to your `ucm` executable.
E.g. on Mac this is likely `/opt/homebrew/bin/ucm`.

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

Note that this causes the MCP server to run as an entirely separate process.
If you're also running a UCM instance you may notice that it gets out of sync with changes made by AI agents via MCP.

## Usage

By default, Claude will now automatically use the Unison MCP server when it deems it appropriate, however
if you're planning to ask Claude to write some Unison code it's recommended you use one of the Unison MCP prompts
to kick off your interaction. You can find them by clicking the "plus" icon next to the prompt input box, and then
choosing `Add from unison` and selecting the appropriate prompt.
