# MCP Setup

UCM comes packaged with a built-in MCP server for use with AI agents.
It includes tools for allowing an AI agent to inspect and search your code, write and typecheck new code, search Share
for projects and definitions, and more\!

## Setup

To configure the MCP for use with Claude, edit your Claude Desktop config JSON file, which is found:

  - On Mac: `~/Library/Application Support/Claude/claude_desktop_config.json`
  - On Windows: `%APPDATA%\Claude\claude_desktop_config.json`

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

After saving the file, restart the Claude Desktop app. You should now see a new "unison" option in the MCP server list.

## Usage

By default, Claude will now automatically use the Unison MCP server when it deems it appropriate, however
if you're planning to ask Claude to write some Unison code it's recommended you use one of the Unison MCP prompts
to kick off your interaction. You can find them by clicking the "plus" icon next to the prompt input box, and then
choosing `Add from unison` and selecting the appropriate prompt.
