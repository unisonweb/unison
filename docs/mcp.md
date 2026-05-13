# MCP Setup

UCM comes packaged with a built-in MCP server for use with AI agents.
It includes tools for allowing an AI agent to inspect and search your code, write and typecheck new code, search Share
for projects and definitions, and more!

## Setup

### MCP as an independent process (Recommended)

This approach allows agents to connect to UCM's MCP server directly via stdin/stdout.

Note that this causes an additional UCM to run as an entirely independent process for each agent you're using.

#### Claude Code and IBM Bob

To configure the MCP for use with Claude Code, IBM Bob, and any tools which read the same `mcpServers` JSON config shape, edit the appropriate config JSON file:

Claude Code:
* On Mac: `~/Library/Application Support/Claude/claude_desktop_config.json`
* On Windows: `%APPDATA%\Claude\claude_desktop_config.json`
* On Linux: `$HOME/.claude.json`

IBM Bob:
* Global: `~/.bob/settings/mcp_settings.json`
* Project: `.bob/mcp.json`

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
}
```

_e.g._ my complete file on macOS looks like this:

``` json
{
  "mcpServers": {
    "unison": {
      "command": "/opt/homebrew/bin/ucm",
      "args": ["mcp"]
    }
  }
}
```

After saving the file, restart the app. For Claude, you should then see a new "unison" option in the MCP server list. For Bob, make sure "Use MCP Servers" is enabled in the MCP settings, then manage or restart the `unison` server from there.

#### Codex

Codex is primarily used for OpenAI models, but can be used with other model providers that support the OpenAI API.

Configuration is similar to Claude Code; locate your Codex config file:

* On Linux: `$HOME/.codex/config.toml`

``` toml
[mcp_servers.unison]
command = "/path/to/ucm"
args = ["mcp"]
```

Restart `codex`; you should now be able to see the Unison MCP server by entering `/mcp` in Codex:

```
/mcp

🔌  MCP Tools

  • Server: unison
    • Command: /home/bbarker/.nix-profile/bin/ucm mcp
    • Tools: docs, get-current-project-context, lib-install, list-definition-dependencies, list-definition-dependents, list-library-definitions, list-local-projects, list-project-branches,
list-project-definitions, list-project-libraries, search-by-type, search-definitions-by-name, share-project-readme, share-project-search, typecheck-code, view-definitions
```

#### Kiro CLI

Kiro CLI supports MCP servers at both global and workspace scope.

**Workspace scope** (recommended — scoped to this project):

Create `.kiro/settings/mcp.json` in the project root:

``` json
{
  "mcpServers": {
    "unison": {
      "command": "<path-to-ucm>",
      "args": ["mcp"]
    }
  }
}
```

**Global scope** (available in all projects):

``` bash
kiro-cli mcp add --name unison --command <path-to-ucm> --args mcp --scope global
```

If you're using a custom agent (e.g. defined in `~/.kiro/agents/my-agent.json`), workspace and global MCP servers are not automatically included. Instead, add the same `mcpServers` entry above directly to the agent's JSON config file.

After saving, restart Kiro CLI. You can verify the server is loaded with `/mcp` in chat.

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

By default, your coding assistant will automatically use the Unison MCP server when it deems it appropriate, however 
you'll get much better results with additional prompting. You may wish to start with the prompts [in this repository](https://github.com/unisoncomputing/unison-llm-support/tree/main), in particular this [main prompt](https://github.com/unisoncomputing/unison-llm-support/blob/main/instructions.md) which delegates to one of several "modes" depending on the situation, with clear instructions for each.

There are also some prompts available in the MCP server itself (for instance, there is a Unison language guide). 
If you're using Claude Desktop, you can find these prompts by clicking the "plus" icon next to the prompt input box, and then
choosing `Add from unison` and selecting the appropriate prompt.
