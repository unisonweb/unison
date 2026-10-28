# MCP Setup

UCM comes packaged with a built-in MCP server for use with AI agents.
It has one idea: you give it a Unison [transcript](https://www.unison-lang.org/docs/tooling/transcripts/) (Markdown with `ucm` blocks of UCM commands and `unison` blocks of code), and it gives you back the transcript with UCM's output filled in after each command. Anything you can do at the UCM command line, an agent can do this way: search and read code, write and typecheck new code, add it to the codebase, install libraries, run tests, and read from Unison Share.

## Setup

### Configuring your agent

This approach allows agents to connect to UCM's MCP server directly via stdin/stdout.

Note that this causes an additional UCM to run as an entirely independent process for each agent you're using. Each one runs its transcripts in place, on the codebase it was started with (the default codebase, or the one given by `--codebase`), so changes an agent makes are really made there.

#### Claude Code, IBM Bob, Gemini CLI, and Antigravity CLI

To configure the MCP for use with Claude Code, IBM Bob, Gemini CLI, Antigravity CLI, and any tools which read the same `mcpServers` JSON config shape, edit the appropriate config JSON file:

Claude Code:
* On Mac: `~/Library/Application Support/Claude/claude_desktop_config.json`
* On Windows: `%APPDATA%\Claude\claude_desktop_config.json`
* On Linux: `$HOME/.claude.json`

IBM Bob:
* Global: `~/.bob/settings/mcp_settings.json`
* Project: `.bob/mcp.json`

Gemini CLI:
* Global: `~/.gemini/settings.json`
* Project: `.gemini/settings.json`

Antigravity CLI:
* Global: `~/.gemini/antigravity-cli/mcp_config.json`
* Project: `.agents/mcp_config.json`

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

After saving the file, restart the agent. For Claude, you should then see a new "unison" option in the MCP server list. For Bob, make sure "Use MCP Servers" is enabled in the MCP settings, then manage or restart the `unison` server from there. In Gemini CLI, Claude Code, and Antigravity CLI, check with `/mcp`.

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
    • Tools: transcript-read-only, transcript, transcript-io, transcript-all
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

## Tools

There are four tools. They all take the transcript either as `transcript` (the Markdown text) or as `transcript-file` (the path of a file that has it, relative to the directory the server was started in; the file isn't changed), and they differ only in which UCM commands they allow, so an agent can be given just as much power as you're comfortable with. A transcript that uses a command its tool doesn't allow is rejected before anything runs, with a list of what to change.

| Tool | Allows |
| --- | --- |
| `transcript-read-only` | Commands that don't change the codebase: searching, viewing, docs, diffs, history, running tests, and reading from Share. Every project and branch the transcript mentions must already exist. |
| `transcript` | The above, plus changes that are easy to undo (with `undo`, the reflog, or by deleting a branch): `update`, `merge`, creating and renaming branches and projects, `lib.install`, `pull`, deleting definitions, and so on. |
| `transcript-io` | The above, plus running code (`run`, `io.test`), which can do anything IO can. |
| `transcript-all` | Everything, including deleting projects and branches and `push`ing to Share. |

The last two are marked destructive, so agents that ask for approval before using destructive tools will ask before using them.

For anything long, `transcript-file` is better: when something goes wrong, the agent can edit just the part that needs fixing and run the file again, without spending tokens on sending the whole transcript each time. When a transcript sent inline is more than 25 lines, the reply says so.

A call that takes more than five minutes is cancelled; set `UNISON_MCP_TIMEOUT` (in seconds) in the server's environment to change that.

The reply repeats the transcript with UCM's output filled in, except that a `unison` block of more than 10 lines is replaced by a note saying which lines of the transcript it was, since the caller has just written it. Pass `echo: true` to get such blocks back. Commands are always repeated, and short blocks too.

UCM quotes the line it is complaining about with a number that counts from the start of the code block. In messages about the transcript's own `unison` blocks those numbers are rewritten to count from the start of the transcript, so they can be used directly; messages about a file loaded with `load` keep that file's numbers.

If the result is more than 25 lines, it is written to a temporary file and the tool returns the path, so the agent can search the file instead of reading all of it. The files are removed when the MCP server exits. When a transcript fails, the error is always in the reply itself; only the part of the transcript before the failure goes to the file.

### Reading from Share

Four commands read from [Unison Share](https://share.unison-lang.org) without touching your codebase. They work at the UCM command line too.

``` ucm
myproject/main> share.search List.map in @unison/base
myproject/main> share.search [a] -> Nat
myproject/main> share.project-search http
myproject/main> share.source @unison/base:List.map
myproject/main> share.render @unison/base:List.map
myproject/main> share.render @unison/base
```

`share.search` is the search box on the front page of Share: a name like `List.map` or a type like `[a] -> Nat` finds terms and types, a query starting with `@` finds users, organizations and projects, and `in @user/project` (or `in @user`) narrows a definition search. `share.project-search` finds projects by name or summary. `share.source` prints the source of a definition, and `share.render` prints its documentation, or a project's README if you give it no name. By default they look at the project's latest release; `@unison/base/releases/7.20.0:List.map` picks a release and `@unison/base/main:List.map` a branch.

### When an action can't finish

`update`, `delete`, `merge`, `pull` and `upgrade` must produce a branch that compiles. When one can't, UCM switches to a temporary branch and writes the definitions that need attention to `<action>-<shorthash>.u` (for example `merge-a0458b71.u`; the hash is of the action, its arguments and the branches involved). The reply names the file and the branch and says what to do next: edit the file until it compiles, check it with `load <file>`, then run `update` to merge the temporary branch back and delete it (or `cancel` to abandon). Deleting a definition from the file deletes it from the branch.

## Usage

By default, your coding assistant will automatically use the Unison MCP server when it deems it appropriate, however
you'll get much better results with additional prompting. You may wish to start with the prompts [in this repository](https://github.com/unisoncomputing/unison-llm-support/tree/main), in particular this [main prompt](https://github.com/unisoncomputing/unison-llm-support/blob/main/instructions.md) which delegates to one of several "modes" depending on the situation, with clear instructions for each.
