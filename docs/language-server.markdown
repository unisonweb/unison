# Unison Language Server

[![asciicast](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0.svg)](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0)

## Overview

Supported features:

* Autocompletion
* Inline type and parser error messages
* Show type on hover

Notes:

* The LSP listens for changes from the UCM it's linked to, so name resolution is dependent on your current UCM path.

## Installation and setup

Currently the only supported configuration is to connect to the LSP via a specified port, not all LSP implementations support this configuration.

By default the LSP is hosted at `127.0.0.1:5757`, but you can change the port using `UNISON_LSP_PORT=1234`.

Note for Windows users: Due to an outstanding issue with GHC's IO manager on Windows, the LSP is **disabled by default** on Windows machines.
Enabling the LSP on windows can cause UCM to hang on exit and may require the process to be killed by the operating system or via Ctrl-C.
Note that this doesn't pose any risk of codebase corruption or cause any known issues, it's simply an annoyance.

If you accept this annoyance, you can enable the LSP server on Windows by exporting the `UNISON_LSP_ENABLED=true` environment variable. 

You can set this persistently in powershell using:

```powershell
[System.Environment]::SetEnvironmentVariable('UNISON_LSP_ENABLED','true')
```

See [this issue](https://github.com/unisonweb/unison/issues/3487) for more details.

### NeoVim

Before configuring the LSP, install the Vim plugin for filetype detection and syntax highlighting.
For [Packer](https://github.com/wbthomason/packer.nvim) you can install the package as follow:

```lua
-- You may need to increase the git clone timeout setting in Packer!
use {
  "unisonweb/unison",
  branch = "trunk",
  rtp = "/editor-support/vim"
}
```

or [Plug](https://github.com/junegunn/vim-plug):

```vim
Plug 'unisonweb/unison', { 'branch': 'trunk', 'rtp': 'editor-support/vim' }
```

Configuration for [coc-nvim](https://github.com/neoclide/coc.nvim), enter the following in the relevant place of your CocConfig

```
  "languageserver": {
    "unison": {
      "filetypes": ["unison"],
      "host": "127.0.0.1",
      "port": 5757,
      "settings": {}
    }
  }
```

For [lspconfig](https://github.com/neovim/nvim-lspconfig), you can use the following setup function:

```lua
require('lspconfig').unison.setup({})
```

Note that you'll need to start UCM _before_ you try connecting to it in your editor or your editor might give up.

### VSCode

Simply install the [Unison Language VSCode extension](https://marketplace.visualstudio.com/items?itemName=unison-lang.unison).

### Helix Editor

To `~/.config/helix/languages.toml` append this code:

```toml
[[language]]
name = "unison"
scope = "source.unison"
injection-regex = "unison"
file-types = ["u"]
shebangs = []
roots = []
auto-format = false
comment-token = "--"
indent = { tab-width = 4, unit = "    " }
language-server = { command = "ncat", args = ["localhost", "5757"] }
```

or follow the instructions for Unison in "[How to install the default language servers](https://github.com/helix-editor/helix/wiki/How-to-install-the-default-language-servers#unison)" wiki page.


### Other Editors

If your editor provides a mechanism for connecting to a host and port, provide a host of `127.0.0.1` and port `5757`.

If your editor requires a command to run, you can provide the command `nc localhost 5757` on Mac, or `netcat localhost 5757` on linux.
Note that some editors require passing the command and arguments as separate parameters.

## Configuration

Supported settings and their defaults. See information for your language server client about where to provide these.

```json
{
  // The number of completions the server should collect and send based on a single query.
  // Increasing this limit will provide more completion results, but at the cost of being slower to respond.
  // If explicitly set to `null` the server will return ALL completions available.
  "maxCompletions": 100
}
```
