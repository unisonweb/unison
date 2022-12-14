# Unison Language Server

[![asciicast](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0.svg)](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0)

* [Overview](#overview)
* [Installation and setup](#installation-and-setup)
  * [Settings](#settings)
  * [NeoVim](#neovim)
  * [VSCode](#vscode)

## Overview

Supported features:

* Autocompletion
* Inline type and parser error messages
* Format on save
* Show type on hover

Notes:

* The LSP listens for changes from the UCM it's linked to, so name resolution is dependent on your current UCM path.

## Installation and setup

Currently the only supported configuration is to connect to the LSP via a specified port, not all LSP implementations support this configuration.

By default the LSP is hosted at `127.0.0.1:5757`, but you can change the port using `UNISON_LSP_PORT=1234`.

### Settings

Supported settings and their defaults. See information for your language server client about where to provide these.

```json
{
  // A suggestion for the formatter about how wide (in columns) to print definitions.
  "formattingWidth": 80,
  // Whether the formatter should rewrite your names for you, typically reducing them to their smallest unambiguous suffix,
  // and introducing 'use' statements when necessary for disambiguoation.
  "rewriteNamesOnFormat": true
}
```

### NeoVim

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

Note that you'll need to start UCM _before_ you try connecting to it in your editor or your editor might give up.

### VSCode

Simply install the [Unison Language VSCode extension](https://marketplace.visualstudio.com/items?itemName=unison-lang.unison).


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
