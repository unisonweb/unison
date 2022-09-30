# Unison Language Server

[![asciicast](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0.svg)](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0)

* [Overview](#overview)
* [Installation and setup](#installation-and-setup)
  * [Settings](#settings)
  * [NeoVim](#neovim)
  * [VSCode](#vscode)

## Overview

Supported features:

* Show type on hover
* Inline type and parser error messages
* Format on save
* NO autocomplete yet, but soon.

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

Install the [Unison Extension](https://marketplace.visualstudio.com/items?itemName=unison-lang.unison) from the extension marketplace.
