# Configuration


<!-- toc GFM -->

* [UCM Configuration](#ucm-configuration)
  * [Environment variables](#environment-variables)
    * [`UNISON_DEBUG`](#unison_debug)
    * [`UNISON_PAGER`](#unison_pager)
    * [`UNISON_LSP_PORT`](#unison_lsp_port)
    * [`UNISON_SHARE_HOST`](#unison_share_host)
    * [`UNISON_SHARE_ACCESS_TOKEN`](#unison_share_access_token)
    * [Local Codebase Server](#local-codebase-server)
* [Codebase Configuration](#codebase-configuration)

<!-- tocstop -->

## UCM Configuration

### Environment variables

#### `UNISON_DEBUG`

Enable debugging output for various portions of the application. 
See `lib/unison-prelude/src/Unison/Debug.hs` for the full list of supported flags.

E.g.

```sh
# Enable ALL debugging flags (likely quite noisy)
$ UNISON_DEBUG= ucm
# Enable timing debugging, printing how long different actions take.
$ UNISON_DEBUG=TIMING ucm
# Enable LSP and TIMING debugging
$ UNISON_DEBUG=LSP,TIMING ucm
```

#### `UNISON_PAGER`

Allows selecting which pager to use for long command outputs.
Defaults to `less` on Linux & Mac, `more` on Windows

E.g.

```sh
# User more instead of less
$ UNISON_PAGER=more ucm
```

#### `UNISON_LSP_PORT`

Allows selecting the port to run the LSP server on. Defaults to `5757`.

E.g.

```sh
$ UNISON_LSP_PORT=8080 ucm
```

#### `UNISON_SHARE_HOST`

Allows selecting the location for the default Share server.

E.g.

```sh
$ UNISON_SHARE_HOST="http://localhost:5424" ucm
```

#### `UNISON_SHARE_ACCESS_TOKEN`

Allows overriding the credentials used when authenticating with the Share server.

E.g.

```sh
$ UNISON_SHARE_ACCESS_TOKEN="my.token.string" ucm
```

#### Local Codebase Server

The port, host and token to be used for the local codebase server can all be configured by providing environment
variables when starting `ucm`, using `UCM_PORT`, `UCM_HOST`, and `UCM_TOKEN`.

E.g.

```sh
UCM_PORT=8080 UCM_HOST=localhost UCM_TOKEN=1234 ucm
```

## Codebase Configuration

Also, see the guide [here](https://www.unison-lang.org/learn/tooling/configuration/)

The following configuration options can be provided within the `.unisonConfig` file,
which exists within the codebase directory, or at `~/.unisonConfig` for your default codebase.

```
# Attach myself as author and use BSD license for all of my contributions
DefaultMetadata = [ ".metadata.authors.chrispenner"
                  , ".metadata.licenses.chrispenner" ]

# RemoteMapping allows mapping a path in the codebase to a specific location on share.
# Here I state that I want my .share namespace to push to .chrispenner.public
# Everything inside .share will be mapped accordingly, e.g. .share.foo will map to
# chrispenner.public.foo on share.
RemoteMapping {
  share = "chrispenner.public"
}
```
