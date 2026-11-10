When the task needs something the base library doesn't have (reading or writing JSON, a web server or client, dates, a file format, an API), look for a published library on Unison Share before you write it yourself. A hand-written parser or protocol is long, easy to get wrong, and not what a maintainer would want. Libraries under the `@unison` user are the first choice.

- **Search Share.** `share.project-search json` finds projects by name and summary, and `share.search <name or type>` finds definitions (see [share](learn:ucm/share)). Try the plain word for the job (`json`, `http`, `time`, `csv`), then a few synonyms. Prefer a project under `@unison`, then one that is recent and has a README.
- **Read about it before installing.** `share.render @unison/json` prints the README, and `share.source @unison/json:Decoder.run` a definition. `share.releases @unison/json` lists its releases with dates, and `share.releases.detail 1` shows the release notes of the first.
- **Install it.** `lib.install @unison/json/releases/latest`. It goes into `lib` in the current project (see [libraries](learn:ucm/libraries)). Check `list lib` first, because it may be installed already, and never install a second copy of one you have. Installing takes minutes: put each `lib.install` in a transcript of its own, and if one times out, run it again.
- **To use a library's own dependency by name, install it too.** The types of `@unison/routes` come from `@unison/http`, in `lib.unison_routes_7_0_2.lib.unison_http_8_0_0`, and names that deep don't resolve and `find.all` doesn't reach them. `lib.install @unison/http/releases/latest` makes `HttpRequest` and the rest available, instead of a `use` of the long path.
- **Get to know it,** with its README (`share.render`, above) and documentation: `find.all <word>`, `docs <name>`, `view <name>` (see [familiarize](learn:skills/familiarize)). Names resolve by their last parts, so after `find.all Decoder` you can use `docs Decoder.run`.
- **Use it for both directions:** a JSON library reads *and* builds JSON, so build output with it too (`Json.object`, `Json.toText`) rather than joining strings with `++`; a web library has helpers for its responses. Hand-built text is the part that goes wrong (escaping, quotes).
- **Use it, then test and document your code** as usual.
- **A newer version of a library you already have** is installed and switched to with `upgrade` (see [upgrade](learn:ucm/upgrade)), which also removes the old copy. Don't edit your code by hand and leave both versions in `lib`.
- **A library that doesn't fit** (a missing feature, a different type) is a reason to adapt your code to it or to pick another library, not to copy its internals. Writing your own is a last resort when no library does the job.

## Detail

**An example,** for a task that reads JSON text into a record:

````
``` ucm
> share.project-search json
> share.render @unison/json
> lib.install @unison/json/releases/latest
> find.all Decoder
> docs Decoder.run
```
````

**Web services and servers.** Search for `http` and `routes`: a library may provide the pieces of a server (routes, a handler type, a way to test a handler without running a server), and a JSON library the request and response bodies.

**If nothing is found,** say so in your answer and write the minimum yourself, with tests.
