## Some things I wish I'd known about Github Actions

You can't have an `env:` key defined in terms of another `env` key, but

You can't define a `matrix` at the top level, it has to be defined within a `job`'s `strategy`.

Windows doesn't seem to honor the `default: run: shell:` setting, so you need to set the `shell:` on `run:` manually?

Don't hesitate to do a lot with `run:` blocks aka bash scripts — at least bash is mature and well documented.

e.g.
    echo "bar=whatever" >> $GITHUB_OUTPUT
    # access with `steps.<name>.outputs.bar` in yaml strings

    echo "foo=whatever" >> $GITHUB_ENV
    # access with `env.foo` in yaml strings, or `$foo` in bash


Default Environment Variables:
https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables

When using the `cache` action, getting a cache hit on the primary key means you won't update the cache with any changes.

Workflow syntax:
https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions
