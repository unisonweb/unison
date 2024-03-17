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


### Cache
When using the `cache` action, getting a cache hit on the primary key means you won't update the cache with any changes.

When picking a key, you have to ask, "Which key, if exactly matched, would mean that I'm already so done that I don't even want to save anything new from this run."

Similarly, `save-always: true` only if a key hit means there will be nothing new to save, even if a previous run failed AND a failed result is worth starting with.

Backup restore keys: "Is there a prior run that would be worth starting out from? With the caveat that any irrelevant garbage it includes will be saved into this run too."

### Reusability

Github supports splitting off "reusable workflows" (`jobs` that can be imported into another workflow), and "composite actions" (multi-step `steps` that can be imported into another `job`).

#### Composite actions

Needs to have `shell:` specified on every `run:`

### Reference

Default Environment Variables:
https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables

Workflow syntax:
https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions

Reusable workflows:
https://docs.github.com/en/actions/using-workflows/reusing-workflows

Composite actions:
https://docs.github.com/en/actions/creating-actions/creating-a-composite-action
