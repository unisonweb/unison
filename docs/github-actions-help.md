## Some things I wish I'd known about Github Actions

You can't have an `env:` key defined in terms of another `env` key, but

You can't define a `matrix` at the top level, it has to be defined within a `job`'s `strategy`.

Windows doesn't seem to honor the `default: run: shell:` setting, so you need to set the `shell:` on `run:` manually?
