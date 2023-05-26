#!/bin/bash
set -ex

ucm=$(stack exec -- which unison)

base_codebase=${XDG_CACHE_HOME:-"$HOME/.cache"}/unisonlanguage/base.unison

if [ ! -d $base_codebase ]; then
    # -S specificies the output codebase (-C specifies the input codebase)
    $ucm transcript -S $base_codebase unison-src/builtin-tests/base.md
fi
