#!/bin/bash
set -ex

ucm=$(stack exec -- which unison)
echo $ucm

base_codebase=${XDG_CACHE_HOME:-"$HOME/.cache"}/unisonlanguage/base.unison

if [ ! -d $base_codebase ]; then
    echo !!!! Creating a codebase in $base_codebase
    $ucm transcript -S $base_codebase unison-src/builtin-tests/base.md
fi

dir=${XDG_DATA_HOME:-"$HOME/.local/share"}/unisonlanguage/scheme-libs
echo $dir

mkdir -p $dir
cp -r scheme-libs/* $dir/

echo $ucm transcript.fork -c $base_codebase unison-src/builtin-tests/interpreter-tests.md
time $ucm transcript.fork -c $base_codebase unison-src/builtin-tests/interpreter-tests.md

