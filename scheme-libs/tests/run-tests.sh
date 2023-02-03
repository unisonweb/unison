#!/bin/bash
set -ex

if [ ! -f "ucm" ]; then
    ln -s $(stack exec -- which unison) ./ucm
fi

if [ ! -d "base.unison" ]; then
    ./ucm transcript -S base.unison base.md
fi

./ucm transcript.fork -c base.unison basic.md

