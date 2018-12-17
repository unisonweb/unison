#!/usr/bin/env bash

until (stack build && \
       (cd runtime-jvm/; bloop compile main) && \
       stack exec unison "$1")
do
  echo "Well that didn't work."
  echo Press any key to re-launch.
  read -n 1
done
