#!/usr/bin/env bash
source="$1"

assert_command_exists () {
  if ! ( type "$1" &> /dev/null ); then
    echo "Sorry, I need the '$1' command, but couldn't find it installed." >&2
    exit 1
  fi
}

assert_command_exists mktemp
assert_command_exists stack
assert_command_exists sbt
assert_command_exists scala
assert_command_exists fswatch

fswatch --batch . | \
  "`dirname $0`/execany.sh" "$source"
