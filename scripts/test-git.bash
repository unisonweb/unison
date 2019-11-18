#!/bin/bash

curl="curl -u $GITHUB_USERNAME:$GITHUB_PATOKEN"
create_github_repo () {
  $curl -v 'https://api.github.com/user/repos' -d '{"name":"'"$1"'"}' 2>&1 |\
    grep "Status: 201 Created" > /dev/null
}

delete_github_repo () {
  $curl -v -X DELETE 'https://api.github.com/repos/'$GITHUB_USERNAME'/'"$1" 2>&1 |\
    grep "Status: 204 No Content" > /dev/null
}

create_bare_local_repo () {
  git init --bare "$1"
}

create_nonbare_local_repo () {
  git init --bare "$1"
  git clone "$1" "$1.co"
  pushd "$1.co"
  git commit --allow-empty -m"nonbare"
  git push
  popd
  rm -rf "$1.co"
}

delete_local_repo () {
  rm -rf $1
}

test_nonbare () {
  if create_nonbare_local_repo "$1"; then
    if stack exec unison -- transcript `dirname $0`/test-git.md; then
      delete_local_repo "$1"
    else
      echo "transcript failed; I'm preserving repo '$1' for inspection."
    fi
  else
    echo "couldn't create repo '$1'"
    exit 1
  fi
}

test_nonbare "/tmp/test1.git"
