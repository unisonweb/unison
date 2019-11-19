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
  if
    git init --bare "$1" && \
    git clone "$1" "$1.co" && \
    pushd "$1.co" && \
    git commit --allow-empty -m"nonbare" && \
    git push && \
    popd && \
    rm -rf "$1.co"
  then
    return 0
  else
    echo "couldn't create repo '$1'"
    return 1
  fi
}

delete_local_repo () {
  rm -rf $1
}

# run_transcript <repo-to-cleanup> <transcript-to-run>
run_transcript_and_delete () {
  if stack exec unison -- transcript `dirname $0`/"$2"; then
    delete_local_repo "$1"
  else
    echo "transcript failed; I'm preserving repo '$1' for inspection."
    exit 1
  fi
}

repo="/tmp/test1.git"
if
  create_nonbare_local_repo "$repo" && \
  run_transcript_and_delete "$repo" "test-git.md"
then
  echo "non-bare test passed"
fi


if
  create_bare_local_repo "$repo" && \
  run_transcript_and_delete "$repo" "test-git.md"
then
  echo "bare test passed"
fi
