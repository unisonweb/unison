#!/bin/bash

getHash() {
    input=$1

    # Remove the initial '@' and split the string by '/'
    IFS='/' read -r -a parts <<< "${input:1}"

    user="${parts[0]}"
    project="${parts[1]}"
    type="" # Can be either 'branch' or 'release'
    name="" # Will hold either branch name or release version

    # Check if it's a release or a branch
    if [[ "${parts[2]}" == "releases" ]]; then
        type="release"
        name="${parts[3]}"
    else
        type="branch"
        # Reconstruct branch name from the remaining parts
        for (( i = 2; i < ${#parts[@]}; i++ )); do
            if [[ -z "$name" ]]; then
                name="${parts[i]}"
            else
                name="$name/${parts[i]}"
            fi
        done
    fi
    # echo "user: $user, project: $project, type: $type, name: $name"

    if [[ "$type" == "branch" ]]; then
        url=https://api.unison-lang.org/users/${user}/projects/${project}/branches/${name}
        hash=$(curl -s $url | jq -r '.causalHash')
    else
        url=https://api.unison-lang.org/users/${user}/projects/${project}/releases/${name}
        hash=$(curl -s $url | jq -r '.causalHashUnsquashed')
    fi
    # echo "url: $url, hash: $hash"
    echo "$hash"
}

# print usage if there's not exactly 1 argument
if [ $# -ne 1 ]; then
    # print to the EOF
    cat <<EOF
Example usage:
$0 @user/project/branchname
$0 @user/project/releases/1.0.0
EOF
    exit 1
fi

getHash "$1"
