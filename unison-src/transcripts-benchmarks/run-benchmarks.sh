#!/bin/bash

ucm="${1:-unison}"

scratchpad=$(mktemp)
for transcript in $(find . -name '*.md' -type f ! -name '*.output.md'); do
    /usr/bin/time time -lp "${ucm}" transcript "$transcript" 2>"$scratchpad"

    # mac
    # peakMemory=$(awk '/peak memory/{ print $1 }' "$scratchpad")
    # realTime=$(awk '/real/{ print $2; exit }' "$scratchpad")
    # userTime=$(awk '/user/{ print $2; exit }' "$scratchpad")
    # sysTime=$(awk '/sys/{ print $2; exit }' "$scratchpad")

    # Linux
    peakMemory=$(awk '/Maximum resident set/{ print $NF }' "$scratchpad")
    realTime=$(awk '/wall clock/{ print $NF; exit }' "$scratchpad")
    userTime=$(awk '/User time/{ print $NF; exit }' "$scratchpad")
    sysTime=$(awk '/System time/{ print $NF; exit }' "$scratchpad")

    cat >"$transcript.json" <<EOF
    { "peakMemory": $peakMemory
    , "realTime": "$realTime"
    , "userTime": $userTime
    , "sysTime": $sysTime
    }
EOF
done
