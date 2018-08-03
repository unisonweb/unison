#!/bin/bash
# Run the following command in the root of your project to install this pre-push hook:
# cp git-hooks/pre-push .git/hooks/pre-push; chmod 700 .git/hooks/pre-push

# Check if we actually have commits to push
commits=`git log @{u}..`
if [ -z "$commits" ]; then
    exit 0
fi

CMD="./scripts/test.sh"
eval $CMD
RESULT=$?
if [ $RESULT -ne 0 ]; then
    echo "The git push operation was canceled because \`$CMD\` did not complete successfully."
    exit 1
fi
exit 0
