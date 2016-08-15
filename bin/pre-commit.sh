#!/bin/sh

if [ $1 = "install" ]; then
   ln -s `pwd`/bin/pre-commit.sh .git/hooks/pre-commit
else
    git stash -q --keep-index
    ./bin/checks.sh
    RESULT=$?
    git stash pop -q
    [ $RESULT -ne 0 ] && exit 1
    exit 0
fi
