#!/usr/bin/env bash


JENRIK="jenrik"
JENRIK_PATH="tests/$JENRIK"
BINARY="hal"

## Setup
set -e
if [[ `pwd` != *"$JENRIK_PATH" ]]; then
    cd $JENRIK_PATH
fi

if [[ ! -f $BINARY ]]; then
    cd ../..
    make
    mv $BINARY $JENRIK_PATH
    cd $JENRIK_PATH
fi

if [[ ! -f .setup ]]; then
    pip3 install -r requirements.txt
    touch .setup
fi
set +e

## Run
for suite in *.toml; do
    echo -e "\nRunning: [\e[1m$suite\e[0m]"
    ./$JENRIK $suite
done

rm $BINARY
