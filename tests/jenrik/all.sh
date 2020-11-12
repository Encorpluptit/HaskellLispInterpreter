#!/usr/bin/env bash


JENRIK="jenrik"
JENRIK_PATH="tests/$JENRIK"
BINARY="hal"

## Jenrik Setup
set -e
if [[ `pwd` != *"$JENRIK_PATH" ]]; then
    echo "Going to jenrik's directory"
    cd $JENRIK_PATH
fi

if [[ ! -f $BINARY ]]; then
    echo "Building $BINARY"
    cd ../..
    make &> /dev/null
    mv $BINARY $JENRIK_PATH
    cd $JENRIK_PATH
fi

if [[ `cat .setup` != `pwd` ]]; then
    echo "Installing Jenrik's dependencies"
    pip3 install -r requirements.txt
    echo `pwd` > .setup
fi
set +e

## Jenrik Runs
### Vars
declare -A FAILURES
TOTAL=0
OK=0

### functions
function handleSuiteResult() {
    # Stats
    res="`echo "$1" | tail -n 2`"
    ok="`echo "$res" | head -n 1 | sed 's/ :.*//g'`"
    ko="`echo "$res" | tail -n 1 | sed 's/ :.*//g'`"
    total="$(($ok + $ko))"
    ((TOTAL+=total))
    ((OK+=ok))

    # Storing tests failure
    FAILURES["${suite/\.toml/}"]="`echo "$1" | head -n -4 | grep KO | sed 's/ :.*:/:/g'`"

    if [ ! -z ko ]; then
        echo -e "[\e[1;91m$suite\e[0m]: $ok/$total"
    else
        echo -e "[\e[1;92m$suite\e[0m]: $ok/$total"
    fi
}

### Run
echo "Running Jenrik"
echo "     ---"
for suite in *.toml; do
    handleSuiteResult "`./$JENRIK $suite`"
done

### Summary
echo "     ---"
echo "Summary: $OK/$TOTAL"

read -r -p "Do you want to see failures? [y/N] " response
if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
    for i in "${!FAILURES[@]}"
    do
        echo -e "\e[1m$i.toml\e[0m"
        echo -e "${FAILURES[$i]}\n"
    done
fi

rm $BINARY
