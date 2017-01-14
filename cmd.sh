#!/bin/bash

USAGE="Usage: cmd.sh <sub> <opts>

* cmd.sh add <path>

  Add path to dotfile repository

* cmd.sh push

  Push commited changes to GitHub

"

cd $(dirname $0)
set -e

function ask {
    echo "eval? " $(printf ":%s:" $@)
    read
    eval $@
}

function die {
    echo $@ >&2
    exit 1
}

function do_hoist {
    src="$1"
    name="$(basename "$src")"
    tar="./$name"
    [[ ! -e "$tar" ]] || die "File exists $tar"
    ask mv "$src" "$tar"
    ask ln -s "$tar" "$src"
    ask git add "$tar"
    ask git commit -m "Added $tar"
}

cmd=$1
shift
case $cmd in
    "" | "-h" | "--help")
        echo "$USAGE"
        ;;
    "hoist")
        args="$@"
        while [[ $# > 0 ]]
        do
            do_hoist "$1"
            shift
        done
        exit 0;
        ;;
    "restore")
        # restore file $1
        src="$1"
        tar="~/$1"
        [[ ! -h "$tar" ]] || die "Not a symlink: $tar"
        ask rm "$tar"
        ask mv -n "$src" "$tar"
        ask git add "$src"
        ask git commit -m "Removed $src"
        exit 0;
        ;;
    "push")
        git push
        exit 0;
        ;;
    "list")
        ls -lA | grep -v -e '.git' -e cmd.sh
        exit 0;
esac
