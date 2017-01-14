#!/bin/bash

USAGE="Usage: cmd.sh <sub> <opts>

* cmd.sh add <path>

  Add path to dotfile repository

* cmd.sh push

  Push commited changes to GitHub

"

set -e

cmd=$1
shift
case $cmd in
    "" | "-h" | "--help")
        echo "$USAGE"
        ;;
    "add")
        src="$1"
        name="$(basename "$src")"
        tar="./$name"
        mv -n -v "$src" "$tar"
        ln -s "$tar" "$src"
        git add "$tar"
        git commit -m "Added $name"
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
