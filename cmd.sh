#!/bin/bash

DOT_CMD=$(basename $0)
echo $DOT_CMD
DOT_LOC=$(readlink $0 || echo $0)
DOT_DIR=$(dirname $DOT_LOC)

USAGE="Usage: $DOT_CMD <sub> <opts>"

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

USAGE="$USAGE
* $DOT_CMD hoist <file> [<file>...]
     add files to dot-files.
     replace file by symlink.
"
function do_hoist {
    src="$1"
    name="$(basename "$src")"
    tar="$DOT_DIR/$name"
    [[ ! -e "$tar" ]] || die "File exists $tar"
    ask mv "$src" "$tar"
    ask ln -s "$tar" "$src"
    (
        cd "$DOT_DIR"
        git add "$tar"
        git commit -m "Added $tar"
        git push
    )
}

function do_checkout {
    src="$1"
    name="$(basename "$src")"
    tar="$DOT_DIR/$name"
    [[ -e "$tar" ]] || die "Not found: $tar"
    ask ln -s "$tar" "$HOME"
}

USAGE="$USAGE
* $DOT_CMD restore <file> [<file>...]
     replace symlink in ~/ by original file
     remove file from dot-files
"
function do_restore {
    tar="$1"
    [[ -h "$tar" ]] || die "Not a symlink: $tar"
    src="$DOT_DIR/$(basename $(readlink "$1"))"
    [[ -e "$src" ]] || die "Not found: $src"
    ask rm "$tar"
    ask mv "$src" "$tar"
    (
        cd "$DOT_DIR"
        git add "$src"
        git commit -m "Removed $src"
        git push
    )
}

cmd=$1
shift
case $cmd in
    "" | "-h" | "--help")
        echo "$USAGE"
        exit 0;
        ;;
    "hoist")
        while [[ $# > 0 ]]
        do
            do_hoist "$1"
            shift
        done
        exit 0;
        ;;
    "checkout")
        while [[ $# > 0 ]]
        do
            do_checkout "$1"
            shift
        done
        exit 0;
        ;;
    "restore")
        while [[ $# > 0 ]]
        do
            do_restore "$1"
            shift
        done
        exit 0;
        ;;
esac

