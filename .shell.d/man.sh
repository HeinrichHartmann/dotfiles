#!/bin/bash

function man-pdf {
    fname="$(mktemp).$1.pdf"
    man -Tpdf "$1" > "$fname" || \
        man -t "$1" | ps2pdf - > "$fname"
    open "$fname"
}

function man-bash {
    man bash | less -p "^[[:space:]]*$1 ";
}

# get help on bash builtins
function help-bash {
    bash -c "help $@" | less
}
