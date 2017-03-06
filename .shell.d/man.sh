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

function man-zsh {
    man zshbuiltins | less -p "^[[:space:]]*$1 ";
}

# get help on bash builtins
function help-bash {
    bash -c "help $@" | less
}

function man-gnu {
    MANPATH=$(find /usr/local/Cellar -type d -name gnuman | paste -s -d : -) man $@
}

function man-last {
    last_line=$(fc -ln -1)
    last_cmd=${last_line%% *}
    man "$last_cmd"
}
