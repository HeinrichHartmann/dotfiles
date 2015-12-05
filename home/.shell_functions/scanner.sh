#
# scanner pdf handling
#

DOC_ROOT="$HOME/Documents"

function scan_collate {
    pdftk A="$1" B="$2" shuffle A Bend-1S output "$3"
}

alias scan_merge="pdfunite"

function scan_get {
    A=`newest ~/scanner/`
    B="$DOC_ROOT/`date +%F`_$1.pdf"
    mv "$A" "$B"
    echo "Moved $A to $B"
}

function archive {
    B="$DOC_ROOT/`date +%F`_$1"
    cp "$1" "$B"
    echo "Archived $1 to $B"
    ls -l "$B"
}

function stamp {
    mv "$1" "`date +%F`_$1"
}
