
function pdf-collate {
    pdftk A="$1" B="$2" shuffle A Bend-1S output -
}

alias pdf-merge="pdfunite"

function pdf-split {
    pdftk "$1" burst output "${1%%.pdf}_%02d.pdf"
}

function pdf-render-txt {
    cat $1 | iconv -f utf8 -t latin1 | enscript -B -p - | ps2pdf - - > ${1%%.txt}.pdf
}
