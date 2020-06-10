BOOKDIR=~/Shelf/Books

function books-index {
  (
    set -e
    cd "$BOOKDIR"
    mkdir -p .index/txt
    gfind . -path "./.index/*" -prune -o -type f -print > .index/files.txt
    # create directories for pdftotext target files
    gfind . \( -name '.index' -o -name '.git' -o -name '_*' \) -prune -o -type d -exec mkdir -p ".index/txt/{}" \;
    gfind . -path "./.index/*" -prune -o -type f -name "*.pdf" -print -exec pdftotext -l 20 "{}" ".index/txt/{}" \;
  )
}

function books-find {
  QUERY="$*"
  (
    cd "$BOOKDIR"
    grep -i "$QUERY" .index/files.txt | sed 's|^.//|'"$BOOKDIR"'/|'
  )
}

function books-search {
  QUERY="$*"
  (
    cd "$BOOKDIR"
    export BOOKDIR
    ag -C 2 --group --nonumber -i "$QUERY" -R .index/txt/ | perl -pe 's|\.index/txt(.*)|\n\n### $ENV{BOOKDIR}$1 ###\n|'
  )
}

function books-list {
  gfind . -type f -path "./.index/*" -prune -o -print
}

function books-add {
  mv "$1" "$BOOKDIR"
}
