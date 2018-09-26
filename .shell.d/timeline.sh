TL_FILE=~/notes/timeline.org

function tl-commit {
  cd "$(dirname $TL_FILE)"
  git add "$TL_FILE"
  git commit -m "."
  git show HEAD
}

function tl-add {
  HEAD="$1"; shift
  BODY="$*"
  printf "\n* %s \n%s\n" $(date +%Y-%m-%d) "$HEAD" "$BODY" >> "$TL_FILE"
  tl-commit
}

function tl-cat {
  cat $TL_FILE
}

function tl-edit {
  emacsclient --create-frame --tty -e '(tl-edit "'"$TL_FILE"'")'
  tl-commit
}
