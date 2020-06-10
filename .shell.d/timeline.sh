TL_FILE=~/Notes/timeline.org

function tl-commit {
  cd "$(dirname $TL_FILE)"
  git add "$TL_FILE"
  git commit -m "."
  git --no-pager show HEAD
}

function tl-add {
  HEAD="$1"; shift
  BODY="$*"
  printf "\n* %s %s\n%s\n" $(date +%Y-%m-%d) "$HEAD" "$BODY" >> "$TL_FILE"
  tl-commit
}

function tl-edit {
  emacs -nw -q "$TL_FILE"
  tl-commit
}

function tl-cat {
  cat $TL_FILE
}
