function tl-add {
  HEAD="$1"; shift
  BODY="$*"
  printf "\n* %s %s\n" $(date +%Y-%m-%d) "$*"  >> ~/notes/timeline.org
  cd ~/notes
  git add ~/notes/timeline.org
  git commit -m "."
  git show HEAD
}

function tl-cat {
  cat ~/notes/timeline.org
}

function tl-edit {
  eval $EDITOR ~/notes/timeline.org
}
