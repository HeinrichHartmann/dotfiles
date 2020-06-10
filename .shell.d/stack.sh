function newest () {
  (
    set -e
    builtin cd $1
    realpath "$(ls -rt -1 . | tail -n1)"
  )
}

function stack-ls () {
  ls -t ~/Stack | head -n 3 | nl
}

function stack-pop() {
  FILE="$(newest ~/Stack)"
  mv "$FILE" ./
  printf "%s\n" "$(basename "$FILE")"
}

function stack-push() {
  if [[ -f "$1" ]]
  then
    mv "$1" ~/Stack
  else
    printf "Not found:%s\n" "$1"
  fi
}

function stack-open() {
  open "$(newest ~/Stack)"
}

function stack-prune() {
  (
    set -e
    gfind ~/Stack -mtime +30
    printf "\nDelete? y/n\n"
    read OK
    if [[ "$OK" = y ]]
    then
      gfind ~/Stack -mtime +30 -delete
    else
      printf "aborted\n"
    fi
  )
}
