TEMPLATE_DIR="$HOME/var/proj/templates"

function mkproj {
  DIR="$(gdate -I)-$*"
  mkdir "$DIR"
  cd "$DIR"
}

function proj-init-python {
  [ -d venv ] || virtualenv -p python3 venv
  # don't clobber existing files
  rsync --ignore-existing -av "$TEMPLATE_DIR/python/" ./
}

function proj-template-add {
  DIR="$TEMPLATE_DIR/$1"
  shift
  [ -d "$DIR" ] || {
    printf "Create %s? [y/n] " "$DIR"
    read -q input
    if [[ $input = "y" ]];
    then
      mkdir -p "$DIR"
    else
      return 1
    fi
  }
  gcp -v -t "$DIR" "$@"
}
