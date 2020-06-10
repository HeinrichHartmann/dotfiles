#
# Modifications of GNU Utils
#

function dirstack {
  builtin dirs -v | while read nr line; do echo "$nr: $line"; done
}

# Change directory, print dirstack and most recent files
function cd()
{
  local dir=$1
  if test -n "$dir"     ## non-empty string?
  then
    pushd "$dir" > /dev/null
  else
    pushd "$HOME"
  fi # 1>&2 2>/dev/null   ## do not show DIRSTACK listing and error
  if test $? -ne 0      ## pushd failed?
  then
    builtin cd "$dir" ## let cd provide error message
  else                  ## print content
    printf "Dirstack:\n"
    dirstack
    printf "\n"
    #printf "Contents:\n"
    # ls
    #ls -lt | tail -n+2 | head -n 5 | while read line; do echo "* $line"; done
    #printf "\n"
  fi
}

# list all directories
function ls-d {
  ls -l $@ | grep ^d | cat
}

function f {
  find . | grep $@
}

function ls-csv {
  {
    printf '%s\t' T mtime owner perm size # header row
    printf '%s\n' name
    gfind . -maxdepth 1 -mindepth 1 -name ".*" -prune -o -printf \
          '%Y\t%TY-%Tm-%Td\t%u:%g\t%m\t%s\t%P\n'
  }
}

function L {
  csvls | csvsort -t -c 1,2 | csvlook
}
