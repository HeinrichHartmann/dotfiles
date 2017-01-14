#
# from  C.FA Johnson - Pro Bash Programming
#
function cd() #@ DESCRIPTION: Change directory, print most recent files
{             #@ USAGE: cd DIR
    local dir=$1
    if test -n "$dir"     ## non-empty string?
    then
        pushd "$dir" > /dev/null
    else
        pushd "$HOME"
    fi 1>&2 2>/dev/null   ## do not show DIRSTACK listing and error

    if test $? -ne 0      ## pushd failed?
    then
        builtin cd "$dir" ## let cd provide error message
    else                  ## print content
        printf "Contents:\n"
        ls
        # ls -lt | tail -n+2 | head -n 5 | while read line; do echo "* $line"; done
        printf "\n"
    fi
}
