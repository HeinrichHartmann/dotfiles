#
# Todo.txt
#

# todo.txt CLI
alias T=todo.sh


alias TD=topydo

TODO_DIR="$HOME/Dropbox/todo/"
alias tde="emacs -q -nw todo.txt"
alias td="$HOME/git/todo.txt-cli/todo.sh -d $HOME/git/todo.txt-cli/todo.cfg"
# alias td=topydo



function tda {
    printf "%s" "Added: "
    topydo add $@
    printf "%s\n" "---"
    topydo ls
}

function tdd {
    topydo do $@
    printf "%s\n" "---"
    topydo ls
}

export tdd
export tda
