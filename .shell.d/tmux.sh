# local tmux
function lmux {
    # The default-command causes the session name to be "reattach to
    # user namespace". Hence we override this name if we create a
    # new session
    # tmux new-session -s main -A
    tmux attach -t main 2> /dev/null ||
        (cd $HOME; tmux new-session -s main \; rename-window "home")
}

# remote tmux
function rmux {
    ssh "$@" -t "tmux new-session -s main -A"
}

export DISABLE_AUTO_TILE=true

# open man page in separate tmux window
function tman {
    tmux split-window -h "man $@"
}

function tmux-man-last {
    last_line=$(fc -ln -1)
    last_cmd=${last_line%% *}
    tmux split-window -h "man $last_cmd"
}

# centers current tmux window
function tmux-center {
    panewidth=$(tmux display -p '#{pane_width}')
    midwidth=${1:-120}
    shift
    fringe=$(dc -e "$panewidth $midwidth - 2 / p")
    tmux split-window $@ -h -d -l $fringe 'cat > /dev/null'
    tmux split-window $@ -h -d -b -l $fringe 'cat > /dev/null'
}
