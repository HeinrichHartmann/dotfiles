# local tmux
function lmux {
    title=${1:-lmux}
    iterm2-title-set $title
    iterm2-tab-color-set 192 192 192 # grey
    # The default-command causes the session name to be "reattach to
    # user namespace". Hence we override this name if we create a
    # new session
    # tmux new-session -s main -A
    tmux attach -t $title 2> /dev/null ||
      (cd $HOME; tmux new-session -s $title \; rename-window "$title-home")
}

# remote tmux
function rmux {
    iterm2-title-set "$1"
    iterm2-tab-color-set 135 206 250 # light blue
    title=${2:-main}
    ssh "$1" -t "tmux new-session -s $title -A"
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
    # clear panes if necessary
    if [[ $(tmux list-panes | wc -l) -gt 1 ]]
    then
      tmux kill-pane -a
    fi
    panewidth=$(tmux display -p '#{pane_width}')
    midwidth=${1:-120}
    fringe=$(dc -e "$panewidth $midwidth - 2 / p")
    tmux split-window $@ -h -d -l $fringe 'cat > /dev/null'
    tmux split-window $@ -h -d -b -l $fringe 'cat > /dev/null'
}
