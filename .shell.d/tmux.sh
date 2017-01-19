function lmux { # local tmux
    tmux new-session -s main -A
}

function rmux { # remote tmux
    ssh "$@" -t "tmux new-session -s main -A"
}

export DISABLE_AUTO_TILE=true

function tman {
    tmux split-window -h "man $@"
}

function tmux-man-last {
    last_line=$(fc -ln -1)
    last_cmd=${last_line%% *}
    tmux split-window -h "man $last_cmd"
}
