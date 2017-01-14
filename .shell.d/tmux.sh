function lmux { # local tmux
    tmux new-session -s main -A
}

function rmux { # remote tmux
    ssh "$@" -t "tmux new-session -s main -A"
}

export DISABLE_AUTO_TILE=true
