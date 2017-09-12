function circ-www-update {
    src="$1"; shift
    host="$1"; shift
    echo "Syncing files"
    rsync -av "$src/" "$host:/www/" --exclude logs $@
    echo "Restarting services"
    ssh "$host" -- "sudo svcadm restart http:circonus && sudo svcadm restart circapi"
}

function circ-vpn-dev {
    tmux new-session -d -s vpn
    tmux new-window -t vpn:0
    tmux rename-window -t vpn:0 "vpn-dev"
    tmux send -t vpn:0 "cd ~/Circonus/vpns/bel && sudo openvpn heinrich.hartmann-yubi.conf" ENTER
    tmux attach -t vpn:0
}

function circ-vpn-chi {
    tmux new-session -d -s vpn
    tmux new-window -t vpn:1
    tmux rename-window -t vpn:1 vpn-chicago
    tmux send -t vpn:1 "cd ~/Circonus/vpns/chicago-il2 && sudo openvpn heinrich.hartmann-yubi.conf" ENTER
    tmux attach -t vpn:1
}

function circ-vpn-kill {
    sudo pkill openvpn
}
