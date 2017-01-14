# _NODES are physical hosts
export SNOWTH_TEST_NODE=HH.DS # host in dev, that can be used to run scripts remotely
export SNOWTH_REF_NODE=S12.DS
export SNOWTH_DEV_NODE=S10.DS

# w/o _NODE, the service is meant, including a port
export SNOWTH_REF=${SNOWTH_REF_NODE}:8112 # this should be a production copy
export SNOWTH_DEV=${SNOWTH_DEV_NODE}:8112 # here is the current checkout

function circ-setup-profile {
    printf "Updating .bash_profile\n"
    scp ~/.profile_circonus "$1:~/.profile"
    echo 'source ~/.profile' | ssh $@ 'cat > ~/.bash_profile'
}

function circ-setup-profile-dev {
    circ-setup-profile $@
    echo 'HOST_CLASS="DEV"' | ssh $@ 'cat > ~/.circenv'
}

function circ-setup-profile-prod {
    circ-setup-profile $@
    echo 'CIRCONUS_HOST_CLASS="PROD"' | ssh $@ 'cat > ~/.circonus-env'
}

function circ-setup-tmux {
    printf "Updating tmux.conf\n"
    cat "$HOME/.tmux.conf" | ssh "$@" 'cat > .tmux.conf && touch .tmux.local.conf'
}

function circ-setup-emacs {
    printf "Syncing emacs"
    rsync -v .emacs $1:~/
    rsync -rv .emacs.d $1:~/
}

function circ-setup-hosts {
    if ssh "$@" "cat /etc/hosts" | grep -q 'CIRCONUS'
    then
        printf "/etc/hosts already updated"
    else
        printf 'Updating /etc/hosts\n'
        sed -n '/# CIRCONUS START/,/# CIRCONUS END/p' /etc/hosts \
            | ssh "$@" 'sudo tee -a /etc/hosts'
    fi
}

function circ-setup-all {
    circ-setup-profile "$1"
    circ-setup-tmux "$1"
    # circ-setup-hosts "$1"
    # circ-setup-emacs "$1"
}
