# local tmux
function lmux {
    title=${1:-lmux}
    shift
    iterm2-title-set "$title"
    iterm2-tab-color-set 192 192 192 # grey
    # The default-command causes the session name to be "reattach to
    # user namespace". Hence we override this name if we create a
    # new session
    {
      cd ${DIR:-$HOME}; # new windows will be opened in here
      # try creating a new session group (-t)
      tmux new-session -t $title $@ || {
        # create new session if
        tmux new-session -s $title $@
        tmux rename-window "$title-home"
      }
    }
}

# remote tmux
function rmux {
    iterm2-title-set "RMUX $1"
    iterm2-tab-color-set 135 206 250 # light blue
    title=${2:-main}
    ssh "$1" -t "tmux new-session -s $title -A"
}


# vagrant tmux
function vmux {
  iterm2-title-set "VMUX $1"
  iterm2-tab-color-set 135 206 250 # light blue
  title=${2:-main}
  vagrant ssh "$1" -- -t "tmux new-session -s $title -A"
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

function tmux-clear-window {
  # remove panes if necessary
  if [[ $(tmux list-panes | wc -l) -gt 1 ]]
  then
    tmux kill-pane -a
  fi
}

# centers current tmux window
function tmux-center {
  tmux-clear-window
  panewidth=$(tmux display -p '#{pane_width}')
  midwidth=${1:-120}
  fringe=$(dc -e "$panewidth $midwidth - 2 / p")
  tmux split-window $@ -h -d -l $fringe 'cat > /dev/null'
  tmux split-window $@ -h -d -b -l $fringe 'cat > /dev/null'
}

function wp-blog {
    tmux-center &&
    cd ~/p-workbench/HeinrichHartmann.github.io/ &&
    emacs .
}

function wp-stats {
  tmux-clear-window
  # split-window
  # -d -- don't move to new plane
  # -l <size>
  # -v/-h -- vertical/horizontal
  tmux split-window -d -v -l 10 # console downstairs
  tmux split-window -d -h # create right window
  tmux select-pane -t 3 # select console
  tmux send-keys -t 1 'htop' 'C-m'
  tmux send-keys -t 2 'tail -f /var/log/*.log' 'C-m'
}
