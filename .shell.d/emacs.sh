function xemacsc {
  if [[ -z "$*" ]]
  then
    set -- "$(pwd)" # add to $@
  fi
  set -- "--create-frame" "$@"
  # opts="$opts --alternate-editor= " # start emacs if not running
  # opts="$opts --tty"               # start in a tty. Use emacsclient directly for a graphical session
  # opts="$opts --socket-name=~/.emacs.d/server.socket" # specify socket
  # Apparently eavl is needed to get the arguments separate properly 2018-07-05 zsh@5.2/OSX
  (
    export TERM=xterm-256color
    emacsclient "$@"
  )
}


function emacsc {
  opts=""
  # opts="$opts --alternate-editor= " # start emacs if not running
  opts="$opts --create-frame"      # always create a new frame
  opts="$opts --tty"               # start in a tty. Use emacsclient directly for a graphical session
  # opts="$opts --socket-name=~/.emacs.d/server.socket" # specify socket
  if [[ -z "$*" ]]
  then
    opts="$opts $(pwd)"
  fi
  # Apparently eavl is needed to get the arguments separate properly 2018-07-05 zsh@5.2/OSX
  eval "TERM=xterm-256color emacsclient $opts $@"
}

alias magit='emacs $(pwd) --eval "(magit-status)"'
alias calc='/usr/local/bin/emacs -q -nw -f full-calc'
