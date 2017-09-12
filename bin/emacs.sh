#!/bin/sh

opts=""
opts="$opts --alternate-editor=" # start emacs if not running
opts="$opts --create-frame"      # always create a new frame
opts="$opts --tty"               # start in a tty. Use emacsclient directly for a graphical session
# opts="$opts --socket-name=~/.emacs.d/server.socket" # not needed?!

TERM=xterm-256color emacsclient $opts $*
