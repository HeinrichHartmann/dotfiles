#!/bin/sh

opts=""
opts="$opts --alternate-editor=" # start emacs if not running
opts="$opts --create-frame"      # don't connect to an existing frame
# opts="$opts --socket-name=~/.emacs.d/server.socket" # not needed?!

TERM=xterm-256color emacsclient $opts $*
