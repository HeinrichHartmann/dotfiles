#!/bin/sh

opts=""
opts="$opts --alternate-editor=''" # try to connect to an existing server
opts="$opts --create-frame"        # don't connect to an existing frame
# opts="$opts --socket-name=~/.emacs.d/server.socket" # not needed?!

emacsclient $opts $*
