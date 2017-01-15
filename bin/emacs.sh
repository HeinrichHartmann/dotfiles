#!/bin/sh

# try to connect to an existing server
# if non exists start a new one
/usr/local/bin/emacsclient --socket-name=/Users/hartmann/.emacs.d/server/server --alternate-editor='' --create-frame $*
