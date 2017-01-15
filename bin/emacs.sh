#!/bin/sh

# try to connect to an existing server
# if non exists start a new one
emacsclient --socket-name=~/.emacs.d/server/server --alternate-editor='' --create-frame $*
