dotcmd = bin/dotcmd

.PHONY:submodule-init
submodule-init:
	git submodule update --recursive --init

full: basic zsh emacs

basic:
	$(dotcmd) checkout bin
	$(dotcmd) checkout .profile
	$(dotcmd) checkout .allrc
	$(dotcmd) checkout .shell.d

zsh:
	$(dotcmd) checkout .zshrc
	$(dotcmd) checkout .oh-my-zsh


emacs:
	$(dotcmd) checkout .emacs.d
