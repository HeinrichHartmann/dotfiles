dotcmd = bin/dotcmd

.PHONY:submodule-init
submodule-init:
	git submodule update --recursive --init

full: basic zsh bash emacs
	$(dotcmd) checkout .tmux.conf

basic:
	$(dotcmd) checkout bin
	$(dotcmd) checkout .shell.d
	$(dotcmd) checkout .allrc

bash:
	$(dotcmd) checkout .bashrc

zsh:
	$(dotcmd) checkout .zshrc
	$(dotcmd) checkout .oh-my-zsh

emacs:
	$(dotcmd) checkout .emacs.d
