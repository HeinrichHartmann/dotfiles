dotcmd = bin/dotcmd

basic: submodule-init tmux zsh bash bin

full: basic emacs

.PHONY:submodule-init
submodule-init:
	git submodule update --recursive --init

bin:
	$(dotcmd) checkout bin

shell:
	$(dotcmd) checkout .allrc
	$(dotcmd) checkout .zshrc
	$(dotcmd) checkout .oh-my-zsh
	$(dotcmd) checkout .bashrc
	$(dotcmd) checkout .shell.d

tmux:
	$(dotcmd) checkout .tmux.conf

clean:
	rm -r scratch


setup:
	mkdir -p ~/workbench/src
	mkdir -p ~/workbench/proj
