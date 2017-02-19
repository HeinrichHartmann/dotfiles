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

emacs-install:
	wget https://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz && \
	tar -xzf emacs-25.1.tar.gz && \
	autogen.sh && \
	./configure && \
	make && \
	sudo make install
