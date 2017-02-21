dotcmd = bin/dotcmd

.PHONY:submodule-init
submodule-init:
	git submodule update --recursive --init

full: basic emacs

basic: submodule-init
	$(dotcmd) checkout .bashrc
	$(dotcmd) checkout .zshrc
	$(dotcmd) checkout .oh-my-zsh
	$(dotcmd) checkout .allrc
	$(dotcmd) checkout .shell.d
	$(dotcmd) checkout bin
	$(dotcmd) checkout .tmux.conf

emacs:
	$(dotcmd) checkout .emacs.d

emacs-install:
	[ -e emacs-25.1.tar.gz ] || wget https://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz && \
	tar -xzf emacs-25.1.tar.gz && \
	cd emacs-25.1 && \
	autogen.sh && \
	./configure && \
	make && \
	sudo make install
