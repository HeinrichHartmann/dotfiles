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

emacs-install:
	wget https://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz && \
	tar -xzf emacs-25.1.tar.gz && \
	autogen.sh && \
	./configure && \
	make && \
	sudo make install
