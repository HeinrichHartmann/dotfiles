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
	mkdir -p scratch && cd scratch && \
	[ -e emacs-25.1.tar.gz ] || wget https://ftp.gnu.org/gnu/emacs/emacs-25.1.tar.gz && \
	tar -xzf emacs-25.1.tar.gz && \
	cd emacs-25.1 && \
	./autogen.sh && \
	CFLAGS="-ggdb3 -O0" CXXFLAGS="-ggdb3 -O0" LDFLAGS="-ggdb3" \
        ./configure --prefix=$$HOME --disable-ns-self-contained --with-ns=no --with-x=no && \
	make && \
	make install

lua-sh-install: # make lua available for scripting (sh replacement)
	mkdir -p scratch && cd scratch && \
	[ -e lua-5.1.5.tar.gz ] || wget https://www.lua.org/ftp/lua-5.1.5.tar.gz && \
	tar -xzf lua-5.1.5.tar.gz && cd lua-5.1.5 && \
	make posix && \
	mv src/lua ~/bin/lua-sh

luajit-sh-install:
	mkdir -p scratch && cd scratch && \
	[ -e LuaJIT-2.0.4.tar.gz ] || wget http://luajit.org/download/LuaJIT-2.0.4.tar.gz && \
	tar -xzvf LuaJIT-2.0.4.tar.gz && cd LuaJIT-2.0.4 && \
	make install PREFIX=$HOME INSTALL_TSYMNAME=luajit-shj

clean:
	rm -r scratch
