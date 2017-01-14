install:
	ln -s $$(pwd)/cmd.sh ~/bin/

setup:
	git submodule update --recursive
