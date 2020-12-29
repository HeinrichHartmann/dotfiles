mkdir -p scratch && cd scratch
[ -e lua-5.1.5.tar.gz ] || wget https://www.lua.org/ftp/lua-5.1.5.tar.gz
tar -xzf lua-5.1.5.tar.gz && cd lua-5.1.5 &&
make posix
mv src/lua ~/bin/lua-sh
