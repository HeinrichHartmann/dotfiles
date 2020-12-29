mkdir -p scratch && cd scratch
[ -e LuaJIT-2.0.4.tar.gz ] || wget http://luajit.org/download/LuaJIT-2.0.4.tar.gz && \
tar -xzvf LuaJIT-2.0.4.tar.gz && cd LuaJIT-2.0.4
make install PREFIX=$HOME INSTALL_TSYMNAME=luajit-shj
