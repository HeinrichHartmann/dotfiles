# LUA Interpreter setup

LUA_VERSION=$(lua -v 2>&1 | cut -f 2 -d " ")
LUA_PROMPT="lua ${LUA_VERSION}> "
if ($LUA -e 'require "rlcompleter"' 2> /dev/null);
then
    LUA_COMPLETER='require "rlcompleter"'
fi

export LUA_INIT="_PROMPT=\"${LUA_PROMPT}\"; ${LUA_COMPLETER}"
export LUA_PATH="$LUA_PATH;$HOME/.lua/share/?.lua;$HOME/.luarocks/share/lua/5.1/?.lua;$HOME/.luarocks/share/lua/5.1/?/init.lua"
export LUA_CPATH="$LUA_CPATH;$HOME/.luarocks/lib/lua/5.1/?.so"

alias lua="rlwrap lua"

add-to-path "$HOME/.luarocks/bin/"
