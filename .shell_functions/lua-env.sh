# LUA Interpreter setup
LUA=$(/bin/which lua)
LUA_VERSION=$($LUA -v 2>&1 | cut -f 2 -d " ")
LUA_PROMPT="lua ${LUA_VERSION}> "
if ($LUA -e 'require "rlcompleter"' 2> /dev/null) then
   LUA_COMPLETER='require "rlcompleter"'
fi
export LUA_INIT="_PROMPT=\"${LUA_PROMPT}\"; ${LUA_COMPLETER}"
