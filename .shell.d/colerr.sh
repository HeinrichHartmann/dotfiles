# color stderr

function colerr {
  "$@" 2> >(sed $'s,.*,\e[31m&\e[m,')
}
