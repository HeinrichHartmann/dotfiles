[[ -e .circenv ]]  && source .circenv

case "$HOST_CLASS" in
    DEV*)
        PS1='\e[1;34m[CIRC/DEV/\h]\e[m \u:\w\n\$ '
        PROMPT='[CIRC/DEV/%m] %n:%/'$'\n''$ '
        ;;
    PROD*)
        PS1='\e[1;31m[CIRC/PROD/\h]\e[m \u:\w\n\$ '
        PROMPT='[CIRC/PROD/%m] %n:%/'$'\n''$ '
        ;;
    *)
        PS1='[?] \u@\h:\w\$ '
        PROMPT='[?] %n@%m:%/$ '
        ;;
esac
