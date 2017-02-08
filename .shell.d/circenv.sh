HOSTENV="$HOME/.env"

[[ -e $HOSTENV ]]  && source $HOSTENV

case "$HOST_CLASS" in
    DEV*)
        PS1='\e[1;34m[CIRC/DEV/\h]\e[m \u:\w\n\$ '
        PROMPT='[CIRC/DEV/%m] %n:%/'$'\n''$ '
        ;;
    PROD*)
        PS1='\e[1;31m[CIRC/PROD/\h]\e[m \u:\w\n\$ '
        PROMPT='[CIRC/PROD/%m] %n:%/'$'\n''$ '
        ;;
    PRIV*)
        ;;
    *)
        PS1='[?] \u@\h:\w\$ '
        PROMPT='[?] %n@%m:%/$ '
        ;;
esac

function set_env {
    echo "HOST_CLASS=$1" >> "$HOSTENV"
    echo "HOST_CLASS set to $1.\nRestart shell for changes to take effect."
}
