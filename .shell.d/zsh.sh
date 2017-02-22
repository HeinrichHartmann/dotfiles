if [[ "$SHELL" = /bin/zsh ]]
then

function zsh-insert-date {
    LBUFFER+="$(date +%F)"
}

# create command
zle -N zsh-insert-date

# Bind it to ESC-.
bindkey "^X." zsh-insert-date

fi
