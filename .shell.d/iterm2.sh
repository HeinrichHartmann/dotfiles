iterm2-set-profile() {
    echo -e "\033]50;SetProfile=$1\a"
}

iterm2-set-title() {
    echo -ne "\033]0;"$*"\007"
}
