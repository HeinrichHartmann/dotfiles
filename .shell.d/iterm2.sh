iterm2-profile-set() {
    echo -e "\033]50;SetProfile=$1\a"
}

iterm2-title-set() {
    echo -ne "\033]0;"$*"\007"
}

iterm2-tab-color-set() {
    echo -ne "\033]6;1;bg;red;brightness;$1\a"
    echo -ne "\033]6;1;bg;green;brightness;$2\a"
    echo -ne "\033]6;1;bg;blue;brightness;$3\a"
}

iterm2-tab-color-reset() {
    echo -ne "\033]6;1;bg;*;default\a"
}
