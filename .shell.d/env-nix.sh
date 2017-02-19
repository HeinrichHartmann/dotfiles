if [[ ! -e $HOME/.nix-profile ]]
then
    exit 0
fi

. /export/home/hhartmann/.nix-profile/etc/profile.d/nix.sh
