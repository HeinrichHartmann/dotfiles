if [[ -e $HOME/.nix-profile ]]
then

    [ -f /export/home/hhartmann/.nix-profile/etc/profile.d/nix.sh ]  && \
        source /export/home/hhartmann/.nix-profile/etc/profile.d/nix.sh

fi
