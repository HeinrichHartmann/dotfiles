# https://askubuntu.com/questions/33774/how-do-i-remap-the-caps-lock-and-ctrl-keys
sudo sed -i 's/XKBOPTIONS=.*/XKBOPTIONS="ctrl:nocaps"/g' /etc/default/keyboard

# Set 3 second timeout in grub-menu so we can select snapshots to boot into
sudo sed 's/GRUB_TIMEOUT_STYLE=.*/GRUB_TIMEOUT_STYLE=menu/' -i /etc/default/grub
sudo sed 's/GRUB_TIMEOUT=.*/GRUB_TIMEOUT=3/' -i /etc/default/grub
sudo update-grub

sudo apt-get update
sudo apt-get dist-update

# Essentials
sudo apt-get -y install \
     curl wget git zsh \
     jove \
     fonts-hack-ttf \
     smbclient \
     make build-essential manpages-dev

# Desktop
sudo apt-get install xubuntu-desktop
sudo apt-get install --no-install-recommends firefox

sudo chsh hhartmann -s /usr/bin/zsh
