cd ~/
mkdir -p ~/Applications
git clone --depth 1  https://github.com/Homebrew/brew ~/Applications/brew
ln -s  ~/Applications/brew/bin/brew ~/bin
brew update
