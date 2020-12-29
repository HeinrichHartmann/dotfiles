brew install pyenv

# Python build dependencies
export EBIAN_FRONTEND=noninteractive TZ=Europe/Berlin
sudo apt-get -y install \
     libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev llvm libncurses5-dev \
     libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl

source ~/.shell.d/python.sh

pyenv install 3.8.6
pyenv global 3.8.6
