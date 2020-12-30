#!/bin/bash
#: Title : Get Brew
#: Date : 2020-12-29
#: Author : "Heinrich Hartmann" <heinrich@heinrichhartmann.com>
#: Version : 0.0.1
#: Description : Installs Brew

set -o errexit
set -o nounset
set -o pipefail
set -o noclobber

mkdir -p ~/.brew/
cd ~/.brew/
git clone --depth 1 https://github.com/Homebrew/brew homebrew

# brew puts files relative to the symlinked location, don't ln -s this to ~/bin
mkdir -p ~/.brew/bin
ln -s ~/.brew/homebrew/bin/brew ~/.brew/bin/brew

~/.brew/bin/brew update
