# Added by n-install (see http://git.io/n-install-repo).

# Use ~/.n to store node versions
export N_PREFIX="$HOME/.n"
if [[ -d $N_PREFIX ]]
then
  [[ -e $N_PREFIX ]] || mkdir -p "$N_PREFIX"
  [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"
fi

# Use ~/.npm to store npm packages
export NPM_PACKAGES="$HOME/.npm"
if [[ -d $NPM_PACKAGES ]]
then
  [[ -e "$NPM_PACKAGES" ]] || mkdir -p "$NPM_PACKAGES"
  PATH="$PATH:$NPM_PACKAGES/bin"
  MANPATH="$MANPATH:$NPM_PACKAGES/share/man"
  # Tell Node about these packages
  NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
fi
