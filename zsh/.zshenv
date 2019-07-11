# Set LANG
if [[ -z "$LANG" ]]; then
  export LANG="en_US.UTF-8"
fi

# Set up PATH on non-login top-level shells
if [[ $SHLVL == 1 && ! -o LOGIN ]]; then
    source "${ZDOTDIR:-$HOME}"/.zpath
fi
