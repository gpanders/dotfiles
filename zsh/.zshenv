# Set LANG
if [[ -z "$LANG" ]]; then
  export LANG="en_US.UTF-8"
fi

# Set base path
path=(/usr/local/bin /usr/bin /bin /usr/local/sbin /usr/sbin /sbin)

# macOS specific setup
if [[ "$OSTYPE" == darwin* ]]; then
    # Launch path_helper. This will add paths found in /etc/paths and
    # /etc/paths.d to the path, removing any duplicates. By default, this is
    # done in /etc/zprofile, but doing it in zprofile messes things up so do it
    # here instead
    if [ -x /usr/libexec/path_helper ]; then
        eval `/usr/libexec/path_helper -s`
    fi

    # Add homebrew utils to path
    path=(
        /usr/local/opt/coreutils/bin
        /usr/local/opt/findutils/bin
        /usr/local/opt/mailutils/bin
        $path
    )
fi

# Add pip binary directory to path
path=($HOME/.local/bin $path)

# Setup pyenv
if [[ -s "$HOME/.pyenv/bin/pyenv" ]]; then
  export PYENV_ROOT="$HOME/.pyenv"
  path=("$PYENV_ROOT/bin" $path)
fi

if (( $+commands[pyenv] )); then
  eval "$(pyenv init - --no-rehash zsh)"
fi

# Setup cargo
if [[ -s "$HOME/.cargo/env" ]]; then
  source "$HOME/.cargo/env"
fi

# Setup rbenv
if (( $+commands[rbenv] )); then
  eval "$(rbenv init - --no-rehash zsh)"
fi
