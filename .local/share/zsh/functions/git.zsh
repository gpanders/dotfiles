if [ "$PWD" = "$HOME" ]; then
    command git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
else
    command git "$@"
fi
