# Editor and pager
export EDITOR="vim"
export VISUAL="view"
export PAGER="less"

# Use neovim if available
if (( $+commands[nvim] )); then
  export EDITOR="nvim"
  export VISUAL="nvim"
fi

# Set browser based on OS type
if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER="open"
elif [[ "$OSTYPE" == linux-gnu ]]; then
  export BROWSER="xdg-open"
fi

source "${ZDOTDIR:-$HOME}"/.zpath
