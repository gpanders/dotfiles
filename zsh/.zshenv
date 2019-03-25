# Editor and pager
export EDITOR="vim"
export VISUAL="view"
export PAGER="less"

# Use neovim if available
if (( $+commands[nvim] )); then
  export EDITOR="nvim"
  export VISUAL="nvim"
fi

# Set LANG
if [[ -z "$LANG" ]]; then
  export LANG="en_US.UTF-8"
fi

# Add /usr/local/bin to path first
path=(/usr/local/bin $path)

# Add pip binary directory to path
path=($HOME/.local/bin $path)

# Add TeX installation to path, if it exists
if [[ -d "/Library/TeX/texbin" ]]; then
  path=("/Library/TeX/texbin" $path)
fi

# Remove duplicates in path variables
typeset -gU path fpath cdpath manpath
