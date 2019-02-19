# Set browser based on OS type
if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER="open"
elif [[ "$OSTYPE" == linux-gnu ]]; then
  export BROWSER="xdg-open"
fi

# Editor and pager
export EDITOR="vim"
export VISUAL="view"
export PAGER="less"

if (( $+commands[nvim] )); then
  export EDITOR="nvim"
  export VISUAL="nvim"
fi

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

# Setup pyenv
if [[ -s "$HOME/.pyenv/bin/pyenv" ]]; then
  export PYENV_ROOT="$HOME/.pyenv"
  path=("$PYENV_ROOT/bin" $path)
fi

if (( $+commands[pyenv] )); then
  if [[ -z "$PYENV_ROOT" ]]; then
    export PYENV_ROOT=$(pyenv root)
  fi

  eval "$(pyenv init - --no-rehash zsh)"
fi

# Setup rbenv
if (( $+commands[rbenv] )); then
  eval "$(rbenv init - --no-rehash zsh)"
fi

# Setup cargo
if [[ -s "$HOME/.cargo/env" ]]; then
  source "$HOME/.cargo/env"
fi

# Remove duplicates in path variables
typeset -gU path fpath cdpath manpath

#
# Grep
#
export GREP_COLOR="37;45"
export GREP_COLORS="mt=${GREP_COLOR}"

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS="-F -g -i -M -R -S -w -X -z-4"

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

(( ! ${+LESS_TERMCAP_mb} )) && export LESS_TERMCAP_mb=$'\E[1;31m'   # Begins blinking.
(( ! ${+LESS_TERMCAP_md} )) && export LESS_TERMCAP_md=$'\E[1;31m'   # Begins bold.
(( ! ${+LESS_TERMCAP_me} )) && export LESS_TERMCAP_me=$'\E[0m'      # Ends mode.
(( ! ${+LESS_TERMCAP_se} )) && export LESS_TERMCAP_se=$'\E[0m'      # Ends standout-mode.
(( ! ${+LESS_TERMCAP_so} )) && export LESS_TERMCAP_so=$'\E[7m'      # Begins standout-mode.
(( ! ${+LESS_TERMCAP_ue} )) && export LESS_TERMCAP_ue=$'\E[0m'      # Ends underline.
(( ! ${+LESS_TERMCAP_us} )) && export LESS_TERMCAP_us=$'\E[1;32m'   # Begins underline.
