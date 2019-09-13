# Greg Anders (gpanders)'s ZSH configuration <https://github.com/gpanders/dotfiles.git>

# Add user functions to $fpath
fpath=(~/.local/share/zsh/functions $fpath)

# Enable completion (this needs to be done before sourcing plugins)
autoload -Uz compinit && compinit

# Install zsh plugins with antibody
if [ ! -f "${ZDOTDIR:-$HOME}"/.zplugins ]; then
  echo "Installing zsh plugins. We only need to do this once."
  antibody bundle > "${ZDOTDIR:-$HOME}"/.zplugins << EOF
    zsh-users/zsh-syntax-highlighting
    zsh-users/zsh-autosuggestions
    sorin-ionescu/prezto path:modules/gnu-utility
    mafredri/zsh-async
    sindresorhus/pure
EOF
fi
source "${ZDOTDIR:-$HOME}"/.zplugins

# Autoload all shell functions from all directories in $fpath (following
# symlinks) that have the executable bit on (the executable bit is not
# necessary, but gives you an easy way to stop the autoloading of a particular
# shell function). $fpath should not be empty for this to work.
for func in $^fpath/*(N-.x:t); autoload -U $func

# Enable edit-command-line
autoload -U edit-command-line
zle -N edit-command-line

# Enable menu completion
zstyle ':completion:*' menu select

# zsh-autosuggestions
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

# History settings
HISTFILE="${ZDOTDIR:-$HOME}"/.zhistory
HISTSIZE=10000
SAVEHIST=10000

# Make cd push the old directory onto the directory stack
setopt AUTO_PUSHD

# Do not print the directory stack after pushd or popd
setopt PUSHD_SILENT

# Have pushd with no arguments act like `pushd $HOME'
setopt PUSHD_TO_HOME

# Don't push multiple copies of the same directory onto the directory stack
setopt PUSHD_IGNORE_DUPS

# Allow for extended glob patterns
setopt EXTENDED_GLOB

# Disable flow control
setopt NO_FLOW_CONTROL

# Try to correct the spelling of commands
setopt CORRECT

# Don't beep on errors in ZLE
setopt NO_BEEP

setopt NO_COMPLETE_ALIASES

setopt SHARE_HISTORY

# Split words on slashes (useful for paths)
WORDCHARS=${WORDCHARS/\/}

# Use standard readline keys in vi mode
bindkey -M viins "^A" vi-beginning-of-line
bindkey -M viins "^B" vi-backward-char
bindkey -M viins "^D" vi-delete-char
bindkey -M viins "^E" vi-end-of-line
bindkey -M viins "^F" vi-forward-char
bindkey -M viins "^K" vi-kill-eol
bindkey -M viins "^U" kill-whole-line

# Accept the current autosuggestion
bindkey "^ " autosuggest-accept

# Perform history expansion and insert a space
bindkey " " magic-space

# Add the current line to the stack and start a new command line
bindkey "^Q" push-line

# Edit the current command line in $EDITOR
bindkey "^XE" edit-command-line
bindkey "^X^E" edit-command-line

bindkey "^P" up-line-or-search
bindkey "^N" down-line-or-search
bindkey "^[[A" up-line-or-search
bindkey "^[[B" down-line-or-search
bindkey -M vicmd "k" up-line-or-search
bindkey -M vicmd "j" down-line-or-search

# Disable flow control
if (( $+commands[stty] )); then
  stty -ixon
fi

# Enable dircolors
if (( $+commands[dircolors] )); then
  if [ -f "$HOME"/.dircolors ]; then
    eval "$(dircolors $HOME/.dircolors)"
  else
    eval "$(dircolors --sh)"
  fi
fi

# Setup nvm
# Use lazy loading since nvm takes so dang long to load
if [ -s "$HOME"/.nvm ]; then
  export NVM_DIR="$HOME"/.nvm
  function nvm {
    if [ -s "$NVM_DIR"/nvm.sh ]; then
      source "$NVM_DIR"/nvm.sh
      nvm use system
      nvm $@
    fi
  }
fi

# Export GPG TTY
export GPG_TTY=$(tty)

# Source aliases
if [ -f "${ZDOTDIR:-$HOME}"/.zaliases ]; then
  source "${ZDOTDIR:-$HOME}"/.zaliases
fi

# Remove duplicates in path variables
typeset -gU path fpath cdpath manpath

# Set grep colors
export GREP_COLOR="37;45"
export GREP_COLORS="mt=${GREP_COLOR}"

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS="-F -g -i -M -R -S -w -X -z-4"

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# Colorize man page output
export LESS_TERMCAP_mb=$(printf "\e[1;31m")
export LESS_TERMCAP_md=$(printf "\e[1;31m")
export LESS_TERMCAP_me=$(printf "\e[0m")
export LESS_TERMCAP_se=$(printf "\e[0m")
export LESS_TERMCAP_so=$(printf "\e[1;44;33m")
export LESS_TERMCAP_ue=$(printf "\e[0m")
export LESS_TERMCAP_us=$(printf "\e[1;32m")
