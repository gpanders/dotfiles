# Greg Anders (gpanders)'s ZSH configuration <https://github.com/gpanders/dotfiles.git>

# Enable completion (this needs to be done before sourcing plugins)
autoload -Uz compinit && compinit

# Install zsh plugins with antibody
if [ ! -f "${ZDOTDIR:-$HOME}"/.zplugins ]; then
  echo "Installing zsh plugins. We only need to do this once."
  antibody bundle > "${ZDOTDIR:-$HOME}"/.zplugins << EOF
    zsh-users/zsh-syntax-highlighting
    zsh-users/zsh-completions
    zsh-users/zsh-autosuggestions
    zsh-users/zsh-history-substring-search
    robbyrussell/oh-my-zsh path:plugins/fzf
    sorin-ionescu/prezto path:modules/gnu-utility
    mafredri/zsh-async
    sindresorhus/pure
EOF
fi
source "${ZDOTDIR:-$HOME}"/.zplugins

# Enable edit-command-line
autoload -U edit-command-line
zle -N edit-command-line

# Load some awesome zsh functions
autoload -U zmv zargs zed

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

# Set up other keybindings
bindkey "^ " autosuggest-accept
bindkey "^P" history-substring-search-up
bindkey "^N" history-substring-search-down
bindkey "^[[A" history-substring-search-up
bindkey "^[[B" history-substring-search-down
bindkey -M vicmd "k" history-substring-search-up
bindkey -M vicmd "j" history-substring-search-down
bindkey "^Q" push-line
bindkey "^XE" edit-command-line
bindkey "^X^E" edit-command-line

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

# Configure fzf
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf ]; then
  source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf
fi

# Source aliases
if [ -f "${ZDOTDIR:-$HOME}"/.zaliases ]; then
  source "${ZDOTDIR:-$HOME}"/.zaliases
fi

# Autoload custom functions
if [ -d "${ZDOTDIR:-$HOME}"/.zfunctions ]; then
  autoload -U "${ZDOTDIR:-$HOME}"/.zfunctions/**/*
fi

# Remove duplicates in path variables
typeset -gU path fpath cdpath manpath
