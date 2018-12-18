#!/usr/bin/zsh

source /usr/share/zsh/share/antigen.zsh
antigen init ~/.antigenrc

# Enable completion
autoload -Uz compinit
compinit -d $HOME/.cache/zsh/compdump

# zsh-autosuggest
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=1

# History settings
HISTFILE="$HOME/.cache/zsh/history"
HISTSIZE=10000
SAVEHIST=10000

# Perform textual history expansion, csh-style, treating the character `!`
# specially.
setopt BANG_HIST

# Don't allow duplicates in history
setopt HIST_IGNORE_DUPS

# Remove command lines from history list when the first character is a space
setopt HIST_IGNORE_SPACE

# Verify an expanded command before executing
setopt HIST_VERIFY

# Treat single word simple commands without redirection as candidates for
# resumption of an existing job
setopt AUTO_RESUME

# Allow comments starting with `#` even in interactive shells
setopt INTERACTIVE_COMMENTS

# List jobs in the long format by default
setopt LONG_LIST_JOBS

# Report the status of background jobs immediately, rather than waiting until
# just before printing a prompt
setopt NOTIFY

# Prevent running all background jobs at a lower priority
setopt NO_BG_NICE

# cd adds directories to dirstack
setopt AUTO_PUSHD pushd_silent pushd_to_home

# Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

# This reverses the +/- operators
setopt PUSHD_MINUS

# Append to history file instead of overwriting
setopt APPENDHISTORY

# Allow for extended glob patterns
setopt EXTENDEDGLOB

# Write to multiple descriptors.
setopt MULTIOS

# Don't throw an error if there is no match
unsetopt NOMATCH


# Emacs/readline style keybindings
bindkey -e

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
bindkey '^ ' autosuggest-accept

# Enable dircolors
if (( $+commands[dircolors] )); then
  eval "$(dircolors --sh)"
fi

# Source fzf config
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ -f ~/.zaliases ]; then
  source ~/.zaliases
fi

