# If not running interactively, don't do anything
[[ $- == *i* ]] || return

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will match all
# files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ -n "$SSH_TTY" ]; then
    PS1="\[\e[32;1m\]\u@\h\[\e[34;1m\] \w \$\[\e[0m\] "
else
    PS1="\[\e[32;1m\]\u\[\e[34;1m\] \w \$\[\e[0m\] "
fi

if command -v dircolors >/dev/null 2>&1; then
    if [ -r ~/.dir_colors ]; then
        eval "$(dircolors -b ~/.dir_colors)"
    elif [ -r ~/.dircolors ]; then
        eval "$(dircolors -b ~/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
fi

if [[ "$OSTYPE" == darwin* ]]; then
    export LSCOLORS="ExGxcxdxCxegedabagacad"
    alias ls="ls -G"
elif [[ "$OSTYPE" == linux-gnu* ]]; then
    alias ls="ls --color=auto"
fi

alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

# colored GCC warnings and errors
export GCC_COLORS="error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01"

# add bash completion sources from /usr/local if they exist
if [ -d /usr/local/etc/bash_completion.d/ ]; then
    export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d/"
fi

# enable programmable completion features (you don't need to enable this, if
# it's already enabled in /etc/bash.bashrc and /etc/profile sources
# /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /usr/local/etc/profile.d/bash_completion.sh ]; then
        . /usr/local/etc/profile.d/bash_completion.sh
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable fzf
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf ]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf
fi
