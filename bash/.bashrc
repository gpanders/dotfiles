# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Set variables to configure git prompt
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"

prompt_command() {
    [[ $? -eq 0 ]] && color="35" || color="31"
    PS1="${debian_chroot:+($debian_chroot)}\[\033[34m\]\w\[\033[90m\]"
    if type __git_ps1 >/dev/null 2>&1; then
        PS1="$PS1$(__git_ps1 " %s")"
    fi
    PS1="$PS1\n${SSH_TTY:+\h }\[\033[${color}m\]\$ \[\033[00m\]"
}
PROMPT_COMMAND=prompt_command

# enable color support of ls
if command -v gdircolors >/dev/null 2>&1; then
    alias dircolors=gdircolors
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
elif [[ "$OSTYPE" == linux-gnu ]]; then
    alias ls="ls --color=auto"
fi
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

# colored GCC warnings and errors
export GCC_COLORS="error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -d /usr/local/etc/bash_completion.d/ ]; then
    export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d/"
fi

if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /usr/local/etc/profile.d/bash_completion.sh ]; then
        . /usr/local/etc/profile.d/bash_completion.sh
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# enable fzf
if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf ]; then
    source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf
fi

export LESS="-g -M -R -W -X -z-4"

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
