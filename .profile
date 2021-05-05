#!/bin/sh

export EDITOR=vi
export VISUAL=vi
export PAGER=less
export LESS="-F -g -M -R -W -X -z-4"

if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

if [ -d "${XDG_CONFIG_HOME:-$HOME/.config}"/profile.d ]; then
    for i in "${XDG_CONFIG_HOME:-$HOME/.config}"/profile.d/*.sh; do
        [ -r "$i" ] && . "$i"
    done
fi

if [ -n "$BASH_VERSION" ] && [ -n "$PS1" ]; then
    # Source .bashrc for interactive bash shells
    [ -r ~/.bashrc ] && . ~/.bashrc
fi
