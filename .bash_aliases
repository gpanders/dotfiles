#!/bin/bash

alias l='ls -lh'
alias ll='ls -Alh'

alias g='git'

if command -v nvim >/dev/null; then
    alias vi='nvim'
fi

mkdcd() {
    mkdir -p "$1" && cd "$1" || return 1
}
