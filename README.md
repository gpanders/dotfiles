# dotfiles

This repository contains configuration files for a variety of programs.

## Bootstrapping

    git --git-dir=$HOME/.dotfiles init
    git --git-dir=$HOME/.dotfiles remote add -f origin git@git.sr.ht:~gpanders/dotfiles
    git --git-dir=$HOME/.dotfiles sparse-checkout set '/*' '!LICENSE' '!README.md'
    git --git-dir=$HOME/.dotfiles config status.showUntrackedFiles no
    git --git-dir=$HOME/.dotfiles --work-tree=$HOME checkout -f master
