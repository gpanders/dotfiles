# dotfiles

This repository contains configuration files for a variety of programs.

    export GIT_DIR=$HOME/.dotfiles
    export GIT_WORK_TREE=$HOME
    git init
    git remote add -f origin https://git.sr.ht/~gpanders/dotfiles
    git checkout -f master
    git sparse-checkout set '/*' '!LICENSE' '!README.md'
    git config status.showUntrackedFiles no
