# dotfiles

This repository contains configuration files for a variety of programs.

    export GIT_DIR=$HOME/.dotfiles
    export GIT_WORK_TREE=$HOME
    git init
    git remote add -f origin https://codeberg.org/gpanders/dotfiles
    git checkout -f master
    git sparse-checkout set --no-cone '/*' '!LICENSE' '!README.md'
    echo '*' >> $HOME/.dotfiles/info/exclude
