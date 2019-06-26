#!/bin/bash

shopt -s extglob

curr_dir=$(pwd)

install() {
    if [[ "$OSTYPE" == darwin* ]]; then
        brew install "$@"
    elif [[ "$OSTYPE" == linux-gnu ]]; then
        if hash apt-get 2>/dev/null; then
            sudo apt-get install -y "$@"
        elif hash yum 2>/dev/null; then
            sudo yum install "$@"
        elif hash pacman 2>/dev/null; then
            sudo pacman -S "$@"
        fi
    fi
}

if ! hash stow 2>/dev/null; then
    install stow
fi

ARGS="$@"
if [ $# -eq 0 ]; then
    ARGS="vim neovim emacs git X alacritty bash zsh pylint flake8 pandoc ranger"
    if hash tmux 2>/dev/null; then
        ARGS="$ARGS tmux"
    else
        read -r -p "Install tmux? [y/N] " ans
        if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
            install tmux
            ARGS="$ARGS tmux"
        fi
    fi

    if hash i3 2>/dev/null; then
        ARGS="$ARGS i3 conky"
    else
        if [[ "$OSTYPE" == linux-gnu ]]; then
            read -r -p "Install i3? [y/N] " ans
            if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
                install i3wm conky
                ARGS="$ARGS i3 conky"
            fi
        fi
    fi

    if hash mutt 2>/dev/null || hash neomutt 2>/dev/null; then
        ARGS="$ARGS mutt"
    fi

    if hash offlineimap 2>/dev/null; then
        ARGS="$ARGS offlineimap"
    fi

    if ! (hash mutt 2>/dev/null || hash neomutt 2>/dev/null) || ! hash offlineimap 2>/dev/null; then
        read -r -p "Install email tools? [y/N] " ans
        if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then

            if ! hash mutt 2>/dev/null && ! hash neomutt 2>/dev/null; then
                install neomutt

                # Symlink `mutt` to `neomutt`
                if [ ! -s "$(dirname $(which neomutt))/mutt" ]; then
                    pushd "$(dirname $(which neomutt))"
                    sudo ln -s neomutt mutt
                    sudo chown --reference=neomutt mutt
                    popd
                fi
                ARGS="$ARGS mutt"
            fi

            if ! hash offlineimap 2>/dev/null; then
                install offlineimap
                ARGS="$ARGS offlineimap"
            fi

            if ! hash urlscan 2>/dev/null; then
                install urlscan
            fi

            if ! hash w3m 2>/dev/null; then
                install w3m
            fi
        fi
    fi

    if hash weechat 2>/dev/null; then
        ARGS="$ARGS weechat"
    else
        read -r -p "Install weechat? [y/N] " ans
        if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
            install weechat
            ARGS="$ARGS weechat"
        fi
    fi

    if [ ! -d "$HOME/.pyenv" ]; then
        read -r -p "Install pyenv? [y/N] " ans
        if [[ "$ans" =~ ^([Yy]|[Yy][Ee][Ss])+$ ]]; then
            if [[ "$OSTYPE" == darwin* ]]; then
                brew install pyenv pyenv-virtualenv
            elif [[ "$OSTYPE" == linux-gnu ]]; then
                git clone https://github.com/pyenv/pyenv.git "$HOME/.pyenv"
                git clone https://github.com/pyenv/pyenv-virtualenv.git "$HOME/.pyenv/plugins/pyenv-virtualenv"
            fi
        fi
    fi
fi

for mod in $ARGS; do
    if [ -d "$mod" ]; then
        echo "Installing symlinks for $mod"
        stow -t "$HOME" "$mod"
    fi

    # Handle special cases
    if [[ "$mod" == "tmux" ]]; then
        if hash tmux 2>/dev/null; then
            # Install tmux plugins in a background session
            git submodule update --init
            tmux new-session -s install_plugins -d "tmux run-shell $HOME/.tmux/plugins/tpm/bindings/install_plugins"
        fi

        if ! infocmp tmux-256color &>/dev/null; then
            tic -x terminfo/tmux-256color.txt
        fi
    elif [[ "$mod" == "git" ]]; then
        # Configure git
        if [ ! -f "$HOME/.gitconfig" ] && [ ! -f "${XDG_CONFIG_HOME:-$HOME/.config}/git/config" ]; then
            touch "${XDG_CONFIG_DIR:-$HOME/.config}/git/config"
        fi

        if ! git config --global --get user.name 1>/dev/null ; then
            git config --global user.name "Greg Anders"
        fi

        if ! git config --global --get user.email 1>/dev/null ; then
            read -r -p "Git email address [greg@gpanders.com]: " ans
            git config --global user.email "${ans:-greg@gpanders.com}"
        fi

        # This repository should always use my personal email
        git config user.email greg@gpanders.com

        git config --global commit.verbose true
        git config --global init.templatedir "$HOME/.config/git/template"
        git config --global alias.a add
        git config --global alias.b branch
        git config --global alias.cm commit
        git config --global alias.co checkout
        git config --global alias.ctags "!.git/hooks/ctags"
        git config --global alias.d diff
        git config --global alias.f fetch
        git config --global alias.l log
        git config --global alias.m merge
        git config --global alias.p push
        git config --global alias.re reset
        git config --global alias.st status
        git config --global alias.snapshot "!git stash && git stash apply -q"
        git config --global alias.t tag

        # Alias for just the dotfiles repo
        git config alias.update "!git stash && git fetch origin master:master && git rebase master && git stash pop"

        # Set sendmail settings for git
        git config --global sendemail.smtpEncryption tls
        git config --global sendemail.smtpServer mail.gandi.net
        git config --global sendemail.smtpUser greg@gpanders.com
        git config --global sendemail.smtpServerPort 587
    elif [[ "$mod" == "zsh" ]]; then
        if ! hash antibody 2>/dev/null; then
            echo "Downloading antibody. This may require your password."
            curl -L git.io/antibody | sh -s
        fi
    fi
done

echo " "
echo "Setup complete."
