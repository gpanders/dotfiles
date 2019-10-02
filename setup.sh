#!/bin/sh

uname="$(uname)"

install() {
    case $uname in
        Darwin)
            brew install "$@"
            ;;
        Linux)
            if command -v apt-get >/dev/null; then
                sudo apt-get install -y "$@"
            elif command -v yum >/dev/null; then
                sudo yum install "$@"
            elif command -v pacman >/dev/null; then
                sudo pacman -S "$@"
            fi
            ;;
        *)
            echo "Unknown OS" >&2
            return 1
            ;;
    esac
}

ask() {
    printf "%s [y/N] " "$1"
    read -r ans
    printf "\n"
    if echo "$ans" | grep -qE '^([Yy]|[Yy][Ee][Ss])+$'; then
        return 0
    else
        return 1
    fi
}

if ! command -v stow >/dev/null; then
    install stow
fi

ARGS="$*"

if [ $# -eq 0 ]; then
    ARGS="vim neovim emacs git X alacritty bash pylint flake8 pandoc ranger ctags khard vdirsyncer"
    if command -v tmux >/dev/null; then
        ARGS="$ARGS tmux"
    elif ask "Install tmux?"; then
        install tmux
        ARGS="$ARGS tmux"
    fi

    if command -v zsh >/dev/null; then
        ARGS="$ARGS zsh"
    elif ask "Install zsh?"; then
        install zsh
        ARGS="$ARGS zsh"
    fi

    if command -v fish >/dev/null; then
        ARGS="$ARGS fish"
    elif ask "Install fish?"; then
        install fish
        ARGS="$ARGS fish"
    fi

    if ! command -v fzy >/dev/null && ask "Install fzy?"; then
        install fzy
    fi

    if command -v i3 >/dev/null; then
        ARGS="$ARGS i3 conky"
    elif [ "$uname" = Linux ] && ask "Install i3?"; then
        install i3wm conky
        ARGS="$ARGS i3 conky"
    fi

    if command -v mutt >/dev/null || command -v neomutt >/dev/null; then
        ARGS="$ARGS mutt"
    fi

    if command -v offlineimap >/dev/null; then
        ARGS="$ARGS offlineimap"
    fi

    if command -v isync >/dev/null || command -v mbsync >/dev/null; then
        ARGS="$ARGS isync"
    fi

    if ! (command -v mutt >/dev/null || command -v neomutt >/dev/null) || ! command -v mbsync >/dev/null; then
        if ask "Install email tools?"; then
            if ! command -v mutt >/dev/null && ! command -v neomutt >/dev/null; then
                install neomutt

                # Symlink `mutt` to `neomutt`
                if [ ! -s "$(dirname "$(command -v neomutt)")/mutt" ]; then
                    cd "$(dirname "$(command -v neomutt)")" || exit 1
                    sudo ln -s neomutt mutt
                    sudo chown --reference=neomutt mutt
                    cd "$OLDPWD" || exit 1
                fi
                ARGS="$ARGS mutt"
            fi

            if ! command -v mbsync >/dev/null; then
                install isync
                ARGS="$ARGS isync"
            fi

            if ! command -v urlscan >/dev/null; then
                install urlscan
            fi

            if ! command -v w3m >/dev/null; then
                install w3m
            fi
        fi
    fi

    if command -v weechat >/dev/null; then
        ARGS="$ARGS weechat"
    elif ask "Install weechat?"; then
        install weechat
        ARGS="$ARGS weechat"
    fi
fi

for mod in $ARGS; do
    if [ -d "$mod" ]; then
        echo "Installing symlinks for $mod"
        stow -t "$HOME" "$mod"
    fi

    # Handle special cases
    case $mod in
        tmux)
            if command -v tmux >/dev/null; then
                # Install tmux plugins in a background session
                git submodule update --init
                tmux new-session -s install_plugins -d "tmux run-shell $HOME/.tmux/plugins/tpm/bindings/install_plugins"
            fi

            if ! infocmp tmux-256color >/dev/null 2>&1; then
                tic -x terminfo/tmux-256color.txt
            fi
            ;;
        git)
            # Configure git
            if [ ! -f "$HOME/.gitconfig" ] && [ ! -f "${XDG_CONFIG_HOME:-$HOME/.config}/git/config" ]; then
                touch "${XDG_CONFIG_DIR:-$HOME/.config}/git/config"
            fi

            if ! git config --global --get user.name >/dev/null ; then
                git config --global user.name "Greg Anders"
            fi

            if ! git config --global --get user.email >/dev/null ; then
                printf "Git email address [greg@gpanders.com]: "
                read -r ans
                printf "\n"
                git config --global user.email "${ans:-greg@gpanders.com}"
            fi

            # This repository should always use my personal email
            git config user.email greg@gpanders.com

            git config --global commit.verbose true
            git config --global rebase.autoSquash true
            git config --global grep.lineNumber true
            git config --global grep.patternType extended
            git config --global alias.a add
            git config --global alias.b branch
            git config --global alias.cm commit
            git config --global alias.co checkout
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
            git config alias.update "!git fetch origin master:master && git rebase --autostash master"

            # Set sendmail settings for git
            git config --global sendemail.smtpEncryption tls
            git config --global sendemail.smtpServer mail.gandi.net
            git config --global sendemail.smtpUser greg@gpanders.com
            git config --global sendemail.smtpServerPort 587
            ;;
        zsh)
            if ! command -v antibody >/dev/null; then
                if [ "$uname" = Darwin ]; then
                    brew install getantibody/tap/antibody
                else
                    mkdir -p "$HOME"/.local
                    curl -sfL git.io/antibody | sh -s - -b "$HOME"/.local/bin
                fi
            fi
            ;;
    esac
done

echo " "
echo "Setup complete."
