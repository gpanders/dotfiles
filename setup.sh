#!/bin/sh

uname="$(uname)"

installed() {
    command -v "$1" >/dev/null
}

install() {
    case $uname in
        Darwin)
            brew install "$@"
            ;;
        Linux)
            if installed apt-get; then
                sudo apt-get install -y "$@"
            elif installed yum; then
                sudo yum install "$@"
            elif installed pacman; then
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

if ! installed stow; then
    install stow
fi

ARGS="$*"

if [ $# -eq 0 ]; then
    ARGS="vim neovim git bash pylint flake8"

    if [ "$uname" = "Linux" ]; then
        ARGS="$ARGS X"
    fi

    if [ "$uname" = "Darwin" ]; then
        ARGS="$ARGS alacritty"
    fi

    if installed ranger; then
        ARGS="$ARGS ranger"
    fi

    for mod in ctags fish khard khal pandoc tmux vdirsyncer weechat zsh; do
        if installed $mod || (ask "Install $mod?" && install $mod); then
            ARGS="$ARGS $mod"
        fi
    done

    if ! installed fzf && ask "Install fzf?"; then
        install fzf
    fi

    if ! installed nnn && ask "Install nnn?"; then
        install nnn
    fi

    if installed i3 || ([ "$uname" = Linux ] && ask "Install i3?" && install i3wm polybar feh rofi compton); then
        ARGS="$ARGS i3 polybar rofi compton"
    fi

    if ! (installed mutt || installed neomutt) || ! installed mbsync; then
        if ask "Install email tools?"; then
            if ! installed mutt && ! installed neomutt; then
                install neomutt

                # Symlink `mutt` to `neomutt`
                if [ ! -s "$(dirname "$(command -v neomutt)")/mutt" ]; then
                    cd "$(dirname "$(command -v neomutt)")" || exit 1
                    sudo ln -s neomutt mutt
                    sudo chown --reference=neomutt mutt
                    cd "$OLDPWD" || exit 1
                fi
            fi

            if ! installed mbsync; then
                install isync
            fi

            if ! installed urlview; then
                install urlview
            fi

            if ! installed w3m; then
                install w3m
            fi
        fi
    fi

    if installed mutt || installed neomutt; then
        ARGS="$ARGS mutt"
    fi

    if installed offlineimap; then
        ARGS="$ARGS offlineimap"
    fi

    if installed isync || installed mbsync; then
        ARGS="$ARGS isync"
    fi

    if installed conky; then
        ARGS="$ARGS conky"
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
            if installed tmux; then
                # Install tmux plugins in a background session
                git submodule update --init
                tmux new-session -s install_plugins -d "tmux run-shell $HOME/.tmux/plugins/tpm/bindings/install_plugins"
            fi

            if ! infocmp tmux-256color >/dev/null 2>&1; then
                tic -x tmux/.local/share/tmux/terminfo/tmux-256color
            fi
            ;;
        git)
            # This repository should always use my personal email
            git config --local user.email greg@gpanders.com

            # Aliases for just the dotfiles repo
            git config --local alias.update "!git fetch origin master:master && git rebase --autostash master"
            git config --local alias.update-master '!git stash && git checkout master && git cherry-pick @{-1} && git push && git checkout - && git rebase master && git stash pop'
            ;;
        zsh)
            if ! installed antibody; then
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
