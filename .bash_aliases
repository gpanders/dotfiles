alias l="ls -lh"
alias ll="ls -Alh"

alias g="git"

git() {
    if [ "$PWD" = "$HOME" ]; then
        command git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" "$@"
    else
        command git "$@"
    fi
}

if command -v nvim >/dev/null; then
    alias vi="nvim"
    alias vim="nvim"
fi

if [ -n "$EDITOR" ]; then
    alias e="\$EDITOR"
fi

if [ -n "$BROWSER" ]; then
    alias b="\$BROWSER"
fi

alias d="dirs -v"

mkdcd() {
    mkdir -p "$1" && cd "$1" || return 1
}

__cd() {
    builtin cd "$@" && pushd -n "$(pwd)" >/dev/null
}
alias cd=__cd

for i in $(seq 1 9)
do
    alias "$i"="builtin cd ~$i"
done

alias tmux="tmux -f ${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.conf"
alias weechat="weechat -d ${XDG_CONFIG_HOME:-$HOME/.config}/weechat"
