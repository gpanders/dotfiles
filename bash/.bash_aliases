alias l="ls -lh"
alias ll="ls -Alh"

alias g="git"

if command -v nvim >/dev/null; then
    alias vi="nvim"
    alias vim="nvim"
fi

if [ -n "$EDITOR" ]; then
    alias e="$EDITOR"
fi

if [ -n "$BROWSER" ]; then
    alias b="$BROWSER"
fi

alias d="dirs -v"

mkdcd() {
    mkdir -p "$1" && cd "$1"
}

__cd() {
    builtin cd "$@"
    if [ $? -eq 0 ]; then
        pushd -n "$(pwd)" >/dev/null
    fi
}
alias cd=__cd

for i in $(seq 1 9)
do
    alias "$i"="builtin cd ~$i"
done

alias tmux="tmux -f ${XDG_CONFIG_HOME:-$HOME/.config}/tmux/tmux.conf"
