alias l="ls -lFh"
alias ll="ls -AlFh"

if hash nvim 2>/dev/null; then
  alias vi="nvim"
  alias vim="nvim"
fi

if [ ! -z "$EDITOR" ]; then
  alias e="$EDITOR"
fi

if [ ! -z "$BROWSER" ]; then
  alias b="$BROWSER"
fi

alias open="xdg-open"
alias d="dirs -v"

function mkdcd {
  mkdir -p "$1" && cd "$1"
}

for ((i=1; i <= 9; i++))
do
  alias "$i"="cd +${i}"
done

alias tmux="tmux -2"
