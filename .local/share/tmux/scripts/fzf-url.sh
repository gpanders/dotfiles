#!/bin/sh -f

urls=$(tmux capture-pane -J -p | grep -oE '(?:https?|file)://[-a-zA-Z0-9@:%_+.~#?&/=]+[-a-zA-Z0-9@%_+.~#?&/=]+')

if [ -z "$urls" ]; then
	tmux display-message 'No URLs found'
	exit
fi

header='Press CTRL-Y to copy URL to clipboard'
selected=$(printf '%s\n' "$urls" | fzf-tmux -p -m --expect ctrl-y --header "$header")

# shellcheck disable=2086
set -- $selected

cmd='xargs -n 1 open'
if [ "$1" = "ctrl-y" ]; then
	if command -v pbcopy >/dev/null; then
		cmd=pbcopy
	elif command -v xsel >/dev/null; then
		cmd='xsel --clipboard --input'
	elif command -v xclip >/dev/null; then
		cmd='xclip -selection clipboard'
	else
		tmux display-message 'No command to control clipboard with'
		exit
	fi
fi

shift
printf '%s\n' "$@" | $cmd
