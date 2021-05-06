#!/bin/sh -uf

if [ -z "${TMUX:-}" ]; then
	echo 'Not in a tmux session' >&2
	exit
fi

if [ -t 0 ]; then
	tmux display-message 'No data on stdin'
	exit
fi

urls=$(grep -oE '(https?|file)://[-a-zA-Z0-9@:%_+.~#?&/=]+[-a-zA-Z0-9@%_+.~#?&/=]+')
if [ -z "$urls" ]; then
	tmux display-message 'No URLs found'
	exit
fi

main() {
	header='Press Enter to copy URL to clipboard or Ctrl-O to open in browser'
	selected=$(printf '%s\n' "$urls" | fzf-tmux ${TMUX_HAS_POPUP:+-p} -m --expect ctrl-o --header "$header")

	[ -n "$selected" ] || exit 0

	# shellcheck disable=2086
	set -- $selected

	if [ "$1" = "ctrl-o" ]; then
		shift
		printf '%s\n' "$@" | xargs -n 1 open
		exit
	fi

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

	printf '%s\n' "$@" | $cmd
}

main &
