#!/bin/sh -uf

if [ -z "${TMUX:-}" ]; then
	echo 'Not in a tmux session' >&2
	exit
fi

if [ -t 0 ]; then
	tmux display-message 'No data on stdin'
	exit
fi

copy() {
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

	printf '%s' "$@" | $cmd
}

while getopts "u" o; do
	case "$o" in
	u)
		pattern='(https?|file)://[-a-zA-Z0-9@:%_+.~#?&/=<>]+'
		name='URLs'
		header='Press Enter to copy URL to clipboard or Ctrl-O to open in browser'
		action() { printf '%s\n' "$@" | xargs -n 1 open ; }
		;;
	*)
		printf 'Invalid option: %s\n' "$o"
		exit 1
	esac
done
shift $((OPTIND - 1))

items=$(grep -oE "$pattern" | uniq)
if [ -z "$items" ]; then
	tmux display-message "No $name found"
	exit
fi

main() {
	selected=$(printf '%s\n' "$items" | fzf-tmux ${TMUX_HAS_POPUP:+-p} -m --expect ctrl-o --header "$header")

	[ -n "$selected" ] || exit 0

	# shellcheck disable=2086
	set -- $selected

	if [ "$1" = "ctrl-o" ]; then
		shift
		action "$@"
		exit
	fi

	copy "$@"
}

main &
