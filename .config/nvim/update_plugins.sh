#!/bin/sh

set -euf

packpath="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/plugins"

plug() {
	url="$1"
	name=${url##*/}
	type=${2:-start}
	path="$packpath/$type/$name"

	if [ -d "$path" ]; then
		git -C "$path" fetch --quiet &
	else
		printf 'Cloning %s\n' "$name"
		git clone --quiet --recurse-submodules --shallow-submodules --depth=1 ${3:+--branch $3} "$url" "$path" &
	fi
}

showlog() {
	plugins="$(find "$packpath" -mindepth 2 -maxdepth 2 -type d)"
	for path in $plugins; do
		count="$(git -C "$path" rev-list --count 'HEAD...@{u}')"
		if [ "$count" -gt 0 ]; then
			name=${path##*/}
			printf '%s has %d new commits:\n' "$name" "$count"
			git -C "$path" log --color --format='%>(12)%C(auto)%h %s' 'HEAD...@{u}'
			printf '\nUpdate? [y/N/q] '
			read -r ans
			case "$ans" in
			[Yy])
				git -C "$path" merge --ff-only '@{u}'
				printf '\n'
				;;
			[qQ])
				break
				;;
			*) ;;
			esac

		fi
	done
}

github() {
	name="$1"
	shift
	plug "https://github.com/$name" "$@"
}

tpope() {
	name="$1"
	shift
	plug "https://tpope.io/vim/$name" "$@"
}

github gpanders/nvim-parinfer
tpope surround
tpope commentary
tpope repeat
tpope abolish
tpope eunuch
tpope rsi
tpope scriptease opt
tpope fugitive
tpope sleuth
tpope obsession
tpope dispatch
github justinmk/vim-dirvish
github junegunn/vim-easy-align
github mfussenegger/nvim-lsp-compl
github lewis6991/gitsigns.nvim
github nvim-lua/plenary.nvim # Required by telescope
github nvim-telescope/telescope.nvim opt 0.1.x
github nvim-telescope/telescope-fzy-native.nvim
github nvim-treesitter/nvim-treesitter opt
github nvim-treesitter/playground opt
github ii14/exrc.vim
github dcampos/nvim-snippy opt

# Language plugins
github ziglang/zig.vim

wait

showlog
