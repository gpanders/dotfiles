#!/bin/sh

set -euf

packpath="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/plugins"

plug() {
	url="https://$1"
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

prompt() {
	path="$1"
	quit=0
	while true; do
		printf 'Update? [y/N/l/q] '
		read -r ans
		case "$ans" in
		[yY])
			git -C "$path" merge --ff-only '@{u}'
			printf '\n'
			break
			;;
		[lL])
			git -C "$path" log --patch --color 'HEAD...@{u}'
			;;
		[qQ])
			quit=1
			break
			;;
		*)
			break
			;;
		esac
	done

	return "$quit"
}

showlog() {
	plugins="$(find "$packpath" -mindepth 2 -maxdepth 2 -type d)"
	for path in $plugins; do
		count="$(git -C "$path" rev-list --count 'HEAD...@{u}')"
		if [ "$count" -gt 0 ]; then
			name=${path##*/}
			printf '%s has %d new commits:\n' "$name" "$count"
			git -C "$path" log --color --format='%>(12)%C(auto)%h %s' 'HEAD...@{u}'
			if ! prompt "$path"; then
				break
			fi
		fi
	done
}

plug tpope.io/vim/surround
plug tpope.io/vim/commentary
plug tpope.io/vim/repeat
plug tpope.io/vim/abolish
plug tpope.io/vim/eunuch
plug tpope.io/vim/rsi
plug tpope.io/vim/scriptease opt
plug tpope.io/vim/fugitive
plug tpope.io/vim/sleuth
plug tpope.io/vim/obsession
plug tpope.io/vim/dispatch
plug github.com/gpanders/nvim-parinfer
plug github.com/justinmk/vim-dirvish
plug github.com/junegunn/vim-easy-align
plug github.com/mfussenegger/nvim-lsp-compl
plug github.com/lewis6991/gitsigns.nvim
plug github.com/nvim-lua/plenary.nvim # Required by telescope
plug github.com/nvim-telescope/telescope.nvim opt 0.1.x
plug github.com/nvim-telescope/telescope-fzy-native.nvim
plug github.com/nvim-treesitter/nvim-treesitter opt
plug github.com/nvim-treesitter/playground opt
plug github.com/dcampos/nvim-snippy
plug github.com/ziglang/zig.vim

wait

showlog

nvim --headless +'helptags ALL' +q
