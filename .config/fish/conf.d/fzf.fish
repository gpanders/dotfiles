status is-interactive; or exit

if command -sq fd
    set -g FZF_CTRL_T_COMMAND fd --type f --follow --strip-cwd-prefix --hidden --exclude .git
    set -g FZF_ALT_C_COMMAND fd --type d --strip-cwd-prefix
else if command -sq fdfind
    set -g FZF_CTRL_T_COMMAND fdfind --type f --follow --strip-cwd-prefix --hidden --exclude .git
    set -g FZF_ALT_C_COMMAND fdfind -t d --strip-cwd-prefix
else if command -sq rg
    set -g FZF_CTRL_T_COMMAND rg --files
    set -g FZF_ALT_C_COMMAND find -L . -mindepth 1 -type d -print
else if command -sq ag
    set -g FZF_CTRL_T_COMMAND ag -g ''
    set -g FZF_ALT_C_COMMAND find -L . -mindepth 1 -type d -print
end

set -gx FZF_DEFAULT_COMMAND "$FZF_CTRL_T_COMMAND"
set -gx FZF_DEFAULT_OPTS '--tiebreak=length,end --color=fg:-1,bg:-1,fg+:-1:reverse,bg+:-1:reverse,hl:3,hl+:3:reverse,pointer:-1,marker:4,info:-1,prompt:-1 --scrollbar=▐'

if command -sq bat
    set -gx FZF_CTRL_T_OPTS "--ansi --preview 'bat -f -p --theme ansi --line-range :300 {}'"
else
    set -gx FZF_CTRL_T_OPTS "--preview 'cat {}'"
end

# Export options to tmux for fzfurl script
if test -n "$TMUX"
    command tmux set-env -g FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS"
end
