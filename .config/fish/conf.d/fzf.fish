status is-interactive; or exit

if command -sq fd
    set -g FZF_CTRL_T_COMMAND fd --type f --follow
    set -g FZF_ALT_C_COMMAND fd -t d --hidden
else if command -sq fdfind
    set -g FZF_CTRL_T_COMMAND fdfind --type f --follow --hidden
    set -g FZF_ALT_C_COMMAND fdfind -t d --hidden
else if command -sq rg
    set -g FZF_CTRL_T_COMMAND rg --files --hidden
    set -g FZF_ALT_C_COMMAND find -L . -mindepth 1 -type d -print
else if command -sq ag
    set -g FZF_CTRL_T_COMMAND ag -g ''
    set -g FZF_ALT_C_COMMAND find -L . -mindepth 1 -type d -print
end

set -gx FZF_DEFAULT_COMMAND "$FZF_CTRL_T_COMMAND"
set -gx FZF_DEFAULT_OPTS '--no-color'

# Export options to tmux for fzfurl script
if test -n "$TMUX"
    command tmux set-env -g FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS"
end
