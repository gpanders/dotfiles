status is-interactive; or exit

if command -sq fd
    set -g FZF_CTRL_T_COMMAND fd --type f --follow
    set -g FZF_ALT_C_COMMAND fd -t d
else if command -sq rg
    set -g FZF_CTRL_T_COMMAND rg --files
    set -g FZF_ALT_C_COMMAND find -L . -mindepth 1 -type d -print | sed 's|^\./||'
else if command -sq ag
    set -g FZF_CTRL_T_COMMAND ag -g ''
    set -g FZF_ALT_C_COMMAND find -L . -mindepth 1 -type d -print | sed 's|^\./||'
end

set -gx FZF_DEFAULT_COMMAND "$FZF_CTRL_T_COMMAND"
set -gx FZF_DEFAULT_OPTS '--color=16,fg:-1,fg+:-1:bold,hl:3:bold,hl+:3:bold,pointer:4,marker:-1,prompt:-1,info:-1,border:11'

set -l dirs /opt/local/share/fzf/shell /usr/share/doc/fzf/examples $HOME/.fzf/shell
for dir in $dirs
    if test -f $dir/key-bindings.fish
        source $dir/key-bindings.fish
        break
    end
end

functions -q fzf_key_bindings; and fzf_key_bindings
