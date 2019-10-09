if status is-login; and command -sq fzf
    if command -sq fd
        set -gx FZF_FIND_FILE_COMMAND "fd --type f --hidden --follow --exclude .git"
        set -gx FZF_CD_COMMAND "fd -t d"
    else if command -sq rg
        set -gx FZF_FIND_FILE_COMMAND "rg --files --hidden --glob '!.git'"
    else if command -sq ag
        set -gx FZF_FIND_FILE_COMMAND "ag -g ''"
    end

    set -gx FZF_COLORS "bg+:10,bg:0,spinner:6,hl:4,fg:12,header:4,info:3,pointer:6,marker:6,fg+:13,prompt:3,hl+:4"
    set -gx FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT --color $FZF_COLORS"
end
