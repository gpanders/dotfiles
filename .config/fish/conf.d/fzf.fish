if status is-interactive; and command -sq fzf
    if command -sq fd
        set FZF_FIND_FILE_COMMAND "fd --type f --follow"
        set FZF_CD_COMMAND "fd -t d"
    else if command -sq rg
        set FZF_FIND_FILE_COMMAND "rg --files"
    else if command -sq ag
        set FZF_FIND_FILE_COMMAND "ag -g ''"
    end

    set -gx FZF_DEFAULT_COMMAND $FZF_FIND_FILE_COMMAND
    set -gx FZF_DEFAULT_OPTS "--height=10 --reverse --no-info --color=16 --cycle"
end
