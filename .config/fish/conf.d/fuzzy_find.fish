if status is-interactive
    if command -sq fd
        set fuzzy_find_file_command fd --type f --follow
        set fuzzy_find_dir_command fd -t d
    else if command -sq rg
        set fuzzy_find_file_command rg --files
        set fuzzy_find_dir_command find -L . -mindepth 1 -type d -print | sed 's|^\./||'
    else if command -sq ag
        set fuzzy_find_file_command ag -g ''
        set fuzzy_find_dir_command find -L . -mindepth 1 -type d -print | sed 's|^\./||'
    end

    set -gx FZF_DEFAULT_COMMAND "$fuzzy_find_file_command"
    set -gx FZF_DEFAULT_OPTS '--height=20 --reverse --no-info --cycle --color=16,fg:-1,fg+:-1:bold,hl:3:bold,hl+:3:bold,pointer:4,marker:-1,prompt:-1,info:-1'
end
