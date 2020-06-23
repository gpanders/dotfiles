if status is-interactive; and command -sq fzf
    if command -sq fd
        set fuzzy_find_file_command 'fd --type f --follow'
        set fuzzy_find_dir_command 'fd -t d'
    else if command -sq rg
        set fuzzy_find_file_command 'rg --files'
        set fuzzy_find_dir_command "find -L . -mindepth 1 -type d -print | sed 's|^\./||'"
    else if command -sq ag
        set fuzzy_find_file_command "ag -g ''"
        set fuzzy_find_dir_command "find -L . -mindepth 1 -type d -print | sed 's|^\./||'"
    end

    set -gx FZF_DEFAULT_COMMAND $fuzzy_find_file_command
    set -gx FZF_DEFAULT_OPTS "--height=10 --reverse --no-info --no-color --cycle"

    bind \ct '__fuzzy_find fzf'
    bind \cr '__fuzzy_history fzf'
    bind \ec '__fuzzy_cd fzf'
end
