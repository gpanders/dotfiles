function __fzy_find --argument-names cmd
    set -l tok (commandline -t)
    eval $cmd | fzy --query=$tok | read -l selection
    and commandline -t -- (string escape $selection)
    commandline -f repaint
end

function __fzy_files
    if not set -q FZY_FIND_FILE_COMMAND
        if command -sq fd
            set FZY_FIND_FILE_COMMAND "fd --type f"
        else if command -sq rg
            set FZY_FIND_FILE_COMMAND "rg --files"
        else if command -sq ag
            set FZY_FIND_FILE_COMMAND "ag -g ''"
        else
            set FZY_FIND_FILE_COMMAND "find . -type f -not -path '*/.*' 2>/dev/null"
        end
    end
    __fzy_find $FZY_FIND_FILE_COMMAND
end

function __fzy_dir
    if not set -q FZY_FIND_DIR_COMMAND
        if command -sq fd
            set FZY_FIND_DIR_COMMAND "fd --type d"
        else
            set FZY_FIND_DIR_COMMAND "find . -type d -not -path '*/.*' 2>/dev/null"
        end
    end
    __fzy_find $FZY_FIND_DIR_COMMAND
end

function __fzy
    set -l cmd (commandline -oc)
    if test "$cmd[1]" = "cd"
        __fzy_dir
    else
        __fzy_files
    end
end
