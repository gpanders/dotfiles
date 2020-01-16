if status is-interactive; and command -sq fzy
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

    if not set -q FZY_FIND_DIR_COMMAND
        if command -sq fd
            set FZY_FIND_DIR_COMMAND "fd --type d"
        else
            set FZY_FIND_DIR_COMMAND "find . -type d -not -path '*/.*' 2>/dev/null"
        end
    end
end
