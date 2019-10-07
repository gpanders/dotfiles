function __fzy_find --argument-names cmd
    set -l selection (eval $cmd | fzy)
    if test -n "$selection"
        # Compensate for multiline prompts by moving the cursor
        # \033[<N>A is the control character to move the cursor up N lines
        printf \033\["%d"A (math (count (fish_prompt)) - 1)
        commandline -it -- (string escape $selection)
    end
    commandline -f repaint
end

function __fzy_files
    if not set -q FZY_FIND_FILE_COMMAND
        if command -sq fd
            set FZY_FIND_FILE_COMMAND "fd --type f --hidden --exclude '.git'"
        else if command -sq rg
            set FZY_FIND_FILE_COMMAND "rg --files --hidden --glob '!.git'"
        else if command -sq ag
            set FZY_FIND_FILE_COMMAND "ag -g ''"
        else
            set FZY_FIND_FILE_COMMAND "find -type f 2>/dev/null | sed 's:^\.git/::'"
        end
    end
    __fzy_find $FZY_FIND_FILE_COMMAND
end

function __fzy_dir
    if not set -q FZY_FIND_DIR_COMMAND
        if command -sq fd
            set FZY_FIND_DIR_COMMAND "fd --type d"
        else
            set FZY_FIND_DIR_COMMAND "find -type d 2>/dev/null"
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
