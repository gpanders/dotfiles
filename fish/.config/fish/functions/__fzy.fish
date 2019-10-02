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

    set -l selection (eval $FZY_FIND_FILE_COMMAND | fzy)

    if test -z "$selection"
        commandline -f repaint
        return
    end

    commandline -it -- (string escape $selection)
    commandline -f repaint
end

function __fzy_dir
    if not set -q FZY_FIND_DIR_COMMAND
        if command -sq fd
            set FZY_FIND_DIR_COMMAND "fd --type d"
        else
            set FZY_FIND_DIR_COMMAND "find -type d 2>/dev/null"
        end
    end

    set -l selection (eval $FZY_FIND_DIR_COMMAND | fzy)

    if test -z "$selection"
        commandline -f repaint
        return
    end

    commandline -it -- (string escape $selection)
    commandline -f repaint
end

function __fzy
    set -l cmd (commandline -oc)
    if test "$cmd[1]" = "cd"
        __fzy_dir
    else
        __fzy_files
    end
end
