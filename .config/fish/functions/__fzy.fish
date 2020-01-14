function __fzy_find --argument-names cmd
    set -l tok (commandline -t)
    eval $cmd | fzy --query=$tok | read -l selection
    and commandline -t -- (string escape $selection)
    commandline -f repaint
end

function __fzy --description "Find file or directory with fzy"
    set -l cmd (commandline -oc)
    if test "$cmd[1]" = "cd"
        __fzy_find $FZY_FIND_DIR_COMMAND
    else
        __fzy_find $FZY_FIND_FILE_COMMAND
    end
end
