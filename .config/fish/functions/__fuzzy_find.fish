function __fuzzy_find_cmd --argument-names prog cmd
    set -l tok (commandline -t)
    eval $cmd | $prog --query=$tok | read -l selection
    and commandline -t -- (string escape $selection)
    commandline -f repaint
end

function __fuzzy_find --description "Find file or directory with fuzzy finder" --argument-names prog
    set -l cmd (commandline -oc)
    if test "$cmd[1]" = "cd"
        __fuzzy_find_cmd $prog $fuzzy_find_dir_command
    else
        __fuzzy_find_cmd $prog $fuzzy_find_file_command
    end
end
