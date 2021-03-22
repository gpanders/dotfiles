function __fuzzy_find_cmd
    set -l cmd (commandline -oc)
    if test "$cmd[1]" = cd
        $fuzzy_find_dir_command
    else
        $fuzzy_find_file_command
    end
end

function __fuzzy_find --description "Find file or directory with fuzzy finder"
    set -l tok (commandline -t)
    __fuzzy_find_cmd | $argv --query=$tok | read -l selection
    and commandline -t -- (string escape $selection)
    commandline -f repaint
end
