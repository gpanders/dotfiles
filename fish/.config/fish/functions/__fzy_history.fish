function __fzy_history --description "Filter history through fzy"
    builtin history merge
    set -l cmd (commandline)
    set -l query "builtin history search"
    if test -n $cmd
        set -a query $cmd
    end
    set -l selection (eval $query | fzy)
    if test -n $selection
        # Compensate for multiline prompts by moving the cursor
        # \033[<N>A is the control character to move the cursor up N lines
        printf \033\["%d"A (math (count (fish_prompt)) - 1)
        commandline $selection
    end
    commandline -f repaint
end

