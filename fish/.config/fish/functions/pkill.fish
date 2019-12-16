function pkill -d "Wrapper around pkill to use fuzzy finder when no argument given"
    if test (count $argv) -eq 0
        ps ax -o user=,pid=,%cpu=,%mem=,stat=,time=,command= | fzf | awk '{print $2}' | xargs kill
    else
        command pkill $argv
    end
end
