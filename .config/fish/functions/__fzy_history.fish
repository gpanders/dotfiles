function __fzy_history --description "Show command history with fzy"
    history merge
    set -l cmd (commandline)
    test -n $cmd; or set cmd search
    history $cmd | fzy | read -l selection
    and commandline -- $selection
    commandline -f repaint
end

