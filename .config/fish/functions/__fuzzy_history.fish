function __fuzzy_history --description "Show command history with fuzzy finder" --argument-names prog
    history merge
    set -l cmd (commandline)
    test -n $cmd; or set cmd search
    history $cmd | $prog | read -l selection
    and commandline -- $selection
    commandline -f repaint
end

