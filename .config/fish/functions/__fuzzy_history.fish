function __fuzzy_history --description "Show command history with fuzzy finder"
    history merge
    set -l cmd (commandline)
    test -n $cmd; or set cmd search
    history $cmd | $argv | read -l selection
    and commandline -- $selection
    commandline -f repaint
end
