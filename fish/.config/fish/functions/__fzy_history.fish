function __fzy_history --description "Show command history with fzy"
    history merge
    eval (string trim "history "(commandline)) | fzy | read -l selection
    and commandline -- $selection
    commandline -f repaint
end

