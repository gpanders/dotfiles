function tmux -d "Alias for tmux to open config file in custom location"
    command tmux -f $HOME/.config/tmux/tmux.conf $argv

end
