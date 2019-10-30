function weechat -d "Alias for weechat with custom config location"
    command weechat -d $HOME/.config/weechat $argv
end
