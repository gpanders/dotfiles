function weechat -d "Alias for weechat with custom config location and Python virtual env"
    set -l venv
    if test -n "$venv_dir"; and test -d $venv_dir/weechat
        set venv $venv_dir/weechat/lib/python*/site-packages
    end

    PYTHONPATH=$venv command weechat -d $HOME/.config/weechat $argv
end
