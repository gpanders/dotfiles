function __fish_complete_venvs
    set -q VENV_DIR; or set -U VENV_DIR $XDG_DATA_HOME/venv
    printf "%s\n" (command ls $VENV_DIR)
end

complete -x -c venv -n '__fish_use_subcommand' -a '(__fish_complete_venvs)'
complete -x -c venv
