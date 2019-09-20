function __fish_complete_venvs
    set -q VENV_DIR; or set -U VENV_DIR $XDG_DATA_HOME/venv
    printf "%s\n" (command ls $VENV_DIR)
end

complete -x -c venv -n '__fish_use_subcommand' -a 'create' -d 'Create a virtual environment'
complete -x -c venv -n '__fish_use_subcommand' -a 'rm' -d 'Delete a virtual environment'
complete -x -c venv -n '__fish_use_subcommand' -a 'ls' -d 'List all virtual environments'
complete -x -c venv -n '__fish_use_subcommand' -a '(__fish_complete_venvs)'
complete -x -c venv -n '__fish_seen_subcommand_from rm' -a '(__fish_complete_venvs)'
