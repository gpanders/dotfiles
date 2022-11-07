function __venv_create --argument-names dir
    python3 -m venv $dir
    if not test $status
        echo "Something went wrong; virtual environment creation failed" >&2
        return 1
    end
end

function __venv_activate --argument-names venv
    set -q venv_dir; or set venv_dir $XDG_DATA_HOME/venv
    VIRTUAL_ENV_DISABLE_PROMPT=1 source $venv_dir/$venv/bin/activate.fish
end

function venv
    set -q venv_dir; or set venv_dir $XDG_DATA_HOME/venv

    switch $argv[1]
        case '-h' '--help' 'help'
            echo 'Usage:
    venv                                List existing venvs and select using fzf
    venv create <venv>                  Create a new venv
    venv rm <venv> [venv [...]]         Remove existing venv(s)
    venv <venv>                         Activate the given venv
' >&2
            return 1
        case ''
            set -l venv (printf '%s\n' (command ls $venv_dir) | fzf)
            if test -n "$venv"
                __venv_activate $venv
            end
        case create
            if __venv_create $venv_dir/$argv[2]
                echo "Virtual environment $argv[2] created. Activate it using 'venv $argv[2]'"
            end
        case rm
            for _venv in $argv[2..-1]
                if not test -d $venv_dir/$_venv
                    echo "venv '$_venv' not found" >&2
                    return 1
                end
                rm -rf $venv_dir/$_venv
                if test $status
                    echo "Virtual environment '$_venv' was successfully deleted"
                else
                    echo "Something went wrong; virtual environment deletion failed" >&2
                    return 1
                end
            end
        case '*'
            if test -d $venv_dir/$argv[1]
                __venv_activate $argv[1]
            else
                read -P "venv '$argv[1]' does not exist. Create it? [Y/n] " ans
                if test (string lower $ans) != 'n'
                    if __venv_create $venv_dir/$argv[1]
                        echo "Virtual environment $argv[1] created"
                        __venv_activate $argv[1]
                    end
                end
            end
    end
end

