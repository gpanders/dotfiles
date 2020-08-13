function __venv_create --argument-names dir
    python3 -m venv $dir
    if not test $status
        echo "Something went wrong; virtual environment creation failed" >&2
        return 1
    end
end

function venv
    set -q venv_dir; or set -U venv_dir $XDG_DATA_HOME/venv

    if test (count $argv) -lt 1
        echo 'Usage:
    venv create <venv>
    venv rm <venv>
    venv ls
    venv <venv>' >&2
        return 1
    end

    switch $argv[1]
        case create
            if __venv_create $venv_dir/$argv[2]
                echo "Virtual environment $argv[2] created. Activate it using 'venv $argv[2]'"
            end
        case ls
            printf '%s\n' (command ls $venv_dir)
        case rm
            if not test -d $venv_dir/$argv[2]
                echo "venv '$argv[2]' not found" >&2
                return 1
            end
            rm -rf $venv_dir/$argv[2]
            if test $status
                echo "Virtual environment '$argv[2]' was successfully deleted"
            else
                echo "Something went wrong; virtual environment deletion failed" >&2
                return 1
            end
        case '*'
            if test -d $venv_dir/$argv[1]
                source $venv_dir/$argv[1]/bin/activate.fish
            else
                read -P "venv '$argv[1]' does not exist. Create it? [Y/n] " ans
                if test (string lower $ans) != 'n'
                    if __venv_create $venv_dir/$argv[1]
                        echo "Virtual environment $argv[1] created"
                        source $venv_dir/$argv[1]/bin/activate.fish
                    end
                end
            end
    end
end

