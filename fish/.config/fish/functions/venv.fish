function venv
    set -q VENV_DIR
    or set -U VENV_DIR $XDG_DATA_HOME/venv

    if test (count $argv) -lt 1
        echo "Usage: venv <venv>" >&2
        return 1
    end

    if not test -d $VENV_DIR/$argv[1]
        echo "venv '$argv[1]' not found" >&2
        return 1
    end

    source $VENV_DIR/$argv[1]/bin/activate.fish
end

