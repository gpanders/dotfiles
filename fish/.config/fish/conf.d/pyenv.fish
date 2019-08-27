if status is-login
    if test -s ~/.pyenv/bin/pyenv
        set -x PYENV_ROOT ~/.pyenv
        set -x PATH $PYENV_ROOT/bin $PATH
    end

    if command -sq pyenv
        source (pyenv init - --no-rehash fish | psub)
    end
end
