function __dotfiles_pwd_handler --on-variable PWD
    if test "$PWD" = "$HOME"
        if test -z "$GIT_DIR"; and test -z "$GIT_WORK_TREE"
            set -gx GIT_DIR $HOME/.dotfiles
            set -gx GIT_WORK_TREE $HOME
            set -g __dotfiles_git
        end
    else if set -q __dotfiles_git
        set -e GIT_DIR
        set -e GIT_WORK_TREE
        set -e __dotfiles_git
    end
end
