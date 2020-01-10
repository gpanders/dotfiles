function git --description "Wrapper around git to manage dotfiles when in home directory"
    if test $PWD = $HOME
        command git --git-dir=$HOME/.dotfiles --work-tree=$HOME $argv
    else
        command git $argv
    end
end

