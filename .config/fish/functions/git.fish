function git
    if test "$PWD" = "$HOME"
        command git --git-dir=$HOME/.dotfiles --work-tree=$HOME $argv
    else
        command git $argv
    end
end

# Allow fish completion to work with wrapped git
complete -c git --condition 'test $PWD = $HOME' --wraps "git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
