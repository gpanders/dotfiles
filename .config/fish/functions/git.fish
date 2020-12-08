function git
    if test "$PWD" = "$HOME"; and not contains clone $argv; and not contains -- -C $argv
        command git --git-dir=$HOME/.dotfiles --work-tree=$HOME $argv
    else
        command git $argv
    end
end

# Allow fish completion to work with wrapped git
complete -c git --condition 'test $PWD = $HOME; and not __fish_seen_argument -s C' --wraps "git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
