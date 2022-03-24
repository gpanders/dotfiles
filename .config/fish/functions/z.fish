# Jump to a directory using only keywords.
function z
    set argc (count $argv)
    if test $argc -eq 0
        cd $HOME
    else if test "$argv" = -
        cd -
    else if test $argc -eq 1 -a -d $argv[1]
        cd $argv[1]
    else
        set -l result (command zoxide query --exclude $PWD -- $argv)
        and cd $result
    end
end
