# Jump to a directory using interactive search.
function zi
    set -l result (command zoxide query -i -- $argv)
    and cd $result
end
