# Jump to a directory using interactive search.
function zi
    if not command -sq zoxide
        functions -e zi
        fish_command_not_found zi
        return 127
    end

    set -l result (command zoxide query -i -- $argv)
    and cd $result
end
