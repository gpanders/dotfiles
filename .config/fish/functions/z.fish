# Jump to a directory using only keywords.
function z
    if not command -sq zoxide
        functions -e z
        fish_command_not_found z
        return 127
    end

    set -l argc (count $argv)
    set -l completion_regex '^'(string escape --style=regex 'z!')'(.*)$'

    if test $argc -eq 0
        cd $HOME
    else if test "$argv" = -
        cd -
    else if test $argc -eq 1 -a -d $argv[1]
        cd $argv[1]
    else if set -l result (string match --groups-only --regex $completion_regex $argv[-1])
        cd $result
    else
        set -l result (command zoxide query --exclude $PWD -- $argv)
        and cd $result
    end
end
