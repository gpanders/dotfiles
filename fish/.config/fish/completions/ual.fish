function __fish_ual_needs_command
    set cmd (commandline -opc)
    return (test (count $cmd) -eq 1)
end

function __fish_ual_using_command
    set cmd (commandline -opc)
    if test (count $cmd) -gt 1
        switch $cmd[2]
            case rm edit
                return 0
            case '*'
                return 1
        end
    end
    return 1
end

function __fish_complete_notes
    set -q NOTES_PATH; or set -l NOTES_PATH "$HOME/.notes"
    printf "%s\n" (command ls $NOTES_PATH | sed 's/\.[^.]*$//')
end

complete -k -f -c ual -n '__fish_ual_using_command' -a '(__fish_complete_notes)'
complete -k -f -c ual -n '__fish_ual_needs_command' -a '(__fish_complete_notes)'
complete -k -f -c ual -n '__fish_ual_needs_command' -a 'rm' -d 'Remove a note'
complete -k -f -c ual -n '__fish_ual_needs_command' -a 'ls' -d 'List all notes'
complete -k -f -c ual -n '__fish_ual_needs_command' -a 'edit' -d 'Edit a note'
complete -k -f -c ual -n '__fish_ual_needs_command' -a 'sync' -d 'Synchronize notes'
