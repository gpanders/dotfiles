function __fish_complete_notes
    set -q NOTES_PATH; or set -l NOTES_PATH "$HOME/.notes"
    printf "%s\n" (command ls $NOTES_PATH | sed 's/\.[^.]*$//')
end

complete -k -x -c ual -n '__fish_seen_subcommand_from rm edit' -a '(__fish_complete_notes)'
complete -k -x -c ual -n '__fish_seen_subcommand_from sync ls'
complete -k -x -c ual -n '__fish_use_subcommand' -a '(__fish_complete_notes)'
complete -k -x -c ual -n '__fish_use_subcommand' -a 'rm' -d 'Remove a note'
complete -k -x -c ual -n '__fish_use_subcommand' -a 'ls' -d 'List all notes'
complete -k -x -c ual -n '__fish_use_subcommand' -a 'edit' -d 'Edit a note'
complete -k -x -c ual -n '__fish_use_subcommand' -a 'sync' -d 'Synchronize notes'
