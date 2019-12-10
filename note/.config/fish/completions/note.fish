function __fish_complete_notes
    set -q NOTES_DIR; or set -l NOTES_DIR $HOME/Sync/Notes
    printf "%s\n" (command ls $NOTES_DIR | sed 's/\.[^.]*$//')
end

complete -k -x -c note -n '__fish_seen_subcommand_from s sh sho show' -a '(__fish_complete_notes)'
complete -k -x -c note -n '__fish_seen_subcommand_from o op ope open' -a '(__fish_complete_notes)'
complete -k -x -c note -n '__fish_seen_subcommand_from l li lis list'
complete -k -x -c note -n '__fish_use_subcommand' -a 'new' -d 'Create a note'
complete -k -x -c note -n '__fish_use_subcommand' -a 'show' -d 'Display a note'
complete -k -x -c note -n '__fish_use_subcommand' -a 'open' -d 'Open a note in $EDITOR (all notes if no argument)'
complete -k -x -c note -n '__fish_use_subcommand' -a 'list' -d 'List notes'
complete -k -x -c note -n '__fish_use_subcommand' -a 'grep' -d 'Search notes'
complete -k -x -c note -n '__fish_use_subcommand' -a 'tag' -d 'List notes with given tag'
