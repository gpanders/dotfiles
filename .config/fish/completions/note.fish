function __fish_complete_notes
    if not set -q NOTES_DIR
        sed -E 's/^([^[:space:]]+)=(.*)/set \1 "\2"/' < ~/.config/note/config | source
    end
    printf "%s\n" (command ls $NOTES_DIR | sed 's/\.[^.]*$//')
end

function __fish_complete_note_tags
    if not set -q NOTES_DIR
        sed -E 's/^([^[:space:]]+)=(.*)/set \1 "\2"/' < ~/.config/note/config | source
    end

    awk 'tolower($1) ~ /(keywords|tags)/ {$1=""; print $0}' $NOTES_DIR/*.txt | tr ',' '\n' | sort -u | awk '{$1=$1;print}'
end

complete -k -x -c note -n '__fish_seen_subcommand_from s sh sho show' -a '(__fish_complete_notes)'
complete -k -x -c note -n '__fish_seen_subcommand_from o op ope open' -a '(__fish_complete_notes)'
complete -k -x -c note -n '__fish_seen_subcommand_from p pr pre prev previ previe preview' -a '(__fish_complete_notes)'
complete -k -x -c note -n '__fish_seen_subcommand_from t ta tag tags' -a '(__fish_complete_note_tags)'
complete -k -x -c note -n '__fish_seen_subcommand_from l li lis list'
complete -k -x -c note -n '__fish_use_subcommand' -a 'new' -d 'Create a note'
complete -k -x -c note -n '__fish_use_subcommand' -a 'show' -d 'Display a note'
complete -k -x -c note -n '__fish_use_subcommand' -a 'open' -d 'Open a note in $EDITOR (all notes if no argument)'
complete -k -x -c note -n '__fish_use_subcommand' -a 'list' -d 'List notes'
complete -k -x -c note -n '__fish_use_subcommand' -a 'grep' -d 'Search notes'
complete -k -x -c note -n '__fish_use_subcommand' -a 'tag' -d 'List notes with given tag'
complete -k -x -c note -n '__fish_use_subcommand' -a 'preview' -d 'Preview a note'
