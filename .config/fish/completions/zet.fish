function __fish_complete_zettels
    if not set -q ZETTEL_DIR
        sed -E 's/^([^[:space:]]+)=(.*)/set \1 "\2"/' < ~/.config/zet/config | source
    end
    printf "%s\n" (command ls $ZETTEL_DIR | sed 's/\.[^.]*$//')
end

function __fish_complete_zettel_tags
    if not set -q ZETTEL_DIR
        sed -E 's/^([^[:space:]]+)=(.*)/set \1 "\2"/' < ~/.config/zet/config | source
    end

    awk 'tolower($1) ~ /(keywords|tags)/ {$1=""; print $0}' $ZETTEL_DIR/*.txt | tr ',' '\n' | sort -u | awk '{$1=$1;print}'
end

complete -k -x -c zet -n '__fish_seen_subcommand_from sh show' -a '(__fish_complete_zettels)'
complete -k -x -c zet -n '__fish_seen_subcommand_from o open' -a '(__fish_complete_zettels)'
complete -k -x -c zet -n '__fish_seen_subcommand_from p pre prev preview' -a '(__fish_complete_zettels)'
complete -k -x -c zet -n '__fish_seen_subcommand_from t tag tags' -a '(__fish_complete_zettel_tags)'
complete -k -x -c zet -n '__fish_seen_subcommand_from l ls list'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'new' -d 'Create a zettel'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'show' -d 'Display a zettel'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'open' -d 'Open a zettel in $EDITOR'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'list' -d 'List zettels'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'search' -d 'Search zettels'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'tag' -d 'List zettels with given tag'
complete -k -x -c zet -n '__fish_use_subcommand' -a 'preview' -d 'Preview a zettel'
