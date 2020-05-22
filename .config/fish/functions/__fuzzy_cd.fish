function __fuzzy_cd --description "Find and chdir to directory with fzf" --argument-names prog
    echo $prog
    eval $fuzzy_find_dir_command | $prog | read -l selection
    and cd $selection
    commandline -f repaint
end
