function __fuzzy_cd --description "Find and chdir to directory with fzf"
    echo $prog
    eval $fuzzy_find_dir_command | $argv | read -l selection
    and cd $selection
    commandline -f repaint
end
