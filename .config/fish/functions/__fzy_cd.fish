function __fzy_cd --description "Find and chdir to directory with fzy"
    eval $FZY_FIND_DIR_COMMAND | fzy | read -l selection
    and cd $selection
    commandline -f repaint
end
