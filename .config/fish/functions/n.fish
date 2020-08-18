function n --wraps nnn --description "Alias for nnn"
    if test -n "$XDG_CONFIG_HOME"
        set nnn_config_dir $XDG_CONFIG_HOME/nnn
    else
        set nnn_config_dir $HOME/.config/nnn
    end

    # Filetype colors. This mimics dircolors
    set -x NNN_FCOLORS 030304020000060801030500

    # Trash (instead of delete) files to desktop Trash
    set -x NNN_TRASH 1

    set -x NNN_PLUG p:preview-tui

    if test -x $nnn_config_dir/plugins/nuke
        set -x NNN_OPENER $nnn_config_dir/plugins/nuke
        set -x NNN_OPTS "$NNN_OPTS"c
    end

    # Block nesting of nnn in subshells
    if test -n "$NNNLVL"
        if test (expr $NNNLVL + 0) -ge 1
            echo 'nnn is already running'
            return
        end
    end

    set NNN_TMPFILE $nnn_config_dir/nnn/.lastd

    nnn -a -e $argv

    if test -e "$NNN_TMPFILE"
        cat $NNN_TMPFILE
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end
