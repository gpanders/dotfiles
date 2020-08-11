function n --wraps nnn --description "Alias for nnn"
    # Block nesting of nnn in subshells
    if test -n "$NNNLVL"
        if test (expr $NNNLVL + 0) -ge 1
            echo "nnn is already running"
            return
        end
    end

    if test -n "$XDG_CONFIG_HOME"
        set NNN_TMPFILE $XDG_CONFIG_HOME/nnn/.lastd
    else
        set NNN_TMPFILE $HOME/.config/nnn/.lastd
    end

    nnn -e $argv

    if test -e "$NNN_TMPFILE"
        cat $NNN_TMPFILE
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end
