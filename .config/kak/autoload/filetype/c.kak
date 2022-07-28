hook global WinSetOption filetype=(c|cpp) %{
    %sh{
        {
            if [ "$kak_opt_filetype" = cpp ]; then
                compiler=${kak_client_env_CXX:-c++}
                lang='c++'
            else
                compiler=${kak_client_env_CC:-cc}
                lang='c'
            fi
            $compiler -E -Wp,-v -x "$lang" /dev/null 2>&1 | awk '
                /^ / {print "set-option -add buffer path " $1}
            ' > $kak_command_fifo
        } &
    }
}
