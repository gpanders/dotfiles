hook global WinSetOption filetype=sh %{
    set-option buffer lintcmd 'shellcheck -f gcc'
    set-option buffer formatcmd 'shfmt'
}
