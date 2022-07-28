hook global WinSetOption filetype=zig %{
    set-option buffer makecmd 'zig build'
}
