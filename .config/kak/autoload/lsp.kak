try %{
    eval %sh{kak-lsp --kakoune -s $kak_session}
    hook global WinSetOption filetype=(rust|python|go|zig|c|cpp) %{
        lsp-enable-window
        lsp-auto-signature-help-enable
    }
}
