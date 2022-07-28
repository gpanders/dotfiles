hook global WinCreate .* %{
    try %{ editorconfig-load }
}
