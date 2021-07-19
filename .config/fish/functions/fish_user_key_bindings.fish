function fish_user_key_bindings
    bind \ef nextd-or-forward-word
    bind \eb prevd-or-backward-word

    functions -q fzf_key_bindings; and fzf_key_bindings

    command -sq doas; and bind \es 'fish_commandline_prepend doas'
end
