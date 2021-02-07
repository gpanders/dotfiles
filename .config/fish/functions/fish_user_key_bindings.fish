function fish_user_key_bindings
    bind \ef nextd-or-forward-word
    bind \eb prevd-or-backward-word

    bind \ct '__fuzzy_find fzf'
    bind \cr '__fuzzy_history fzf'
    bind \ec '__fuzzy_cd fzf'
end
