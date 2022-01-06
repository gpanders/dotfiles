if not set -q __fish_user_colors
    set -l light false

    if $light
        set -U fish_color_command normal
        set -U fish_color_quote brblack
        set -U fish_color_redirection brblack
        set -U fish_color_end brblack
        set -U fish_color_error red
        set -U fish_color_param brblack
        set -U fish_color_comment brblack
        set -U fish_color_match --background=brblue
        set -U fish_color_selection white --bold --background=brblack
        set -U fish_color_search_match bryellow --background=brblack
        set -U fish_color_history_current --bold
        set -U fish_color_operator cyan
        set -U fish_color_escape cyan
        set -U fish_color_cwd blue
        set -U fish_color_cwd_root red
        set -U fish_color_valid_path --underline
        set -U fish_color_autosuggestion brblack
        set -U fish_color_user brgreen
        set -U fish_color_host normal
        set -U fish_color_cancel -r
        set -U fish_pager_color_completion normal
        set -U fish_pager_color_description B3A06D yellow
        set -U fish_pager_color_prefix normal --bold --underline
        set -U fish_pager_color_progress brwhite --background=cyan

        set -U fish_color_jobs brblack
        set -U fish_color_git brblack
        set -U fish_color_cmd_duration brblack
        set -U fish_color_prompt_delim magenta
    else
        set -U fish_color_cwd blue
        set -U fish_color_command green
        set -U fish_color_comment yellow
        set -U fish_color_normal normal
        set -U fish_color_error red --bold
        set -U fish_color_param normal
        set -U fish_color_quote yellow
        set -U fish_color_redirection normal
        set -U fish_color_autosuggestion brblack
        set -U fish_color_match blue
        set -U fish_color_escape cyan
        set -U fish_color_operator blue
        set -U fish_color_end normal
        set -U fish_color_search_match normal --background=bryellow
        set -U fish_color_cancel black --background=white
        set -U fish_color_selection normal --background=bryellow
        set -U fish_color_venv brblack
        set -U fish_pager_color_prefix normal --bold
        set -U fish_pager_color_completion normal
        set -U fish_pager_color_description yellow

        set -U fish_color_jobs white
        set -U fish_color_git brblack
        set -U fish_color_cmd_duration yellow
        set -U fish_color_prompt_delim magenta
    end

    set -U __fish_user_colors 1
end
