if not set -q _fish_user_colors
    set -U fish_color_command green
    set -U fish_color_comment yellow
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
    set -U fish_pager_color_prefix normal --bold
    set -U fish_pager_color_completion normal
    set -U fish_pager_color_description yellow

    set -U _fish_user_colors 1
end
