set -U __fish_theme nord

set -l background    2e3440
set -l darkblack     373e4d
set -l black         3b4252
set -l lightblack    434c5e
set -l brightblack   4c566a
set -l brighterblack 616e88
set -l foreground    d8dee9
set -l darkwhite     aeb3bb
set -l white         e5e9f0
set -l brightwhite   eceff4
set -l brightcyan    8fbcbb
set -l cyan          88c0d0
set -l blue          81a1c1
set -l darkblue      5e81ac
set -l red           bf616a
set -l orange        d08770
set -l yellow        ebcb8b
set -l green         a3be8c
set -l magenta       b48ead

set -U fish_color_autosuggestion $brighterblack
set -U fish_color_cancel -r
set -U fish_color_cmd_duration $yellow
set -U fish_color_command $blue
set -U fish_color_comment $brighterblack
set -U fish_color_cwd $blue
set -U fish_color_end $cyan
set -U fish_color_error $red
set -U fish_color_escape $cyan
set -U fish_color_git $brighterblack
set -U fish_color_history_current --bold
set -U fish_color_jobs $foreground
set -U fish_color_keyword $blue
set -U fish_color_normal $foreground
set -U fish_color_operator $cyan
set -U fish_color_option $brightwhite
set -U fish_color_param $brightwhite
set -U fish_color_prompt_delim $magenta
set -U fish_color_quote $green
set -U fish_color_redirection $magenta
set -U fish_color_search_match $yellow --background=$lightblack
set -U fish_color_selection $white --bold --background=$lightblack
set -U fish_color_status $yellow
set -U fish_color_user $foreground
set -U fish_color_valid_path --underline
set -U fish_color_venv $brighterblack
set -U fish_pager_color_completion $foreground
set -U fish_pager_color_description $yellow
set -U fish_pager_color_prefix $foreground --bold --underline
set -U fish_pager_color_progress $brightwhite --background=$cyan
set -U fish_pager_color_selected_background --background=$lightblack
