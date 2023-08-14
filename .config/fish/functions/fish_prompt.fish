function fish_prompt
    printf "\r\e[K\e]133;A\e\\$__prompt_color_host$__prompt_host$__prompt_color_cwd$__prompt_pwd $__prompt_color_git$__prompt_git$__prompt_color_cmd_duration$__prompt_cmd_duration$__prompt_status\n$__prompt_color_jobs$__prompt_jobs$__prompt_color_venv$__prompt_venv$__prompt_color_prompt_delim$fish_prompt_delim\e[0m\e]133;B\e\\ "
end
