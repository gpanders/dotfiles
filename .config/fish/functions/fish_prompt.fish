function fish_prompt
    printf "$__prompt_host$__prompt_pwd $__prompt_git$__prompt_cmd_duration$__prompt_status\n$__prompt_jobs$__prompt_venv$__prompt_delim\e[0m "
end
