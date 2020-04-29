set -gx EDITOR vi
set -gx VISUAL vi
set -gx PAGER less -iMRS

set -q XDG_CONFIG_HOME; or set -U XDG_CONFIG_HOME $HOME/.config
set -q XDG_DATA_HOME; or set -U XDG_DATA_HOME $HOME/.local/share

# Bootstrap fisher
# This needs to be in config.fish instead of a separate file in conf.d so that
# it is guaranteed to run after all scripts in the conf.d directory
set -g fisher_path $XDG_CONFIG_HOME/fish/fisher

set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..-1]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..-1]

if not functions -q fisher
    curl https://git.io/fisher --create-dirs -sLo $fisher_path/functions/fisher.fish
    fish -c fisher
end

for file in $fisher_path/conf.d/*.fish
    builtin source $file 2>/dev/null
end
