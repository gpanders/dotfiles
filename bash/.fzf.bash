# Exit if fzf directory does not exist
if [ ! -d "$HOME/.fzf" ]; then
    return 1
fi

# Setup fzf
# ---------
if [[ ! "$PATH" == *$HOME/.fzf/bin* ]]; then
    export PATH="$PATH:$HOME/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "$HOME/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "$HOME/.fzf/shell/key-bindings.bash"

# Color
declare -A fzf_colors
fzf_colors["Solarized Dark"]="dark,fg:-1,bg:-1,hl:#268bd2,fg+:#93a1a1,bg+:#073642,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["Solarized Light"]="light,fg:-1,bg:-1,hl:#268bd2,fg+:#586e75,bg+:#eee8d5,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["One Dark"]="dark,fg:-1,bg:-1,hl:#98c379,fg+:#fefefe,bg+:#3e4452,hl+:#98c379,info:#61afef,prompt:#61afef,pointer:#e5c07b,marker:#e5c07b,spinner:#61afef"

# if [[ -z "$TERMBG" && "$TERMBG" == "light" ]]; then
#   export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors['Solarized Light']}"
# else
#   export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors['Solarized Dark']}"
# fi
export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors['One Dark']}"

# Use ag
if hash rg 2>/dev/null; then
    export FZF_CTRL_T_COMMAND="rg --files"
    _fzf_compgen_path() {
        rg --files "$1"
    }
elif hash ag 2>/dev/null; then
    export FZF_CTRL_T_COMMAND="ag -g ''"
    _fzf_compgen_path() {
        ag -g '' "$1"
    }
fi

export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2>/dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

if [ -n "$TMUX_PANE" ]; then
    export FZF_TMUX=1
    alias fzf="fzf-tmux -d40%"
else
    export FZF_TMUX=0
    export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --height 40% --inline-info --reverse"
fi


