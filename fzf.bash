# Setup fzf
# ---------
if [[ ! "$PATH" == */home/greande/.fzf/bin* ]]; then
  export PATH="$PATH:/home/greande/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/greande/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/greande/.fzf/shell/key-bindings.bash"

# Color
declare -A fzf_colors
fzf_colors["Solarized Dark"]="fg:-1,bg:-1,hl:#268bd2,fg+:#93a1a1,bg+:#073642,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["Solarized Light"]="fg:-1,bg:-1,hl:#268bd2,fg+:#586e75,bg+:#eee8d5,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"

if [[ -z "$TERM_BG_COLOR" && "$TERM_BG_COLOR" == "light" ]];
  export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors['Solarized Light']}"
else
  export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors['Solarized Dark']}"
fi

# Use ag
if hash ag 2>/dev/null; then
  export FZF_CTRL_T_COMMAND="ag -g ''"
  _fzf_compgen_path() {
    ag -g '' "$1"
  }
fi

export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

if [ -n "$TMUX_PANE" ]; then
  export FZF_TMUX=1
  alias fzf="fzf-tmux -d40%"
else
  export FZF_TMUX=0
  export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --height 40% --inline-info --reverse"
fi


