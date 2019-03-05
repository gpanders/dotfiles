FZF_DEFAULT_OPTS=""

# Color
# -----
typeset -A fzf_colors
fzf_colors["Solarized Dark"]="dark,fg:-1,bg:-1,hl:#268bd2,fg+:#93a1a1,bg+:#073642,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["Solarized Light"]="light,fg:-1,bg:-1,hl:#268bd2,fg+:#586e75,bg+:#eee8d5,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["One Dark"]="dark,fg:-1,bg:-1,hl:#98c379,fg+:#fefefe,bg+:#3e4452,hl+:#98c379,info:#61afef,prompt:#61afef,pointer:#e5c07b,marker:#e5c07b,spinner:#61afef"
fzf_colors["Base16 Eighties"]="bg+:#393939,bg:#2d2d2d,spinner:#66cccc,hl:#6699cc,fg:#a09f93,header:#6699cc,info:#ffcc66,pointer:#66cccc,marker:#66cccc,fg+:#e8e6df,prompt:#ffcc66,hl+:#6699cc"

export FZF_COLOR_SCHEME="Base16 Eighties"

FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors["$FZF_COLOR_SCHEME"]}"

# Use ripgrep or ag if available
if (( $+commands[rg] )); then
  export FZF_CTRL_T_COMMAND="rg --files"
  _fzf_compgen_path() {
    rg --files "$1"
  }
elif (( $+commands[ag] )); then
  export FZF_CTRL_T_COMMAND="ag -g ''"
  _fzf_compgen_path() {
    ag -g '' "$1"
  }
fi

if (( $+commands[tree] )); then
  export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2>/dev/null | head -200'"
  export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
fi

if [ -n "$TMUX_PANE" ]; then
  export FZF_TMUX=1
  alias fzf="fzf-tmux -d40%"
else
  export FZF_TMUX=0
  FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --height 40% --inline-info --reverse"
fi

export FZF_DEFAULT_OPTS
