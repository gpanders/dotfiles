# Set up fzf for home directory installation
if [ -d "$HOME/.fzf" ]; then
    if [[ ! "$PATH" == *$HOME/.fzf/bin* ]]; then
        export PATH="$PATH:$HOME/.fzf/bin"
    fi

    # Auto-completion
    # ---------------
    [[ $- == *i* ]] && source "$HOME/.fzf/shell/completion.bash" 2> /dev/null

    # Key bindings
    # ------------
    source "$HOME/.fzf/shell/key-bindings.bash"
fi

FZF_DEFAULT_OPTS=""

# Color
declare -A fzf_colors
fzf_colors["Solarized Dark"]="dark,fg:-1,bg:-1,hl:#268bd2,fg+:#93a1a1,bg+:#073642,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["Solarized Light"]="light,fg:-1,bg:-1,hl:#268bd2,fg+:#586e75,bg+:#eee8d5,hl+:#268bd2,info:#b58900,prompt:#b58900,pointer:#2aa198,marker:#2aa198,spinner:#b58900"
fzf_colors["One Dark"]="dark,fg:-1,bg:-1,hl:#98c379,fg+:#fefefe,bg+:#3e4452,hl+:#98c379,info:#61afef,prompt:#61afef,pointer:#e5c07b,marker:#e5c07b,spinner:#61afef"
fzf_colors["Base 16"]="bg+:10,bg:0,spinner:6,hl:4,fg:12,header:4,info:3,pointer:6,marker:6,fg+:13,prompt:3,hl+:4"

export FZF_COLOR_SCHEME="Base 16"

FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --color ${fzf_colors["$FZF_COLOR_SCHEME"]}"

# Set default command
if hash fd 2>/dev/null; then
    export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"
    export FZF_ALT_C_COMMAND="fd -t d"
    _fzf_compgen_path() {
        fd --hidden --follow --exclude ".git" . "$1"
    }
    _fzf_compgen_dir() {
        fd --type d --hidden --follow --exclude ".git" . "$1"
    }
elif hash rg 2>/dev/null; then
    export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git'"
    _fzf_compgen_path() {
        rg --files --hidden --glob '!.git' "$1"
    }
elif hash ag 2>/dev/null; then
    export FZF_DEFAULT_COMMAND="ag -g ''"
    _fzf_compgen_path() {
        ag -g '' "$1"
    }
fi

if [ ! -z "$FZF_DEFAULT_COMMAND" ]; then
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
fi

export FZF_CTRL_T_OPTS="--preview '! grep -qI . {} && echo {} is a binary file || (highlight -O ansi -l {} || coderay {} || rougify {} || cat {}) 2>/dev/null | head -200'"

if hash tree 2>/dev/null; then
    export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
fi

if [ -n "$TMUX_PANE" ]; then
    export FZF_TMUX=1
    alias fzf="fzf-tmux -d40%"
else
    export FZF_TMUX=0
    export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --height 40%"
fi

FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS} --inline-info --reverse"

export FZF_DEFAULT_OPTS
