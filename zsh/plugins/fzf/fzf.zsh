# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/leemhenson/.fzf/bin* ]]; then
  export PATH="$PATH:/Users/leemhenson/.fzf/bin"
fi

# Man path
# --------
if [[ ! "$MANPATH" == */Users/leemhenson/.fzf/man* && -d "/Users/leemhenson/.fzf/man" ]]; then
  export MANPATH="$MANPATH:/Users/leemhenson/.fzf/man"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/Users/leemhenson/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/leemhenson/.fzf/shell/key-bindings.zsh"

