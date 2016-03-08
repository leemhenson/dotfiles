# allow <C-e> to edit the command line (standard behaviour)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line

