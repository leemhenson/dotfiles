# allow <C-v> to edit the command line (standard behaviour)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^v' edit-command-line

