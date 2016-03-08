# Updates editor information when the terminal start or keymap changes.
function zle-line-init zle-keymap-select() {
  zle reset-prompt
  zle -R
}

# Ensure that the prompt is redrawn when the terminal size changes.
TRAPWINCH() {
  zle && { zle reset-prompt; zle -R }
}

zle -N zle-keymap-select

bindkey -v
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey -M vicmd 'H' run-help
bindkey -M vicmd 'u' undo
bindkey -M vicmd 'yy' vi-yank-whole-line

INSERT_MODE="%{$fg[blue]%}i%{$reset_color%}"
NORMAL_MODE="%{$fg[green]%}n%{$reset_color%}"

function vi_mode_prompt_info() {
  echo "${${KEYMAP/vicmd/$NORMAL_MODE}/(main|viins)/$INSERT_MODE}"
}

