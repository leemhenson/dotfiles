# Updates editor information when the terminal start or keymap changes.
function zle-keymap-select zle-line-init
{
    # change cursor shape in iTerm2
    case $KEYMAP in
        vicmd)      print -n -- "\E]50;CursorShape=0\C-G";;  # block cursor
        viins|main) print -n -- "\E]50;CursorShape=1\C-G";;  # line cursor
    esac

    zle reset-prompt
    zle -R
}

function zle-line-finish
{
    print -n -- "\E]50;CursorShape=0\C-G"  # block cursor
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select

# Ensure that the prompt is redrawn when the terminal size changes.
TRAPWINCH() {
  zle && { zle reset-prompt; zle -R }
}

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

