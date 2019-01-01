prompt_nix_shell() {
  if [[ -n "$IN_NIX_SHELL" ]]; then
    echo -n "nix-shell "
  fi
}

PROMPT='$(vi_mode_prompt_info) %{$fg[yellow]%}$(prompt_nix_shell)%{$fg[cyan]%}%c%{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%}% %{$reset_color%} '
RPROMPT=''

ZSH_THEME_GIT_PROMPT_PREFIX="(%{$fg[blue]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[red]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
