# Lines configured by zsh-newuser-install
HISTFILE=~/.dotfiles/zsh/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install

export CLICOLOR=true
export EDITOR=vim
export KEYTIMEOUT=1
export LSCOLORS="exfxcxdxbxegedabagacad"

source ~/.zplug/zplug

zplug "plugins/git", from:oh-my-zsh
zplug "junegunn/fzf-bin", as:command, from:gh-r, file:fzf, of:"*darwin*amd64*"

zplug "djui/alias-tips", nice:11
zplug "willghatch/zsh-saneopt"
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "zsh-users/zsh-completions", nice:11

zplug "$DOTFILES/zsh/plugins/chruby", from:local, of:chruby.zsh
zplug "$DOTFILES/zsh/plugins/dirpersist", from:local, of:dirpersist.zsh
zplug "$DOTFILES/zsh/plugins/fzf", from:local, of:fzf.zsh
zplug "$DOTFILES/zsh/plugins/editcommand", from:local, of:editcommand.zsh
zplug "$DOTFILES/zsh/themes", from:local, of:"garyblessington.zsh-theme"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load


# aliases
alias ls='ls -alh'
alias bu='bundle update'
alias hc='heroku config "$@"'
alias hcp='hc -r production "$@"'
alias hcs='hc -r staging "$@"'
alias hlt='heroku logs --tail "$@"'
alias hrb='heroku run bash "$@"'
alias hltp='hlt -r production "$@"'
alias hlts='hlt -r staging "$@"'
alias hrbp='hrb -r production "$@"'
alias hrbs='hrb -r staging "$@"'


# source private scripts
if [[ -d $PRIVATE_DOTFILES ]]; then
  for file in $PRIVATE_DOTFILES/zsh/*.zsh; do
    source "$file"
  done
fi
