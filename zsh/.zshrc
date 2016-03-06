# Lines configured by zsh-newuser-install
HISTFILE=~/.dotfiles/zsh/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install

export EDITOR=vim
export ENHANCD_FILTER=fzf-tmux
export KEYTIMEOUT=1

source ~/.zplug/zplug

zplug "plugins/git", from:oh-my-zsh
zplug "plugins/vi-mode", from:oh-my-zsh

zplug "junegunn/fzf-bin", \
    as:command, \
    from:gh-r, \
    file:fzf, \
    of:"*darwin*amd64*"

zplug "b4b4r07/enhancd", of:enhancd.sh
zplug "djui/alias-tips", nice:11
zplug "hchbaw/zce.zsh", of:zce.zsh
zplug "willghatch/zsh-saneopt"
zplug "zsh-users/zsh-syntax-highlighting", nice:10
zplug "zsh-users/zsh-completions", nice:11
zplug "zsh-users/zsh-history-substring-search", nice:12

zplug "$DOTFILES/zsh/themes", from:local, of:"garyblessington.zsh-theme"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load --verbose


# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# zce 
bindkey "^E" zce


# aliases
alias ls='ls -alh'
alias bu='bundle update'
alias hlt='heroku logs --tail "$@"'
alias hrb='heroku run bash "$@"'
alias hltp='hlt -r production "$@"'
alias hlts='hlt -r staging "$@"'
alias hrbp='hrb -r production "$@"'
alias hrbs='hrb -r staging "$@"'
