DISABLE_AUTO_UPDATE=true
HISTFILE=~/Documents/zsh/.histfile
HISTSIZE=100000
KEYTIMEOUT=1
SAVEHIST=100000

# Rename z to j
_Z_CMD=j

# Do not require a leading '.' in a filename to be matched explicitly
setopt globdots
# Do not enter command lines into the history list if they are duplicates of a
# previous event
setopt hist_ignore_all_dups
# Remove command lines from the history list when the first character on the
# line is a space, or when one of the expanded aliases contains a leading space
setopt hist_ignore_space
# Remove superfluous blanks from each command line being added to the history list
setopt hist_reduce_blanks
# immediately appends new commands to the histfile
setopt inc_append_history
# Share one history between zsh sessions
setopt share_history
# Shaddapayourface
unsetopt beep

export ALTERNATE_EDITOR=""
export CHEATCOLORS=true
export CLICOLOR=true
export DEFAULT_CHEAT_DIR="$DOTFILES/cheatsheets"
# export EDITOR="emacsclient -c -nw"
export EDITOR="vim"
export FZF_DEFAULT_OPTS="-e --height 25% --reverse"
export FZF_DEFAULT_SORT=100000
export ZPLUG_HOME=/usr/local/opt/zplug

LESSPIPE=`which src-hilite-lesspipe.sh`
export LESSOPEN="| ${LESSPIPE} %s"
export LESS=' -R -X '

source $ZPLUG_HOME/init.zsh

zplug "felixr/docker-zsh-completion"
zplug "hchbaw/zce.zsh"
zplug "lib/compfix", from:oh-my-zsh
zplug "lib/completion", from:oh-my-zsh
zplug "lib/key-bindings", from:oh-my-zsh
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/httpie", from:oh-my-zsh
zplug "plugins/z", from:oh-my-zsh
zplug "willghatch/zsh-saneopt"
zplug "$DOTFILES/zsh/plugins/dirpersist", from:local, use:dirpersist.zsh

zplug "djui/alias-tips", defer:2
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "$DOTFILES/zsh/plugins/vi-mode", from:local, use:"vi-mode.zsh", defer:2
zplug "$DOTFILES/zsh/themes", from:local, use:"garyblessington.zsh-theme", defer:2

zplug "$DOTFILES/zsh/plugins/editcommand", from:local, use:editcommand.zsh, defer:3
zplug "$DOTFILES/zsh/plugins/fzf", from:local, use:fzf.zsh, defer:3

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'

bindkey "^j" zce

# aliases
alias be='bundle exec'
alias bi='bundle install'
alias bu='bundle update'
alias ccat='source-highlight --failsafe --out-format=esc -o STDOUT -i'
alias emacs="$EDITOR"
alias stop-emacs-daemon="emacsclient -e \"(kill-emacs)\""
alias galc='git reset --soft HEAD^'
alias gap='git add --patch'
alias gcom='git checkout master'
alias gdc='git diff --cached'
alias gfhf='git flow hotfix finish'
alias gfhs='git flow hotfix start'
alias git='hub'
alias grhard='git reset --hard'
alias grom='git rebase origin/master'
alias gt='git tree'
alias hc='heroku config "$@"'
alias hcd='hc -r development "$@"'
alias hcp='hc -r production "$@"'
alias hcs='hc -r staging "$@"'
alias hlt='heroku logs --tail "$@"'
alias hltd='hlt -r development "$@"'
alias hltp='hlt -r production "$@"'
alias hlts='hlt -r staging "$@"'
alias hpr='hub pull-request'
alias hrb='heroku run bash "$@"'
alias hrbd='hrb -r development "$@"'
alias hrbp='hrb -r production "$@"'
alias hrbs='hrb -r staging "$@"'
alias hrc='heroku run rails console "$@"'
alias hrcd='hrc -r development "$@"'
alias hrcp='hrc -r production "$@"'
alias hrcs='hrc -r staging "$@"'
alias j=z
alias ls='ls -alh'

# source other scripts
source /usr/local/share/zsh/site-functions/_aws

# source private scripts
if [[ -d $PRIVATE_DOTFILES ]]; then
  for file in $PRIVATE_DOTFILES/zsh/*.zsh; do
    source "$file"
  done
fi
