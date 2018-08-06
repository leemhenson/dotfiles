export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export GOPATH=$HOME/.go
export VIMPATH="$DOTFILES/vim"
export XDG_CONFIG_HOME="$DOTFILES"

typeset -U path

path[1,0]="$DOTFILES/zsh/bin"
path[1,0]="/usr/local/sbin"
path[1,0]="/usr/local/heroku/bin"
path[1,0]="$HOME/src/other/google-cloud-sdk/bin"
path[1,0]="/usr/local/share/npm/bin"
path[1,0]="/usr/local/opt/node/bin"
path[1,0]="/Applications/Postgres.app/Contents/Versions/10/bin"
path[1,0]="$HOME/.config/yarn/global/node_modules/.bin"
path[1,0]="$HOME/.local/bin"
path[1,0]="$GOPATH/bin"
path[1,0]="./node_modules/.bin"

git config --global core.excludesfile "$DOTFILES/git/global.gitignore"

eval "$(direnv hook zsh)"

if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh

  chruby_auto
fi
