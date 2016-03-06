export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export GOPATH=$HOME/.go
export PATH="/Applications/Postgres.app/Contents/Versions/9.5/bin:$DOTFILES/zsh/bin:$PATH"
export VIMPATH="$DOTFILES/vim"
export XDG_CONFIG_HOME="$DOTFILES"

if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
  source /usr/local/share/chruby/auto.sh

  chruby_auto
fi
