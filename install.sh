#!/usr/bin/env sh

sudo launchctl config user path /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

ln -snf "$DOTFILES/emacs" "$HOME/.emacs.d"
ln -sf "$DOTFILES/emacs/daemon.plist" "$HOME/Library/LaunchAgents/emacs-deamon.plist"
ln -sf "$DOTFILES/git/config" "$HOME/.gitconfig"
ln -sf "$DOTFILES/js-beautify/.jsbeautifyrc" "$HOME/.jsbeautifyrc"
mkdir -p "$HOME/.config/pgcli"
ln -sf "$DOTFILES/pgcli/config" "$HOME/.config/pgcli/config"
ln -sf "$DOTFILES/khdrc" "$HOME/.khdrc"
ln -snf "$DOTFILES/kwm" "$HOME/.kwm"
ln -sf "$DOTFILES/tern/.tern-project" "$HOME/.tern-project"

# tools delivered through package managers

brew bundle

npm install -g \
    bower \
    flow-bin \
    nodemon \
    pulp

pip2 install --upgrade pip
pip3 install --upgrade pip

pip2 install --upgrade neovim
pip3 install --upgrade neovim

# tools built from source

other="$HOME/src/other"

mkdir -p $other

yarn="$other/yarn"

if [ ! -d $yarn ]; then
  cd $other

  mkdir -p ./yarn-temp

  cd ./yarn-temp
  wget https://yarnpkg.com/latest.tar.gz
  tar zxvf latest.tar.gz
  mv ./dist $yarn
  rm latest.tar.gz
  rm -rf ./yarn-temp
fi

ghci_color="$other/ghci-color"

if [ ! -d $ghci_color ]; then
  git clone https://github.com/rhysd/ghci-color.git $ghci_color
  ln -sf "${ghci_color}/ghci-color" /usr/local/bin/ghci-color
fi
