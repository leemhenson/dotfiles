#!/usr/bin/env sh

sudo launchctl config user path /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

ln -snf "$DOTFILES/emacs" "$HOME/.emacs.d"
# ln -sf "$DOTFILES/emacs/daemon.plist" "$HOME/Library/LaunchAgents/emacs-deamon.plist"
ln -sf "$DOTFILES/git/config" "$HOME/.gitconfig"
ln -sf "$DOTFILES/js-beautify/.jsbeautifyrc" "$HOME/.jsbeautifyrc"
mkdir -p "$HOME/.config/pgcli"
ln -sf "$DOTFILES/pgcli/config" "$HOME/.config/pgcli/config"
ln -sf "$DOTFILES/khdrc" "$HOME/.khdrc"
ln -snf "$DOTFILES/kwm" "$HOME/.kwm"
ln -sf "$DOTFILES/tern/.tern-project" "$HOME/.tern-project"
ln -sf "$DOTFILES/vim/.vintrc.yaml" "$HOME/.vintrc.yaml"

# tools delivered through package managers

brew bundle

npm install -g \
    bower \
    flow-bin \
    flow-vim-quickfix \
    nodemon \
    pulp

pip2 install --upgrade pip
pip3 install --upgrade pip

# upgrade neovim and recompile dependent plugins

pip2 install --upgrade neovim
pip3 install --upgrade neovim

pip2 install --upgrade vim-vint
pip3 install --upgrade vim-vint

defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
defaults write com.microsoft.VSCodeInsiders ApplePressAndHoldEnabled -bool false

# fix ctrl-h terminfo bug

infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $HOME/$TERM.ti
tic $HOME/$TERM.ti

# tools built from source

other="$HOME/src/other"

mkdir -p $other

ghci_color="$other/ghci-color"

if [ ! -d $ghci_color ]; then
  git clone https://github.com/rhysd/ghci-color.git $ghci_color
  ln -sf "${ghci_color}/ghci-color" /usr/local/bin/ghci-color
fi

