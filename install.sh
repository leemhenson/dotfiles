#!/usr/bin/env sh

ln -snf "$DOTFILES/emacs" "$HOME/.emacs.d"
ln -sf "$DOTFILES/emacs/daemon.plist" "$HOME/Library/LaunchAgents/emacs-deamon.plist"
ln -sf "$DOTFILES/git/config" "$HOME/.gitconfig"
ln -sf "$DOTFILES/js-beautify/.jsbeautifyrc" "$HOME/.jsbeautifyrc"
ln -snf "$DOTFILES/karabiner-elements" "$HOME/.karabiner.d"
ln -sf "$DOTFILES/khdrc" "$HOME/.khdrc"
ln -snf "$DOTFILES/kwm" "$HOME/.kwm"
ln -sf "$DOTFILES/tern/.tern-project" "$HOME/.tern-project"

other="$HOME/src/other"
ghci_color="$other/ghci-color"

if [ ! -d $ghci_color ]; then
  mkdir -p $other
  git clone https://github.com/rhysd/ghci-color.git $ghci_color
  echo "${ghci_color}/ghci-color"
  ln -sf "${ghci_color}/ghci-color" /usr/local/bin/ghci-color
fi
