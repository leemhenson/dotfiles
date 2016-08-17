#!/usr/bin/env sh

rm_if_link(){ [ ! -L "$1" ] || rm -v "$1"; }

rm_if_link "$HOME/.gitconfig "
rm_if_link "$HOME/.jsbeautifyrc"
rm_if_link "$HOME/.tern-project"
rm_if_link "$HOME/Library/LaunchAgents/emacs-deamon.plist"

