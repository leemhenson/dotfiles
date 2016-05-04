#!/usr/bin/env sh

rm_if_link(){ [ ! -L "$1" ] || rm -v "$1"; }

rm_if_link "$HOME/.tern-project"

