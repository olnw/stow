#!/bin/sh

# From the root directory of this repo, run 'stow Neovim' before executing this script.
# This is so that Neovim can read the init.vim file.
# If it can't read the init.vim file, it won't recognise the PlugInstall command,
# and this script will fail.

# Download vim-plug
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# Install vim-plug
nvim --headless +PlugInstall +qa
